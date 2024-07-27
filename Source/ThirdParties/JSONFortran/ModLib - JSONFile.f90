!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_file_module.F90"
!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  Higher-level [[json_file]] interface for the [[json_value]] type.
!
!--- License
!  * JSON-Fortran is released under a BSD-style license.
!    See the [LICENSE](https://github.com/jacobwilliams/json-fortran/blob/master/LICENSE)
!    file for details.

    MODULE ModLib_JSONFile

    USE,INTRINSIC :: ISO_FORTRAN_ENV
    USE ModLib_JSONKinds
    USE ModLib_JSONParameters, ONLY: UNIT2STR
    USE ModLib_JSONString
    USE ModLib_JSONValue

    IMPLICIT NONE

    PRIVATE

!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_macros.inc" 1
! JSON-Fortran preprocessor macros.
!
! License
!   JSON-Fortran is released under a BSD-style license.
!   See the [LICENSE](https://github.com/jacobwilliams/json-fortran/blob/master/LICENSE)
!   file for details.

!*********************************************************
! File encoding preprocessor macro.
!
!- 15

! don't ask for utf-8 file encoding unless using UCS4
! this may let us use unformatted stream io to read in files more quickly
! even with unicode support turned on `inquire( ... encoding=FL_ENCODING)`
! may be able to detect json files in which each character is exactly one
! byte


!*********************************************************

!*********************************************************
! This C preprocessor macro will take a procedure name as an
! input, and output either that same procedure name if the
! code is compiled without USE_UCS4 being defined or it will
! expand the procedure name to the original procedure name,
! followed by a comma and then the original procedure name
! with 'wrap_' prepended to it. This is suitable for creating
! overloaded interfaces that will accept UCS4 character actual
! arguments as well as DEFAULT/ASCII character arguments,
! based on whether or not ISO 10646 is supported and requested.
!
!- 55



!*********************************************************
!- 25 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_file_module.F90" 2

!*********************************************************
!> author: Jacob Williams
!  date: 12/9/2013
!
!  The `json_file` is the main public class that is
!  used to open a file and get data from it.
!
!  A `json_file` contains only two items: an instance of a [[json_core(type)]],
!  which is used for all data manipulation, and a [[json_value]] pointer,
!  which is used to construct the linked-list data structure.
!  Note that most methods in the `json_file` class are simply wrappers
!  to the lower-level routines in the [[json_value_module]].
!
!--- Example
!
!```fortran
!    program test
!    use json_module
!    implicit none
!    type(json_file) :: json
!    integer :: ival
!    real(real64) :: rval
!    character(len=:),allocatable :: cval
!    logical :: found
!    call json%initialize(compact_reals=.true.)
!    call json%load(filename='myfile.json')
!    call json%print() !print to the console
!    call json%get('var.i',ival,found)
!    call json%get('var.r(3)',rval,found)
!    call json%get('var.c',cval,found)
!    call json%destroy()
!    end program test
!```
!
!@note The `destroy()` method may be called to free the memory if necessary.
!      [[json_file(type)]] includes a finalizer that also calls
!      `destroy()` when the variable goes out of scope.

    TYPE,PUBLIC :: JSON_FILE

        PRIVATE

        TYPE(JSON_CORE)          :: CORE         !! The instance of the [[json_core(type)]]
!! factory used for this file.
        TYPE(JSON_VALUE),POINTER :: P => NULL()  !! the JSON structure read from the file

    CONTAINS

        GENERIC,PUBLIC :: INITIALIZE => INITIALIZE_JSON_CORE_IN_FILE,&
                                        SET_JSON_CORE_IN_FILE

        PROCEDURE,PUBLIC :: GET_CORE => GET_JSON_CORE_IN_FILE

!>
!  Load JSON from a file.
        PROCEDURE,PUBLIC :: LOAD => JSON_FILE_LOAD

!>
!  The same as `load`, but only here for backward compatibility
        PROCEDURE,PUBLIC :: LOAD_FILE => JSON_FILE_LOAD

!>
!  Load JSON from a string.
        GENERIC,PUBLIC :: DESERIALIZE => JSON_FILE_LOAD_FROM_STRING

!>
!  The same as `deserialize`, but only here for backward compatibility
        GENERIC,PUBLIC :: LOAD_FROM_STRING => JSON_FILE_LOAD_FROM_STRING

!>
!  Print the [[json_value]] structure to an allocatable string
        PROCEDURE,PUBLIC :: SERIALIZE => JSON_FILE_PRINT_TO_STRING

!>
!  The same as `serialize`, but only here for backward compatibility
        PROCEDURE,PUBLIC :: PRINT_TO_STRING => JSON_FILE_PRINT_TO_STRING

        PROCEDURE,PUBLIC :: DESTROY => JSON_FILE_DESTROY
        PROCEDURE,PUBLIC :: NULLIFY => JSON_FILE_NULLIFY
        PROCEDURE,PUBLIC :: MOVE    => JSON_FILE_MOVE_POINTER
        GENERIC,PUBLIC   :: INFO    => JSON_FILE_VARIABLE_INFO
        GENERIC,PUBLIC   :: MATRIX_INFO => JSON_FILE_VARIABLE_MATRIX_INFO

!error checking:
        PROCEDURE,PUBLIC :: FAILED => JSON_FILE_FAILED
        PROCEDURE,PUBLIC :: PRINT_ERROR_MESSAGE => JSON_FILE_PRINT_ERROR_MESSAGE
        PROCEDURE,PUBLIC :: CHECK_FOR_ERRORS => JSON_FILE_CHECK_FOR_ERRORS
        PROCEDURE,PUBLIC :: CLEAR_EXCEPTIONS => JSON_FILE_CLEAR_EXCEPTIONS

        GENERIC,PUBLIC :: PRINT => JSON_FILE_PRINT_TO_CONSOLE, &
                                   JSON_FILE_PRINT_TO_UNIT, &
                                   JSON_FILE_PRINT_TO_FILENAME

!>
!  The same as `print`, but only here for backward compatibility
        GENERIC,PUBLIC :: PRINT_FILE => JSON_FILE_PRINT_TO_CONSOLE, &
                                        JSON_FILE_PRINT_TO_UNIT, &
                                        JSON_FILE_PRINT_TO_FILENAME

!>
!  Rename a variable, specifying it by path
        GENERIC,PUBLIC :: RENAME => JSON_FILE_RENAME
!- 131


!>
!  Verify that a path is valid
!  (i.e., a variable with this path exists in the file).
        GENERIC,PUBLIC :: VALID_PATH => JSON_FILE_VALID_PATH

!>
!  Get a variable from a [[json_file(type)]], by specifying the path.
        GENERIC,PUBLIC :: GET => JSON_FILE_GET_OBJECT,      &
                                 JSON_FILE_GET_INTEGER,     &

                                 JSON_FILE_GET_REAL32,      &

                                 JSON_FILE_GET_REAL,        &
!- 148

                                 JSON_FILE_GET_LOGICAL,     &
                                 JSON_FILE_GET_STRING,      &
                                 JSON_FILE_GET_INTEGER_VEC, &

                                 JSON_FILE_GET_REAL32_VEC,  &

                                 JSON_FILE_GET_REAL_VEC,    &
!- 158

                                 JSON_FILE_GET_LOGICAL_VEC, &
                                 JSON_FILE_GET_STRING_VEC,  &
                                 JSON_FILE_GET_ALLOC_STRING_VEC,  &
                                 JSON_FILE_GET_ROOT

!>
!  Add a variable to a [[json_file(type)]], by specifying the path.
!
!--- Example
!
!```fortran
!  program test
!  use json_module, rk=>json_rk, ik=>json_ik
!  implicit none
!  type(json_file) :: f
!  call f%initialize()  ! specify whatever init options you want.
!  call f%add('inputs.t', 0.0_rk)
!  call f%add('inputs.x', [1.0_rk,2.0_rk,3.0_rk])
!  call f%add('inputs.flag', .true.)
!  call f%print()  ! print to the console
!  end program test
!```
        GENERIC,PUBLIC :: ADD => JSON_FILE_ADD, &
                                 JSON_FILE_ADD_OBJECT,      &
                                 JSON_FILE_ADD_INTEGER,     &

                                 JSON_FILE_ADD_REAL32,      &

                                 JSON_FILE_ADD_REAL,        &
!- 190

                                 JSON_FILE_ADD_LOGICAL,     &
                                 JSON_FILE_ADD_STRING,      &
                                 JSON_FILE_ADD_INTEGER_VEC, &

                                 JSON_FILE_ADD_REAL32_VEC,  &

                                 JSON_FILE_ADD_REAL_VEC,    &
!- 200

                                 JSON_FILE_ADD_LOGICAL_VEC, &
                                 JSON_FILE_ADD_STRING_VEC
!- 208


!>
!  Update a scalar variable in a [[json_file(type)]],
!  by specifying the path.
!
!@note These have been mostly supplanted by the `add`
!      methods, which do a similar thing (and can be used for
!      scalars and vectors, etc.)
        GENERIC,PUBLIC :: UPDATE =>  JSON_FILE_UPDATE_INTEGER,  &
                                     JSON_FILE_UPDATE_LOGICAL,  &

                                     JSON_FILE_UPDATE_REAL32,   &

                                     JSON_FILE_UPDATE_REAL,     &
!- 225

                                     JSON_FILE_UPDATE_STRING
!- 230


!>
!  Remove a variable from a [[json_file(type)]]
!  by specifying the path.
        GENERIC,PUBLIC :: REMOVE =>  JSON_FILE_REMOVE

!traverse
        PROCEDURE,PUBLIC :: TRAVERSE => JSON_FILE_TRAVERSE

! ***************************************************
! operators
! ***************************************************

        GENERIC,PUBLIC :: OPERATOR(.IN.) => JSON_FILE_VALID_PATH_OP
        PROCEDURE,PASS(ME) :: JSON_FILE_VALID_PATH_OP

        GENERIC,PUBLIC :: ASSIGNMENT(=) => ASSIGN_JSON_FILE,&
                                           ASSIGN_JSON_FILE_TO_STRING,&
                                           ASSIGN_STRING_TO_JSON_FILE
        PROCEDURE :: ASSIGN_JSON_FILE
        PROCEDURE,PASS(ME) :: ASSIGN_JSON_FILE_TO_STRING
        PROCEDURE :: ASSIGN_STRING_TO_JSON_FILE

! ***************************************************
! private routines
! ***************************************************

!load from string:
        PROCEDURE :: JSON_FILE_LOAD_FROM_STRING

!initialize
        PROCEDURE :: INITIALIZE_JSON_CORE_IN_FILE
        PROCEDURE :: SET_JSON_CORE_IN_FILE

!get info:
        PROCEDURE :: JSON_FILE_VARIABLE_INFO
        PROCEDURE :: JSON_FILE_VARIABLE_MATRIX_INFO

!rename:
        PROCEDURE :: JSON_FILE_RENAME
!- 274


!validate path:
        PROCEDURE :: JSON_FILE_VALID_PATH

!get:
        PROCEDURE :: JSON_FILE_GET_OBJECT
        PROCEDURE :: JSON_FILE_GET_INTEGER

        PROCEDURE :: JSON_FILE_GET_REAL32

        PROCEDURE :: JSON_FILE_GET_REAL
!- 288

        PROCEDURE :: JSON_FILE_GET_LOGICAL
        PROCEDURE :: JSON_FILE_GET_STRING
        PROCEDURE :: JSON_FILE_GET_INTEGER_VEC

        PROCEDURE :: JSON_FILE_GET_REAL32_VEC

        PROCEDURE :: JSON_FILE_GET_REAL_VEC
!- 298

        PROCEDURE :: JSON_FILE_GET_LOGICAL_VEC
        PROCEDURE :: JSON_FILE_GET_STRING_VEC
        PROCEDURE :: JSON_FILE_GET_ALLOC_STRING_VEC
        PROCEDURE :: JSON_FILE_GET_ROOT

!add:
        PROCEDURE :: JSON_FILE_ADD
        PROCEDURE :: JSON_FILE_ADD_OBJECT
        PROCEDURE :: JSON_FILE_ADD_INTEGER

        PROCEDURE :: JSON_FILE_ADD_REAL32

        PROCEDURE :: JSON_FILE_ADD_REAL
!- 314

        PROCEDURE :: JSON_FILE_ADD_LOGICAL
        PROCEDURE :: JSON_FILE_ADD_STRING
        PROCEDURE :: JSON_FILE_ADD_INTEGER_VEC

        PROCEDURE :: JSON_FILE_ADD_REAL32_VEC

        PROCEDURE :: JSON_FILE_ADD_REAL_VEC
!- 324

        PROCEDURE :: JSON_FILE_ADD_LOGICAL_VEC
        PROCEDURE :: JSON_FILE_ADD_STRING_VEC
!- 332


!update:
        PROCEDURE :: JSON_FILE_UPDATE_INTEGER
        PROCEDURE :: JSON_FILE_UPDATE_LOGICAL

        PROCEDURE :: JSON_FILE_UPDATE_REAL32

        PROCEDURE :: JSON_FILE_UPDATE_REAL
!- 343

        PROCEDURE :: JSON_FILE_UPDATE_STRING
!- 348


!remove:
        PROCEDURE :: JSON_FILE_REMOVE

!print:
        PROCEDURE :: JSON_FILE_PRINT_TO_CONSOLE
        PROCEDURE :: JSON_FILE_PRINT_TO_UNIT
        PROCEDURE :: JSON_FILE_PRINT_TO_FILENAME

        FINAL :: FINALIZE_JSON_FILE

    END TYPE JSON_FILE
!*********************************************************

!*********************************************************
!> author: Izaak Beekman
!  date: 07/23/2015
!
!  Structure constructor to initialize a [[json_file(type)]]
!  object with an existing [[json_value]] object or a JSON
!  string, and either the [[json_core(type)]] settings or a
!  [[json_core(type)]] instance.
!
!--- Example
!
!```fortran
! ...
! type(json_file) :: my_file
! type(json_value),pointer :: json_object
! type(json_core) :: json_core_object
! ...
! ! Construct a json_object:
! !could do this:
!   my_file = json_file(json_object)
! !or:
!   my_file = json_file(json_object,verbose=.true.)
! !or:
!   my_file = json_file('{"x": [1]}',verbose=.true.)
! !or:
!   my_file = json_file(json_object,json_core_object)
! !or:
!   my_file = json_file('{"x": [1]}',json_core_object)
!```
    INTERFACE JSON_FILE
       MODULE PROCEDURE INITIALIZE_JSON_FILE, &
                        INITIALIZE_JSON_FILE_V2, &
                        INITIALIZE_JSON_FILE_FROM_STRING, &
                        INITIALIZE_JSON_FILE_FROM_STRING_V2
    END INTERFACE
!*************************************************************************************

    CONTAINS
!*****************************************************************************************

!*****************************************************************************************
!>
!  Finalizer for [[json_file]] class.
!
!  Just a wrapper for [[json_file_destroy]].

    SUBROUTINE FINALIZE_JSON_FILE(ME)

    IMPLICIT NONE

    TYPE(JSON_FILE),INTENT(INOUT) :: ME

    CALL ME%DESTROY(DESTROY_CORE=.TRUE.)

    END SUBROUTINE FINALIZE_JSON_FILE
!*****************************************************************************************

!*****************************************************************************************
!>
!  Check error status in the file.

    PURE FUNCTION JSON_FILE_FAILED(ME) RESULT(FAILED)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(IN) :: ME
    LOGICAL(LK)                 :: FAILED  !! will be true if there has been an error.

    FAILED = ME%CORE%FAILED()

    END FUNCTION JSON_FILE_FAILED
!*****************************************************************************************

!*****************************************************************************************
!>
!  Retrieve error status and message from the class.

    SUBROUTINE JSON_FILE_CHECK_FOR_ERRORS(ME,STATUS_OK,ERROR_MSG)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT) :: ME
    LOGICAL(LK),INTENT(OUT),OPTIONAL :: STATUS_OK !! true if there were no errors
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT),OPTIONAL :: ERROR_MSG !! the error message
!! (if there were errors)

!- 453

    CALL ME%CORE%CHECK_FOR_ERRORS(STATUS_OK,ERROR_MSG)


    END SUBROUTINE JSON_FILE_CHECK_FOR_ERRORS
!*****************************************************************************************

!*****************************************************************************************
!>
!  Clear exceptions in the class.

    PURE SUBROUTINE JSON_FILE_CLEAR_EXCEPTIONS(ME)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT) :: ME

    CALL ME%CORE%CLEAR_EXCEPTIONS()

    END SUBROUTINE JSON_FILE_CLEAR_EXCEPTIONS
!*****************************************************************************************

!*****************************************************************************************
!>
!  This is a wrapper for [[json_print_error_message]].

    SUBROUTINE JSON_FILE_PRINT_ERROR_MESSAGE(ME,IO_UNIT)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT) :: ME
    INTEGER, INTENT(IN), OPTIONAL  :: IO_UNIT

    CALL ME%CORE%PRINT_ERROR_MESSAGE(IO_UNIT)

    END SUBROUTINE JSON_FILE_PRINT_ERROR_MESSAGE
!*****************************************************************************************

!*****************************************************************************************
!>
!  Initialize the [[json_core(type)]] for this [[json_file]].
!  This is just a wrapper for [[json_initialize]].
!
!@note This does not destroy the data in the file.
!
!@note [[initialize_json_core]], [[json_initialize]],
!      [[initialize_json_core_in_file]], [[initialize_json_file]],
!      [[initialize_json_file_v2]], [[initialize_json_file_from_string]],
!      and [[initialize_json_file_from_string_v2]]
!      all have a similar interface.

    SUBROUTINE INITIALIZE_JSON_CORE_IN_FILE(ME,&
!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_initialize_dummy_arguments.inc" 1
!  The dummy argument list for the various `initialize` subroutines.
!
!  See also: json_initialize_argument.inc

VERBOSE,&
COMPACT_REALS,&
PRINT_SIGNS,&
REAL_FORMAT,&
SPACES_PER_TAB,&
STRICT_TYPE_CHECKING,&
TRAILING_SPACES_SIGNIFICANT,&
CASE_SENSITIVE_KEYS,&
NO_WHITESPACE,&
UNESCAPE_STRINGS,&
COMMENT_CHAR,&
PATH_MODE,&
PATH_SEPARATOR,&
COMPRESS_VECTORS,&
ALLOW_DUPLICATE_KEYS,&
ESCAPE_SOLIDUS,&
STOP_ON_ERROR,&
NULL_TO_REAL_MODE,&
NON_NORMAL_MODE,&
USE_QUIET_NAN, &
STRICT_INTEGER_TYPE_CHECKING &
!- 506 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_file_module.F90" 2
                                           )

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT) :: ME
!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_initialize_arguments.inc" 1
!  The argument list for the various `initialize` subroutines.
!
!  See also: json_initialize_dummy_arguments.inc

LOGICAL(LK),INTENT(IN),OPTIONAL :: VERBOSE
!! mainly useful for debugging (default is false)
LOGICAL(LK),INTENT(IN),OPTIONAL :: COMPACT_REALS
!! to compact the real number strings for output (default is true)
LOGICAL(LK),INTENT(IN),OPTIONAL :: PRINT_SIGNS
!! always print numeric sign (default is false)
CHARACTER(KIND=CDK,LEN=*),INTENT(IN),OPTIONAL :: REAL_FORMAT
!! Real number format: 'E' [default], '*', 'G', 'EN', or 'ES'
INTEGER(IK),INTENT(IN),OPTIONAL :: SPACES_PER_TAB
!! number of spaces per tab for indenting (default is 2)
LOGICAL(LK),INTENT(IN),OPTIONAL :: STRICT_TYPE_CHECKING
!! if true, no integer, double, or logical type
!! conversions are done for the `get` routines
!! (default is false).
LOGICAL(LK),INTENT(IN),OPTIONAL :: TRAILING_SPACES_SIGNIFICANT
!! for name and path comparisons, is trailing
!! space to be considered significant.
!! (default is false)
LOGICAL(LK),INTENT(IN),OPTIONAL :: CASE_SENSITIVE_KEYS
!! for name and path comparisons, are they
!! case sensitive. (default is true)
LOGICAL(LK),INTENT(IN),OPTIONAL :: NO_WHITESPACE
!! if true, printing the JSON structure is
!! done without adding any non-significant
!! spaces or linebreaks (default is false)
LOGICAL(LK),INTENT(IN),OPTIONAL :: UNESCAPE_STRINGS
!! If false, then the raw escaped
!! string is returned from [[json_get_string]]
!! and similar routines. If true [default],
!! then the string is returned unescaped.
CHARACTER(KIND=CK,LEN=*),INTENT(IN),OPTIONAL :: COMMENT_CHAR
!! If present, these characters are used
!! to denote comments in the JSON file,
!! which will be ignored if present.
!! Example: `!`, `#`, or `/!#`. Setting this
!! to a blank string disables the
!! ignoring of comments. (Default is `/!#`).
INTEGER(IK),INTENT(IN),OPTIONAL :: PATH_MODE
!! How the path strings are interpreted in the
!! `get_by_path` routines:
!!
!! * 1 : Default mode (see [[json_get_by_path_default]])
!! * 2 : as RFC 6901 "JSON Pointer" paths
!!   (see [[json_get_by_path_rfc6901]])
!! * 3 : JSONPath "bracket-notation"
!!   see [[json_get_by_path_jsonpath_bracket]])
CHARACTER(KIND=CK,LEN=1),INTENT(IN),OPTIONAL :: PATH_SEPARATOR
!! The `path` separator to use
!! in the "default" mode for
!! the paths in the various
!! `get_by_path` routines.
!! Example: `.` [default] or `%`.
!! Note: if `path_mode/=1`
!! then this is ignored.
LOGICAL(LK),INTENT(IN),OPTIONAL :: COMPRESS_VECTORS
!! If true, then arrays of integers,
!! nulls, doubles, and logicals are
!! printed all on one line.
!! [Note: `no_whitespace` will
!! override this option if necessary].
!! (Default is False).
LOGICAL(LK),INTENT(IN),OPTIONAL :: ALLOW_DUPLICATE_KEYS
!! * If True [default] then no special checks
!!   are done to check for duplicate keys.
!! * If False, then after parsing, if any duplicate
!!   keys are found, an error is thrown. A call to
!!   [[json_value_validate]] will also check for
!!   duplicates.
LOGICAL(LK),INTENT(IN),OPTIONAL :: ESCAPE_SOLIDUS
!! * If True then the solidus "`/`" is always escaped
!!   "`\/`" when serializing JSON
!! * If False [default], then it is not escaped.
!!
!! Note that this option does not affect parsing
!! (both escaped and unescaped are still valid in
!! all cases).
LOGICAL(LK),INTENT(IN),OPTIONAL :: STOP_ON_ERROR
!! If an exception is raised, then immediately quit.
!! (Default is False).
INTEGER(IK),INTENT(IN),OPTIONAL :: NULL_TO_REAL_MODE
!! if `strict_type_checking=false`:
!!
!! * 1 : an exception will be raised if
!!   try to retrieve a `null` as a real.
!! * 2 : a `null` retrieved as a real
!!   will return a NaN. [default]
!! * 3 : a `null` retrieved as a real
!!   will return 0.0.
INTEGER(IK),INTENT(IN),OPTIONAL :: NON_NORMAL_MODE
!! How to serialize NaN, Infinity, and
!! -Infinity real values:
!!
!! * 1 : as strings (e.g., "NaN",
!!   "Infinity", "-Infinity") [default]
!! * 2 : as JSON `null` values
LOGICAL(LK),INTENT(IN),OPTIONAL :: USE_QUIET_NAN
!! * If true [default], `null_to_real_mode=2`
!!   and [[string_to_real]] will use
!!   `ieee_quiet_nan` for NaN values.
!! * If false,
!!   `ieee_signaling_nan` will be used.
LOGICAL(LK),INTENT(IN),OPTIONAL :: STRICT_INTEGER_TYPE_CHECKING
!! * If false, when parsing JSON, if an integer numeric value
!!   cannot be converted to an integer (`integer(IK)`),
!!   then an attempt is then make to convert it
!!   to a real (`real(RK)`).
!! * If true, an exception will be raised if the integer
!!   value cannot be read.
!!
!! (default is true)
!- 512 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_file_module.F90" 2

    CALL ME%CORE%INITIALIZE(&
!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_initialize_dummy_arguments.inc" 1
!  The dummy argument list for the various `initialize` subroutines.
!
!  See also: json_initialize_argument.inc

VERBOSE,&
COMPACT_REALS,&
PRINT_SIGNS,&
REAL_FORMAT,&
SPACES_PER_TAB,&
STRICT_TYPE_CHECKING,&
TRAILING_SPACES_SIGNIFICANT,&
CASE_SENSITIVE_KEYS,&
NO_WHITESPACE,&
UNESCAPE_STRINGS,&
COMMENT_CHAR,&
PATH_MODE,&
PATH_SEPARATOR,&
COMPRESS_VECTORS,&
ALLOW_DUPLICATE_KEYS,&
ESCAPE_SOLIDUS,&
STOP_ON_ERROR,&
NULL_TO_REAL_MODE,&
NON_NORMAL_MODE,&
USE_QUIET_NAN, &
STRICT_INTEGER_TYPE_CHECKING &
!- 515 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_file_module.F90" 2
                           )
    END SUBROUTINE INITIALIZE_JSON_CORE_IN_FILE
!*****************************************************************************************

!*****************************************************************************************
!>
!  Set the [[json_core(type)]] for this [[json_file]].
!
!@note This does not destroy the data in the file.
!
!@note This one is used if you want to initialize the file with
!       an already-existing [[json_core(type)]] (presumably, this was already
!       initialized by a call to [[initialize_json_core]] or similar).

    SUBROUTINE SET_JSON_CORE_IN_FILE(ME,CORE)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT) :: ME
    TYPE(JSON_CORE),INTENT(IN)     :: CORE

    ME%CORE = CORE

    END SUBROUTINE SET_JSON_CORE_IN_FILE
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a copy of the [[json_core(type)]] in this [[json_file]].

    SUBROUTINE GET_JSON_CORE_IN_FILE(ME,CORE)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(IN) :: ME
    TYPE(JSON_CORE),INTENT(OUT) :: CORE

    CORE = ME%CORE

    END SUBROUTINE GET_JSON_CORE_IN_FILE
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!  date: 07/23/2015
!
!  Cast a [[json_value]] object as a [[json_file(type)]] object.
!  It also calls the `initialize()` method.
!
!@note [[initialize_json_core]], [[json_initialize]],
!      [[initialize_json_core_in_file]], [[initialize_json_file]],
!      [[initialize_json_file_v2]], [[initialize_json_file_from_string]],
!      and [[initialize_json_file_from_string_v2]]
!      all have a similar interface.

    FUNCTION INITIALIZE_JSON_FILE(P,&
!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_initialize_dummy_arguments.inc" 1
!  The dummy argument list for the various `initialize` subroutines.
!
!  See also: json_initialize_argument.inc

VERBOSE,&
COMPACT_REALS,&
PRINT_SIGNS,&
REAL_FORMAT,&
SPACES_PER_TAB,&
STRICT_TYPE_CHECKING,&
TRAILING_SPACES_SIGNIFICANT,&
CASE_SENSITIVE_KEYS,&
NO_WHITESPACE,&
UNESCAPE_STRINGS,&
COMMENT_CHAR,&
PATH_MODE,&
PATH_SEPARATOR,&
COMPRESS_VECTORS,&
ALLOW_DUPLICATE_KEYS,&
ESCAPE_SOLIDUS,&
STOP_ON_ERROR,&
NULL_TO_REAL_MODE,&
NON_NORMAL_MODE,&
USE_QUIET_NAN, &
STRICT_INTEGER_TYPE_CHECKING &
!- 572 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_file_module.F90" 2
                                 ) RESULT(FILE_OBJECT)

    IMPLICIT NONE

    TYPE(JSON_FILE) :: FILE_OBJECT
    TYPE(JSON_VALUE),POINTER,OPTIONAL :: P  !! `json_value` object to cast
!! as a `json_file` object. This
!! will be nullified.
!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_initialize_arguments.inc" 1
!  The argument list for the various `initialize` subroutines.
!
!  See also: json_initialize_dummy_arguments.inc

LOGICAL(LK),INTENT(IN),OPTIONAL :: VERBOSE
!! mainly useful for debugging (default is false)
LOGICAL(LK),INTENT(IN),OPTIONAL :: COMPACT_REALS
!! to compact the real number strings for output (default is true)
LOGICAL(LK),INTENT(IN),OPTIONAL :: PRINT_SIGNS
!! always print numeric sign (default is false)
CHARACTER(KIND=CDK,LEN=*),INTENT(IN),OPTIONAL :: REAL_FORMAT
!! Real number format: 'E' [default], '*', 'G', 'EN', or 'ES'
INTEGER(IK),INTENT(IN),OPTIONAL :: SPACES_PER_TAB
!! number of spaces per tab for indenting (default is 2)
LOGICAL(LK),INTENT(IN),OPTIONAL :: STRICT_TYPE_CHECKING
!! if true, no integer, double, or logical type
!! conversions are done for the `get` routines
!! (default is false).
LOGICAL(LK),INTENT(IN),OPTIONAL :: TRAILING_SPACES_SIGNIFICANT
!! for name and path comparisons, is trailing
!! space to be considered significant.
!! (default is false)
LOGICAL(LK),INTENT(IN),OPTIONAL :: CASE_SENSITIVE_KEYS
!! for name and path comparisons, are they
!! case sensitive. (default is true)
LOGICAL(LK),INTENT(IN),OPTIONAL :: NO_WHITESPACE
!! if true, printing the JSON structure is
!! done without adding any non-significant
!! spaces or linebreaks (default is false)
LOGICAL(LK),INTENT(IN),OPTIONAL :: UNESCAPE_STRINGS
!! If false, then the raw escaped
!! string is returned from [[json_get_string]]
!! and similar routines. If true [default],
!! then the string is returned unescaped.
CHARACTER(KIND=CK,LEN=*),INTENT(IN),OPTIONAL :: COMMENT_CHAR
!! If present, these characters are used
!! to denote comments in the JSON file,
!! which will be ignored if present.
!! Example: `!`, `#`, or `/!#`. Setting this
!! to a blank string disables the
!! ignoring of comments. (Default is `/!#`).
INTEGER(IK),INTENT(IN),OPTIONAL :: PATH_MODE
!! How the path strings are interpreted in the
!! `get_by_path` routines:
!!
!! * 1 : Default mode (see [[json_get_by_path_default]])
!! * 2 : as RFC 6901 "JSON Pointer" paths
!!   (see [[json_get_by_path_rfc6901]])
!! * 3 : JSONPath "bracket-notation"
!!   see [[json_get_by_path_jsonpath_bracket]])
CHARACTER(KIND=CK,LEN=1),INTENT(IN),OPTIONAL :: PATH_SEPARATOR
!! The `path` separator to use
!! in the "default" mode for
!! the paths in the various
!! `get_by_path` routines.
!! Example: `.` [default] or `%`.
!! Note: if `path_mode/=1`
!! then this is ignored.
LOGICAL(LK),INTENT(IN),OPTIONAL :: COMPRESS_VECTORS
!! If true, then arrays of integers,
!! nulls, doubles, and logicals are
!! printed all on one line.
!! [Note: `no_whitespace` will
!! override this option if necessary].
!! (Default is False).
LOGICAL(LK),INTENT(IN),OPTIONAL :: ALLOW_DUPLICATE_KEYS
!! * If True [default] then no special checks
!!   are done to check for duplicate keys.
!! * If False, then after parsing, if any duplicate
!!   keys are found, an error is thrown. A call to
!!   [[json_value_validate]] will also check for
!!   duplicates.
LOGICAL(LK),INTENT(IN),OPTIONAL :: ESCAPE_SOLIDUS
!! * If True then the solidus "`/`" is always escaped
!!   "`\/`" when serializing JSON
!! * If False [default], then it is not escaped.
!!
!! Note that this option does not affect parsing
!! (both escaped and unescaped are still valid in
!! all cases).
LOGICAL(LK),INTENT(IN),OPTIONAL :: STOP_ON_ERROR
!! If an exception is raised, then immediately quit.
!! (Default is False).
INTEGER(IK),INTENT(IN),OPTIONAL :: NULL_TO_REAL_MODE
!! if `strict_type_checking=false`:
!!
!! * 1 : an exception will be raised if
!!   try to retrieve a `null` as a real.
!! * 2 : a `null` retrieved as a real
!!   will return a NaN. [default]
!! * 3 : a `null` retrieved as a real
!!   will return 0.0.
INTEGER(IK),INTENT(IN),OPTIONAL :: NON_NORMAL_MODE
!! How to serialize NaN, Infinity, and
!! -Infinity real values:
!!
!! * 1 : as strings (e.g., "NaN",
!!   "Infinity", "-Infinity") [default]
!! * 2 : as JSON `null` values
LOGICAL(LK),INTENT(IN),OPTIONAL :: USE_QUIET_NAN
!! * If true [default], `null_to_real_mode=2`
!!   and [[string_to_real]] will use
!!   `ieee_quiet_nan` for NaN values.
!! * If false,
!!   `ieee_signaling_nan` will be used.
LOGICAL(LK),INTENT(IN),OPTIONAL :: STRICT_INTEGER_TYPE_CHECKING
!! * If false, when parsing JSON, if an integer numeric value
!!   cannot be converted to an integer (`integer(IK)`),
!!   then an attempt is then make to convert it
!!   to a real (`real(RK)`).
!! * If true, an exception will be raised if the integer
!!   value cannot be read.
!!
!! (default is true)
!- 581 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_file_module.F90" 2

    CALL FILE_OBJECT%INITIALIZE(&
!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_initialize_dummy_arguments.inc" 1
!  The dummy argument list for the various `initialize` subroutines.
!
!  See also: json_initialize_argument.inc

VERBOSE,&
COMPACT_REALS,&
PRINT_SIGNS,&
REAL_FORMAT,&
SPACES_PER_TAB,&
STRICT_TYPE_CHECKING,&
TRAILING_SPACES_SIGNIFICANT,&
CASE_SENSITIVE_KEYS,&
NO_WHITESPACE,&
UNESCAPE_STRINGS,&
COMMENT_CHAR,&
PATH_MODE,&
PATH_SEPARATOR,&
COMPRESS_VECTORS,&
ALLOW_DUPLICATE_KEYS,&
ESCAPE_SOLIDUS,&
STOP_ON_ERROR,&
NULL_TO_REAL_MODE,&
NON_NORMAL_MODE,&
USE_QUIET_NAN, &
STRICT_INTEGER_TYPE_CHECKING &
!- 584 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_file_module.F90" 2
                               )

    IF (PRESENT(P)) THEN
        FILE_OBJECT%P => P
! we have to nullify it to avoid
! a dangling pointer when the file
! goes out of scope
        NULLIFY(P)
    END IF

    END FUNCTION INITIALIZE_JSON_FILE
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/26/2016
!
!  Cast a [[json_value]] pointer and a [[json_core(type)]] object
!  as a [[json_file(type)]] object.

    FUNCTION INITIALIZE_JSON_FILE_V2(JSON_VALUE_OBJECT,JSON_CORE_OBJECT) &
                                        RESULT(FILE_OBJECT)

    IMPLICIT NONE

    TYPE(JSON_FILE)                     :: FILE_OBJECT
    TYPE(JSON_VALUE),POINTER,INTENT(IN) :: JSON_VALUE_OBJECT
    TYPE(JSON_CORE),INTENT(IN)          :: JSON_CORE_OBJECT

    FILE_OBJECT%P    => JSON_VALUE_OBJECT
    FILE_OBJECT%CORE = JSON_CORE_OBJECT

    END FUNCTION INITIALIZE_JSON_FILE_V2
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 01/19/2019
!
!  Cast a JSON string as a [[json_file(type)]] object.
!  It also calls the `initialize()` method.
!
!--- Example
!
!```fortran
!  type(json_file) :: f
!  f = json_file('{"key ": 1}', trailing_spaces_significant=.true.)
!```
!
!@note [[initialize_json_core]], [[json_initialize]],
!      [[initialize_json_core_in_file]], [[initialize_json_file]],
!      [[initialize_json_file_v2]], [[initialize_json_file_from_string]],
!      and [[initialize_json_file_from_string_v2]]
!      all have a similar interface.

    FUNCTION INITIALIZE_JSON_FILE_FROM_STRING(STR,&
!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_initialize_dummy_arguments.inc" 1
!  The dummy argument list for the various `initialize` subroutines.
!
!  See also: json_initialize_argument.inc

VERBOSE,&
COMPACT_REALS,&
PRINT_SIGNS,&
REAL_FORMAT,&
SPACES_PER_TAB,&
STRICT_TYPE_CHECKING,&
TRAILING_SPACES_SIGNIFICANT,&
CASE_SENSITIVE_KEYS,&
NO_WHITESPACE,&
UNESCAPE_STRINGS,&
COMMENT_CHAR,&
PATH_MODE,&
PATH_SEPARATOR,&
COMPRESS_VECTORS,&
ALLOW_DUPLICATE_KEYS,&
ESCAPE_SOLIDUS,&
STOP_ON_ERROR,&
NULL_TO_REAL_MODE,&
NON_NORMAL_MODE,&
USE_QUIET_NAN, &
STRICT_INTEGER_TYPE_CHECKING &
!- 641 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_file_module.F90" 2
                                             ) RESULT(FILE_OBJECT)

    IMPLICIT NONE

    TYPE(JSON_FILE) :: FILE_OBJECT
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: STR  !! string to load JSON data from
!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_initialize_arguments.inc" 1
!  The argument list for the various `initialize` subroutines.
!
!  See also: json_initialize_dummy_arguments.inc

LOGICAL(LK),INTENT(IN),OPTIONAL :: VERBOSE
!! mainly useful for debugging (default is false)
LOGICAL(LK),INTENT(IN),OPTIONAL :: COMPACT_REALS
!! to compact the real number strings for output (default is true)
LOGICAL(LK),INTENT(IN),OPTIONAL :: PRINT_SIGNS
!! always print numeric sign (default is false)
CHARACTER(KIND=CDK,LEN=*),INTENT(IN),OPTIONAL :: REAL_FORMAT
!! Real number format: 'E' [default], '*', 'G', 'EN', or 'ES'
INTEGER(IK),INTENT(IN),OPTIONAL :: SPACES_PER_TAB
!! number of spaces per tab for indenting (default is 2)
LOGICAL(LK),INTENT(IN),OPTIONAL :: STRICT_TYPE_CHECKING
!! if true, no integer, double, or logical type
!! conversions are done for the `get` routines
!! (default is false).
LOGICAL(LK),INTENT(IN),OPTIONAL :: TRAILING_SPACES_SIGNIFICANT
!! for name and path comparisons, is trailing
!! space to be considered significant.
!! (default is false)
LOGICAL(LK),INTENT(IN),OPTIONAL :: CASE_SENSITIVE_KEYS
!! for name and path comparisons, are they
!! case sensitive. (default is true)
LOGICAL(LK),INTENT(IN),OPTIONAL :: NO_WHITESPACE
!! if true, printing the JSON structure is
!! done without adding any non-significant
!! spaces or linebreaks (default is false)
LOGICAL(LK),INTENT(IN),OPTIONAL :: UNESCAPE_STRINGS
!! If false, then the raw escaped
!! string is returned from [[json_get_string]]
!! and similar routines. If true [default],
!! then the string is returned unescaped.
CHARACTER(KIND=CK,LEN=*),INTENT(IN),OPTIONAL :: COMMENT_CHAR
!! If present, these characters are used
!! to denote comments in the JSON file,
!! which will be ignored if present.
!! Example: `!`, `#`, or `/!#`. Setting this
!! to a blank string disables the
!! ignoring of comments. (Default is `/!#`).
INTEGER(IK),INTENT(IN),OPTIONAL :: PATH_MODE
!! How the path strings are interpreted in the
!! `get_by_path` routines:
!!
!! * 1 : Default mode (see [[json_get_by_path_default]])
!! * 2 : as RFC 6901 "JSON Pointer" paths
!!   (see [[json_get_by_path_rfc6901]])
!! * 3 : JSONPath "bracket-notation"
!!   see [[json_get_by_path_jsonpath_bracket]])
CHARACTER(KIND=CK,LEN=1),INTENT(IN),OPTIONAL :: PATH_SEPARATOR
!! The `path` separator to use
!! in the "default" mode for
!! the paths in the various
!! `get_by_path` routines.
!! Example: `.` [default] or `%`.
!! Note: if `path_mode/=1`
!! then this is ignored.
LOGICAL(LK),INTENT(IN),OPTIONAL :: COMPRESS_VECTORS
!! If true, then arrays of integers,
!! nulls, doubles, and logicals are
!! printed all on one line.
!! [Note: `no_whitespace` will
!! override this option if necessary].
!! (Default is False).
LOGICAL(LK),INTENT(IN),OPTIONAL :: ALLOW_DUPLICATE_KEYS
!! * If True [default] then no special checks
!!   are done to check for duplicate keys.
!! * If False, then after parsing, if any duplicate
!!   keys are found, an error is thrown. A call to
!!   [[json_value_validate]] will also check for
!!   duplicates.
LOGICAL(LK),INTENT(IN),OPTIONAL :: ESCAPE_SOLIDUS
!! * If True then the solidus "`/`" is always escaped
!!   "`\/`" when serializing JSON
!! * If False [default], then it is not escaped.
!!
!! Note that this option does not affect parsing
!! (both escaped and unescaped are still valid in
!! all cases).
LOGICAL(LK),INTENT(IN),OPTIONAL :: STOP_ON_ERROR
!! If an exception is raised, then immediately quit.
!! (Default is False).
INTEGER(IK),INTENT(IN),OPTIONAL :: NULL_TO_REAL_MODE
!! if `strict_type_checking=false`:
!!
!! * 1 : an exception will be raised if
!!   try to retrieve a `null` as a real.
!! * 2 : a `null` retrieved as a real
!!   will return a NaN. [default]
!! * 3 : a `null` retrieved as a real
!!   will return 0.0.
INTEGER(IK),INTENT(IN),OPTIONAL :: NON_NORMAL_MODE
!! How to serialize NaN, Infinity, and
!! -Infinity real values:
!!
!! * 1 : as strings (e.g., "NaN",
!!   "Infinity", "-Infinity") [default]
!! * 2 : as JSON `null` values
LOGICAL(LK),INTENT(IN),OPTIONAL :: USE_QUIET_NAN
!! * If true [default], `null_to_real_mode=2`
!!   and [[string_to_real]] will use
!!   `ieee_quiet_nan` for NaN values.
!! * If false,
!!   `ieee_signaling_nan` will be used.
LOGICAL(LK),INTENT(IN),OPTIONAL :: STRICT_INTEGER_TYPE_CHECKING
!! * If false, when parsing JSON, if an integer numeric value
!!   cannot be converted to an integer (`integer(IK)`),
!!   then an attempt is then make to convert it
!!   to a real (`real(RK)`).
!! * If true, an exception will be raised if the integer
!!   value cannot be read.
!!
!! (default is true)
!- 648 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_file_module.F90" 2

    CALL FILE_OBJECT%INITIALIZE(&
!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_initialize_dummy_arguments.inc" 1
!  The dummy argument list for the various `initialize` subroutines.
!
!  See also: json_initialize_argument.inc

VERBOSE,&
COMPACT_REALS,&
PRINT_SIGNS,&
REAL_FORMAT,&
SPACES_PER_TAB,&
STRICT_TYPE_CHECKING,&
TRAILING_SPACES_SIGNIFICANT,&
CASE_SENSITIVE_KEYS,&
NO_WHITESPACE,&
UNESCAPE_STRINGS,&
COMMENT_CHAR,&
PATH_MODE,&
PATH_SEPARATOR,&
COMPRESS_VECTORS,&
ALLOW_DUPLICATE_KEYS,&
ESCAPE_SOLIDUS,&
STOP_ON_ERROR,&
NULL_TO_REAL_MODE,&
NON_NORMAL_MODE,&
USE_QUIET_NAN, &
STRICT_INTEGER_TYPE_CHECKING &
!- 651 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_file_module.F90" 2
                               )
    CALL FILE_OBJECT%DESERIALIZE(STR)

    END FUNCTION INITIALIZE_JSON_FILE_FROM_STRING
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[initialize_json_file_from_string]], where "str" is kind=CDK.

    FUNCTION WRAP_INITIALIZE_JSON_FILE_FROM_STRING(STR,&
!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_initialize_dummy_arguments.inc" 1
!  The dummy argument list for the various `initialize` subroutines.
!
!  See also: json_initialize_argument.inc

VERBOSE,&
COMPACT_REALS,&
PRINT_SIGNS,&
REAL_FORMAT,&
SPACES_PER_TAB,&
STRICT_TYPE_CHECKING,&
TRAILING_SPACES_SIGNIFICANT,&
CASE_SENSITIVE_KEYS,&
NO_WHITESPACE,&
UNESCAPE_STRINGS,&
COMMENT_CHAR,&
PATH_MODE,&
PATH_SEPARATOR,&
COMPRESS_VECTORS,&
ALLOW_DUPLICATE_KEYS,&
ESCAPE_SOLIDUS,&
STOP_ON_ERROR,&
NULL_TO_REAL_MODE,&
NON_NORMAL_MODE,&
USE_QUIET_NAN, &
STRICT_INTEGER_TYPE_CHECKING &
!- 663 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_file_module.F90" 2
                                                  ) RESULT(FILE_OBJECT)

    IMPLICIT NONE

    TYPE(JSON_FILE) :: FILE_OBJECT
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: STR  !! string to load JSON data from
!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_initialize_arguments.inc" 1
!  The argument list for the various `initialize` subroutines.
!
!  See also: json_initialize_dummy_arguments.inc

LOGICAL(LK),INTENT(IN),OPTIONAL :: VERBOSE
!! mainly useful for debugging (default is false)
LOGICAL(LK),INTENT(IN),OPTIONAL :: COMPACT_REALS
!! to compact the real number strings for output (default is true)
LOGICAL(LK),INTENT(IN),OPTIONAL :: PRINT_SIGNS
!! always print numeric sign (default is false)
CHARACTER(KIND=CDK,LEN=*),INTENT(IN),OPTIONAL :: REAL_FORMAT
!! Real number format: 'E' [default], '*', 'G', 'EN', or 'ES'
INTEGER(IK),INTENT(IN),OPTIONAL :: SPACES_PER_TAB
!! number of spaces per tab for indenting (default is 2)
LOGICAL(LK),INTENT(IN),OPTIONAL :: STRICT_TYPE_CHECKING
!! if true, no integer, double, or logical type
!! conversions are done for the `get` routines
!! (default is false).
LOGICAL(LK),INTENT(IN),OPTIONAL :: TRAILING_SPACES_SIGNIFICANT
!! for name and path comparisons, is trailing
!! space to be considered significant.
!! (default is false)
LOGICAL(LK),INTENT(IN),OPTIONAL :: CASE_SENSITIVE_KEYS
!! for name and path comparisons, are they
!! case sensitive. (default is true)
LOGICAL(LK),INTENT(IN),OPTIONAL :: NO_WHITESPACE
!! if true, printing the JSON structure is
!! done without adding any non-significant
!! spaces or linebreaks (default is false)
LOGICAL(LK),INTENT(IN),OPTIONAL :: UNESCAPE_STRINGS
!! If false, then the raw escaped
!! string is returned from [[json_get_string]]
!! and similar routines. If true [default],
!! then the string is returned unescaped.
CHARACTER(KIND=CK,LEN=*),INTENT(IN),OPTIONAL :: COMMENT_CHAR
!! If present, these characters are used
!! to denote comments in the JSON file,
!! which will be ignored if present.
!! Example: `!`, `#`, or `/!#`. Setting this
!! to a blank string disables the
!! ignoring of comments. (Default is `/!#`).
INTEGER(IK),INTENT(IN),OPTIONAL :: PATH_MODE
!! How the path strings are interpreted in the
!! `get_by_path` routines:
!!
!! * 1 : Default mode (see [[json_get_by_path_default]])
!! * 2 : as RFC 6901 "JSON Pointer" paths
!!   (see [[json_get_by_path_rfc6901]])
!! * 3 : JSONPath "bracket-notation"
!!   see [[json_get_by_path_jsonpath_bracket]])
CHARACTER(KIND=CK,LEN=1),INTENT(IN),OPTIONAL :: PATH_SEPARATOR
!! The `path` separator to use
!! in the "default" mode for
!! the paths in the various
!! `get_by_path` routines.
!! Example: `.` [default] or `%`.
!! Note: if `path_mode/=1`
!! then this is ignored.
LOGICAL(LK),INTENT(IN),OPTIONAL :: COMPRESS_VECTORS
!! If true, then arrays of integers,
!! nulls, doubles, and logicals are
!! printed all on one line.
!! [Note: `no_whitespace` will
!! override this option if necessary].
!! (Default is False).
LOGICAL(LK),INTENT(IN),OPTIONAL :: ALLOW_DUPLICATE_KEYS
!! * If True [default] then no special checks
!!   are done to check for duplicate keys.
!! * If False, then after parsing, if any duplicate
!!   keys are found, an error is thrown. A call to
!!   [[json_value_validate]] will also check for
!!   duplicates.
LOGICAL(LK),INTENT(IN),OPTIONAL :: ESCAPE_SOLIDUS
!! * If True then the solidus "`/`" is always escaped
!!   "`\/`" when serializing JSON
!! * If False [default], then it is not escaped.
!!
!! Note that this option does not affect parsing
!! (both escaped and unescaped are still valid in
!! all cases).
LOGICAL(LK),INTENT(IN),OPTIONAL :: STOP_ON_ERROR
!! If an exception is raised, then immediately quit.
!! (Default is False).
INTEGER(IK),INTENT(IN),OPTIONAL :: NULL_TO_REAL_MODE
!! if `strict_type_checking=false`:
!!
!! * 1 : an exception will be raised if
!!   try to retrieve a `null` as a real.
!! * 2 : a `null` retrieved as a real
!!   will return a NaN. [default]
!! * 3 : a `null` retrieved as a real
!!   will return 0.0.
INTEGER(IK),INTENT(IN),OPTIONAL :: NON_NORMAL_MODE
!! How to serialize NaN, Infinity, and
!! -Infinity real values:
!!
!! * 1 : as strings (e.g., "NaN",
!!   "Infinity", "-Infinity") [default]
!! * 2 : as JSON `null` values
LOGICAL(LK),INTENT(IN),OPTIONAL :: USE_QUIET_NAN
!! * If true [default], `null_to_real_mode=2`
!!   and [[string_to_real]] will use
!!   `ieee_quiet_nan` for NaN values.
!! * If false,
!!   `ieee_signaling_nan` will be used.
LOGICAL(LK),INTENT(IN),OPTIONAL :: STRICT_INTEGER_TYPE_CHECKING
!! * If false, when parsing JSON, if an integer numeric value
!!   cannot be converted to an integer (`integer(IK)`),
!!   then an attempt is then make to convert it
!!   to a real (`real(RK)`).
!! * If true, an exception will be raised if the integer
!!   value cannot be read.
!!
!! (default is true)
!- 670 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_file_module.F90" 2

    FILE_OBJECT = INITIALIZE_JSON_FILE_FROM_STRING(&
                                  TO_UNICODE(STR),&
!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_initialize_dummy_arguments.inc" 1
!  The dummy argument list for the various `initialize` subroutines.
!
!  See also: json_initialize_argument.inc

VERBOSE,&
COMPACT_REALS,&
PRINT_SIGNS,&
REAL_FORMAT,&
SPACES_PER_TAB,&
STRICT_TYPE_CHECKING,&
TRAILING_SPACES_SIGNIFICANT,&
CASE_SENSITIVE_KEYS,&
NO_WHITESPACE,&
UNESCAPE_STRINGS,&
COMMENT_CHAR,&
PATH_MODE,&
PATH_SEPARATOR,&
COMPRESS_VECTORS,&
ALLOW_DUPLICATE_KEYS,&
ESCAPE_SOLIDUS,&
STOP_ON_ERROR,&
NULL_TO_REAL_MODE,&
NON_NORMAL_MODE,&
USE_QUIET_NAN, &
STRICT_INTEGER_TYPE_CHECKING &
!- 674 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_file_module.F90" 2
                                                )

    END FUNCTION WRAP_INITIALIZE_JSON_FILE_FROM_STRING
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/19/2019
!
!  Cast a JSON string and a [[json_core(type)]] object
!  as a [[json_file(type)]] object.

    FUNCTION INITIALIZE_JSON_FILE_FROM_STRING_V2(STR, JSON_CORE_OBJECT) &
                                        RESULT(FILE_OBJECT)

    IMPLICIT NONE

    TYPE(JSON_FILE)                     :: FILE_OBJECT
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: STR  !! string to load JSON data from
    TYPE(JSON_CORE),INTENT(IN)          :: JSON_CORE_OBJECT

    FILE_OBJECT%CORE = JSON_CORE_OBJECT
    CALL FILE_OBJECT%DESERIALIZE(STR)

    END FUNCTION INITIALIZE_JSON_FILE_FROM_STRING_V2
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[initialize_json_file_from_string_v2]], where "str" is kind=CDK.

    FUNCTION WRAP_INITIALIZE_JSON_FILE_FROM_STRING_V2(STR,JSON_CORE_OBJECT) &
                                        RESULT(FILE_OBJECT)

    IMPLICIT NONE

    TYPE(JSON_FILE)                      :: FILE_OBJECT
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: STR  !! string to load JSON data from
    TYPE(JSON_CORE),INTENT(IN)           :: JSON_CORE_OBJECT

    FILE_OBJECT = INITIALIZE_JSON_FILE_FROM_STRING_V2(TO_UNICODE(STR),JSON_CORE_OBJECT)

    END FUNCTION WRAP_INITIALIZE_JSON_FILE_FROM_STRING_V2
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Nullify the [[json_value]] pointer in a [[json_file(type)]],
!  but do not destroy it.
!
!  This should normally only be done if the pointer is the target of
!  another pointer outside the class that is still intended to be in
!  scope after the [[json_file(type)]] has gone out of scope.
!  Otherwise, this would result in a memory leak.
!
!--- See also
!  * [[json_file_destroy]]
!
!--- History
!  * 6/30/2019 : Created

    SUBROUTINE JSON_FILE_NULLIFY(ME)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT) :: ME

    NULLIFY(ME%P)

    END SUBROUTINE JSON_FILE_NULLIFY
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Destroy the [[json_value]] data in a [[json_file(type)]].
!  This may be done when the variable is no longer needed,
!  or will be reused to open a different file.
!  Otherwise a memory leak will occur.
!
!  Optionally, also destroy the [[json_core(type)]] instance (this
!  is not necessary to prevent memory leaks, since a [[json_core(type)]]
!  does not use pointers).
!
!--- See also
!  * [[json_file_nullify]]
!
!--- History
!  * 12/9/2013 : Created
!  * 4/26/2016 : Added optional `destroy_core` argument
!
!@note This routine will be called automatically when the variable
!      goes out of scope.

    SUBROUTINE JSON_FILE_DESTROY(ME,DESTROY_CORE)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT) :: ME
    LOGICAL,INTENT(IN),OPTIONAL :: DESTROY_CORE  !! to also destroy the [[json_core(type)]].
!! default is to leave it as is.

    IF (ASSOCIATED(ME%P)) CALL ME%CORE%DESTROY(ME%P)

    IF (PRESENT(DESTROY_CORE)) THEN
        IF (DESTROY_CORE) CALL ME%CORE%DESTROY()
    END IF

    END SUBROUTINE JSON_FILE_DESTROY
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/5/2014
!
!  Move the [[json_value]] pointer from one [[json_file(type)]] to another.
!  The "from" pointer is then nullified, but not destroyed.
!
!@note If "from%p" is not associated, then an error is thrown.

    SUBROUTINE JSON_FILE_MOVE_POINTER(TO,FROM)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT) :: TO
    CLASS(JSON_FILE),INTENT(INOUT) :: FROM

    IF (ASSOCIATED(FROM%P)) THEN

        IF (FROM%FAILED()) THEN
!Don't get the data if the FROM file has an
!active exception, since it may not be valid.
            CALL TO%CORE%THROW_EXCEPTION('Error in json_file_move_pointer: '//&
                                         'error exception in FROM file.')
        ELSE
            CALL TO%INITIALIZE()  !initialize and clear any exceptions that may be present
            TO%P => FROM%P
            NULLIFY(FROM%P)
        END IF

    ELSE
        CALL TO%CORE%THROW_EXCEPTION('Error in json_file_move_pointer: '//&
                                     'pointer is not associated.')
    END IF

    END SUBROUTINE JSON_FILE_MOVE_POINTER
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/9/2013
!
!  Load the JSON data from a file.
!
!--- Example
!
!```fortran
!     program main
!      use json_module
!      implicit none
!      type(json_file) :: f
!      call f%load('my_file.json')
!      !...
!      call f%destroy()
!     end program main
!```

    SUBROUTINE JSON_FILE_LOAD(ME, FILENAME, UNIT)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: FILENAME  !! the filename to open
    INTEGER(IK),INTENT(IN),OPTIONAL      :: UNIT      !! the unit number to use
!! (if not present, a newunit
!! is used)

    CALL ME%CORE%LOAD(FILE=FILENAME, P=ME%P, UNIT=UNIT)

    END SUBROUTINE JSON_FILE_LOAD
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/13/2015
!
!  Load the JSON data from a string.
!
!--- Example
!
!  Load JSON from a string:
!```fortran
!     type(json_file) :: f
!     call f%deserialize('{ "name": "Leonidas" }')
!```

    SUBROUTINE JSON_FILE_LOAD_FROM_STRING(ME, STR)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)      :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: STR  !! string to load JSON data from

    CALL ME%CORE%DESERIALIZE(ME%P, STR)

    END SUBROUTINE JSON_FILE_LOAD_FROM_STRING
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_load_from_string]], where "str" is kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_LOAD_FROM_STRING(ME, STR)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: STR

    CALL ME%DESERIALIZE(TO_UNICODE(STR))

    END SUBROUTINE WRAP_JSON_FILE_LOAD_FROM_STRING
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/11/2015
!
!  Print the JSON file to the console.

    SUBROUTINE JSON_FILE_PRINT_TO_CONSOLE(ME)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)  :: ME

    CALL ME%CORE%PRINT(ME%P,IUNIT=INT(OUTPUT_UNIT,IK))

    END SUBROUTINE JSON_FILE_PRINT_TO_CONSOLE
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/9/2013
!
!  Prints the JSON file to the specified file unit number.

    SUBROUTINE JSON_FILE_PRINT_TO_UNIT(ME, IUNIT)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)  :: ME
    INTEGER(IK),INTENT(IN)          :: IUNIT  !! file unit number (must not be -1)

    IF (IUNIT/=UNIT2STR) THEN
        CALL ME%CORE%PRINT(ME%P,IUNIT=IUNIT)
    ELSE
        CALL ME%CORE%THROW_EXCEPTION('Error in json_file_print_to_unit: iunit must not be -1.')
    END IF

    END SUBROUTINE JSON_FILE_PRINT_TO_UNIT
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/11/2015
!
!  Print the JSON structure to the specified filename.
!  The file is opened, printed, and then closed.
!
!--- Example
!  Example loading a JSON file, changing a value, and then printing
!  result to a new file:
!```fortran
!     type(json_file) :: f
!     logical :: found
!     call f%load('my_file.json')       !open the original file
!     call f%update('version',4,found)  !change the value of a variable
!     call f%print('my_file_2.json')    !save file as new name
!```

    SUBROUTINE JSON_FILE_PRINT_TO_FILENAME(ME,FILENAME)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: FILENAME  !! filename to print to

    CALL ME%CORE%PRINT(ME%P,FILENAME)

    END SUBROUTINE JSON_FILE_PRINT_TO_FILENAME
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/11/2015
!
!  Print the JSON file to a string.
!
!--- Example
!
!  Open a JSON file, and then print the contents to a string:
!```fortran
!     type(json_file) :: f
!     character(kind=CK,len=:),allocatable :: str
!     call f%load('my_file.json')
!     call f%serialize(str)
!```

    SUBROUTINE JSON_FILE_PRINT_TO_STRING(ME,STR)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)                   :: ME
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT) :: STR  !! string to print JSON data to

    CALL ME%CORE%SERIALIZE(ME%P,STR)

    END SUBROUTINE JSON_FILE_PRINT_TO_STRING
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 2/3/2014
!
!  Returns information about a variable in a [[json_file(type)]].
!
!@note If `found` is present, no exceptions will be thrown if an
!      error occurs. Otherwise, an exception will be thrown if the
!      variable is not found.

    SUBROUTINE JSON_FILE_VARIABLE_INFO(ME,PATH,FOUND,VAR_TYPE,N_CHILDREN,NAME)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)      :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH       !! path to the variable
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND      !! the variable exists in the structure
    INTEGER(IK),INTENT(OUT),OPTIONAL    :: VAR_TYPE   !! variable type
    INTEGER(IK),INTENT(OUT),OPTIONAL    :: N_CHILDREN !! number of children
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT),OPTIONAL :: NAME !! variable name

    CALL ME%CORE%INFO(ME%P,PATH,FOUND,VAR_TYPE,N_CHILDREN,NAME)

    END SUBROUTINE JSON_FILE_VARIABLE_INFO
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_variable_info]], where "path" is kind=CDK.
!
!@note If `found` is present, no exceptions will be thrown if an
!      error occurs. Otherwise, an exception will be thrown if the
!      variable is not found.

    SUBROUTINE WRAP_JSON_FILE_VARIABLE_INFO(ME,PATH,FOUND,VAR_TYPE,N_CHILDREN,NAME)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND
    INTEGER(IK),INTENT(OUT),OPTIONAL     :: VAR_TYPE
    INTEGER(IK),INTENT(OUT),OPTIONAL     :: N_CHILDREN
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT),OPTIONAL :: NAME !! variable name

    CALL ME%INFO(TO_UNICODE(PATH),FOUND,VAR_TYPE,N_CHILDREN,NAME)

    END SUBROUTINE WRAP_JSON_FILE_VARIABLE_INFO
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 6/26/2016
!
!  Returns matrix information about a variable in a [[json_file(type)]].
!
!@note If `found` is present, no exceptions will be thrown if an
!      error occurs. Otherwise, an exception will be thrown if the
!      variable is not found.

    SUBROUTINE JSON_FILE_VARIABLE_MATRIX_INFO(ME,PATH,IS_MATRIX,FOUND,&
                                        VAR_TYPE,N_SETS,SET_SIZE,NAME)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)      :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH      !! path to the variable
    LOGICAL(LK),INTENT(OUT)             :: IS_MATRIX !! true if it is a valid matrix
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND     !! true if it was found
    INTEGER(IK),INTENT(OUT),OPTIONAL    :: VAR_TYPE  !! variable type of data in
!! the matrix (if all elements have
!! the same type)
    INTEGER(IK),INTENT(OUT),OPTIONAL    :: N_SETS    !! number of data sets (i.e., matrix
!! rows if using row-major order)
    INTEGER(IK),INTENT(OUT),OPTIONAL    :: SET_SIZE  !! size of each data set (i.e., matrix
!! cols if using row-major order)
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT),OPTIONAL :: NAME !! variable name

    CALL ME%CORE%MATRIX_INFO(ME%P,PATH,IS_MATRIX,FOUND,VAR_TYPE,N_SETS,SET_SIZE,NAME)

    END SUBROUTINE JSON_FILE_VARIABLE_MATRIX_INFO
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_variable_matrix_info]], where "path" is kind=CDK.
!
!@note If `found` is present, no exceptions will be thrown if an
!      error occurs. Otherwise, an exception will be thrown if the
!      variable is not found.

    SUBROUTINE WRAP_JSON_FILE_VARIABLE_MATRIX_INFO(ME,PATH,IS_MATRIX,FOUND,&
                                                   VAR_TYPE,N_SETS,SET_SIZE,NAME)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH      !! path to the variable
    LOGICAL(LK),INTENT(OUT)              :: IS_MATRIX !! true if it is a valid matrix
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND     !! true if it was found
    INTEGER(IK),INTENT(OUT),OPTIONAL     :: VAR_TYPE  !! variable type of data in
!! the matrix (if all elements have
!! the same type)
    INTEGER(IK),INTENT(OUT),OPTIONAL     :: N_SETS    !! number of data sets (i.e., matrix
!! rows if using row-major order)
    INTEGER(IK),INTENT(OUT),OPTIONAL     :: SET_SIZE  !! size of each data set (i.e., matrix
!! cols if using row-major order)
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT),OPTIONAL :: NAME !! variable name

    CALL ME%MATRIX_INFO(TO_UNICODE(PATH),IS_MATRIX,FOUND,VAR_TYPE,N_SETS,SET_SIZE,NAME)

    END SUBROUTINE WRAP_JSON_FILE_VARIABLE_MATRIX_INFO
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!  date: 7/23/2015
!
!  Get a [[json_value]] pointer to the JSON file root.
!
!@note This is equivalent to calling ```[[json_file]]%get('$',p)```

    SUBROUTINE JSON_FILE_GET_ROOT(ME,P)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    TYPE(JSON_VALUE),POINTER,INTENT(OUT) :: P      !! pointer to the variable

    P => ME%P

    END SUBROUTINE JSON_FILE_GET_ROOT
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Assignment operator for [[json_core(type)]] = [[json_core(type)]].
!  This will duplicate the [[json_core(type)]] and also
!  perform a deep copy of the [[json_value(type)]] data structure.

    SUBROUTINE ASSIGN_JSON_FILE(ME,F)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(OUT) :: ME
    TYPE(JSON_FILE),INTENT(IN)   :: F

    ME%CORE = F%CORE ! no pointers here so OK to copy
    CALL ME%CORE%CLONE(F%P,ME%P)

    END SUBROUTINE ASSIGN_JSON_FILE
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Assignment operator for character = [[json_core(type)]].
!  This is just a wrapper for the [[json_value_to_string]] routine.
!
!--- Note
!  * If an exception is raised or the file contains no data,
!    this will return an empty string.

    SUBROUTINE ASSIGN_JSON_FILE_TO_STRING(STR,ME)

    IMPLICIT NONE

    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT) :: STR
    CLASS(JSON_FILE),INTENT(IN) :: ME

    TYPE(JSON_CORE) :: CORE_COPY !! a copy of `core` from `me`

    IF (ME%CORE%FAILED() .OR. .NOT. ASSOCIATED(ME%P)) THEN
        STR = CK_''
    ELSE

! This is sort of a hack. Since `me` has to have `intent(in)`
! for the assignment to work, we need to make a copy of `me%core`
! so we can call the low level routine (since it needs it to
! be `intent(inout)`) because it's possible for this
! function to raise an exception.

        CORE_COPY = ME%CORE ! copy the parser settings

        CALL CORE_COPY%SERIALIZE(ME%P,STR)
        IF (ME%CORE%FAILED()) STR = CK_''

    END IF

    END SUBROUTINE ASSIGN_JSON_FILE_TO_STRING
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Assignment operator for [[json_core(type)]] = character.
!  This is just a wrapper for the [[json_file_load_from_string]] routine.

    SUBROUTINE ASSIGN_STRING_TO_JSON_FILE(ME,STR)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT) :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: STR

    IF (ASSOCIATED(ME%P)) CALL ME%DESTROY()
    IF (ME%CORE%FAILED()) CALL ME%CORE%CLEAR_EXCEPTIONS()
    CALL ME%DESERIALIZE(STR)

    END SUBROUTINE ASSIGN_STRING_TO_JSON_FILE
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[assign_string_to_json_file]], where "str" is kind=CDK.

    SUBROUTINE WRAP_ASSIGN_STRING_TO_JSON_FILE(ME,STR)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT) :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: STR

    CALL ME%ASSIGN_STRING_TO_JSON_FILE(TO_UNICODE(STR))

    END SUBROUTINE WRAP_ASSIGN_STRING_TO_JSON_FILE
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  A wrapper for [[json_file_valid_path]] for the `.in.` operator

    FUNCTION JSON_FILE_VALID_PATH_OP(PATH,ME) RESULT(FOUND)

    IMPLICIT NONE

    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH   !! the path to the variable
    CLASS(JSON_FILE),INTENT(IN)         :: ME     !! the JSON file
    LOGICAL(LK)                         :: FOUND  !! if the variable was found

    TYPE(JSON_CORE) :: CORE_COPY !! a copy of `core` from `me`

! This is sort of a hack. Since `me` has to have `intent(in)`
! for the operator to work, we need to make a copy of `me%core`
! so we can call the low level routine (since it needs it to
! be `intent(inout)`) because it's technically possible for this
! function to raise an exception. This normally should never
! happen here unless the JSON structure is malformed.

    CORE_COPY = ME%CORE ! copy the settings (need them to know
! how to interpret the path)

    FOUND = CORE_COPY%VALID_PATH(ME%P, PATH) ! call the low-level routine

    CALL CORE_COPY%DESTROY() ! just in case (but not really necessary)

    END FUNCTION JSON_FILE_VALID_PATH_OP
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_valid_path_op]], where "path" is kind=CDK.

    FUNCTION WRAP_JSON_FILE_VALID_PATH_OP(PATH,ME) RESULT(FOUND)

    IMPLICIT NONE

    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH   !! the path to the variable
    CLASS(JSON_FILE),INTENT(IN)          :: ME     !! the JSON file
    LOGICAL(LK)                          :: FOUND  !! if the variable was found

    FOUND = TO_UNICODE(PATH) .IN. ME

    END FUNCTION WRAP_JSON_FILE_VALID_PATH_OP
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Returns true if the `path` is present in the JSON file.

    FUNCTION JSON_FILE_VALID_PATH(ME,PATH) RESULT(FOUND)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)      :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH   !! the path to the variable
    LOGICAL(LK)                         :: FOUND  !! if the variable was found

    FOUND = ME%CORE%VALID_PATH(ME%P, PATH)

    END FUNCTION JSON_FILE_VALID_PATH
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_valid_path]], where "path" is kind=CDK.

    FUNCTION WRAP_JSON_FILE_VALID_PATH(ME,PATH) RESULT(FOUND)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH   !! the path to the variable
    LOGICAL(LK)                          :: FOUND  !! if the variable was found

    FOUND = ME%VALID_PATH(TO_UNICODE(PATH))

    END FUNCTION WRAP_JSON_FILE_VALID_PATH
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Rename a variable in a JSON file.

    SUBROUTINE JSON_FILE_RENAME(ME,PATH,NAME,FOUND)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)      :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH   !! the path to the variable
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: NAME   !! the new name
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND  !! if the variable was found

    CALL ME%CORE%RENAME(ME%P, PATH, NAME, FOUND)

    END SUBROUTINE JSON_FILE_RENAME
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_rename]], where "path" and "name" are kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_RENAME(ME,PATH,NAME,FOUND)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH   !! the path to the variable
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: NAME   !! the new name
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND  !! if the variable was found

    CALL ME%JSON_FILE_RENAME(TO_UNICODE(PATH),TO_UNICODE(NAME),FOUND)

    END SUBROUTINE WRAP_JSON_FILE_RENAME
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Wrapper for [[json_file_rename]] where "path" is kind=CDK).

    SUBROUTINE JSON_FILE_RENAME_PATH_ASCII(ME,PATH,NAME,FOUND)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH   !! the path to the variable
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)  :: NAME   !! the new name
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND  !! if the variable was found

    CALL ME%JSON_FILE_RENAME(TO_UNICODE(PATH),NAME,FOUND)

    END SUBROUTINE JSON_FILE_RENAME_PATH_ASCII
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Wrapper for [[json_file_rename]] where "name" is kind=CDK).

    SUBROUTINE JSON_FILE_RENAME_NAME_ASCII(ME,PATH,NAME,FOUND)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)  :: PATH   !! the path to the variable
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: NAME   !! the new name
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND  !! if the variable was found

    CALL ME%JSON_FILE_RENAME(PATH,TO_UNICODE(NAME),FOUND)

    END SUBROUTINE JSON_FILE_RENAME_NAME_ASCII
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 2/3/2014
!
!  Get a [[json_value]] pointer to an object from a JSON file.

    SUBROUTINE JSON_FILE_GET_OBJECT(ME, PATH, P, FOUND)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)  :: PATH   !! the path to the variable
    TYPE(JSON_VALUE),POINTER,INTENT(OUT) :: P      !! pointer to the variable
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND  !! if it was really found

    CALL ME%CORE%GET(ME%P, PATH, P, FOUND)

    END SUBROUTINE JSON_FILE_GET_OBJECT
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_object]], where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_GET_OBJECT(ME, PATH, P, FOUND)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH    !! the path to the variable
    TYPE(JSON_VALUE),POINTER,INTENT(OUT) :: P       !! pointer to the variable
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND   !! if it was really found

    CALL ME%GET(TO_UNICODE(PATH), P, FOUND)

    END SUBROUTINE WRAP_JSON_FILE_GET_OBJECT
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/9/2013
!
!  Get an integer value from a JSON file.

    SUBROUTINE JSON_FILE_GET_INTEGER(ME, PATH, VAL, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)      :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH    !! the path to the variable
    INTEGER(IK),INTENT(OUT)             :: VAL     !! value
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND   !! if it was really found
    INTEGER(IK),INTENT(IN),OPTIONAL     :: DEFAULT

    CALL ME%CORE%GET(ME%P, PATH, VAL, FOUND, DEFAULT)

    END SUBROUTINE JSON_FILE_GET_INTEGER
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_integer]], where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_GET_INTEGER(ME, PATH, VAL, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH   !! the path to the variable
    INTEGER(IK),INTENT(OUT)              :: VAL    !! value
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND  !! if it was really found
    INTEGER(IK),INTENT(IN),OPTIONAL      :: DEFAULT

    CALL ME%GET(TO_UNICODE(PATH), VAL, FOUND, DEFAULT)

    END SUBROUTINE WRAP_JSON_FILE_GET_INTEGER
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/20/2014
!
!  Get an integer vector from a JSON file.

    SUBROUTINE JSON_FILE_GET_INTEGER_VEC(ME, PATH, VEC, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)                   :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)              :: PATH   !! the path to the variable
    INTEGER(IK),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC    !! the value vector
    LOGICAL(LK),INTENT(OUT),OPTIONAL                 :: FOUND  !! if it was really found
    INTEGER(IK),DIMENSION(:),INTENT(IN),OPTIONAL     :: DEFAULT

    CALL ME%CORE%GET(ME%P, PATH, VEC, FOUND, DEFAULT)

    END SUBROUTINE JSON_FILE_GET_INTEGER_VEC
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_integer_vec]], where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_GET_INTEGER_VEC(ME, PATH, VEC, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)                   :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN)             :: PATH  !! the path to the variable
    INTEGER(IK),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC   !! the value vector
    LOGICAL(LK),INTENT(OUT),OPTIONAL                 :: FOUND !! if it was really found
    INTEGER(IK),DIMENSION(:),INTENT(IN),OPTIONAL     :: DEFAULT

    CALL ME%GET(TO_UNICODE(PATH), VEC, FOUND, DEFAULT)

    END SUBROUTINE WRAP_JSON_FILE_GET_INTEGER_VEC
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/9/2013
!
!  Get a real(RK) variable value from a JSON file.

    SUBROUTINE JSON_FILE_GET_REAL (ME, PATH, VAL, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)      :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH  !! the path to the variable
    REAL(RK),INTENT(OUT)                :: VAL   !! value
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND !! if it was really found
    REAL(RK),INTENT(IN),OPTIONAL        :: DEFAULT

    CALL ME%CORE%GET(ME%P, PATH, VAL, FOUND, DEFAULT)

    END SUBROUTINE JSON_FILE_GET_REAL
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_real]], where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_GET_REAL (ME, PATH, VAL, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH  !! the path to the variable
    REAL(RK),INTENT(OUT)                 :: VAL   !! value
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND !! if it was really found
    REAL(RK),INTENT(IN),OPTIONAL         :: DEFAULT

    CALL ME%GET(TO_UNICODE(PATH), VAL, FOUND, DEFAULT)

    END SUBROUTINE WRAP_JSON_FILE_GET_REAL
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/19/2014
!
!  Get a real(RK) vector from a JSON file.

    SUBROUTINE JSON_FILE_GET_REAL_VEC(ME, PATH, VEC, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)                :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)           :: PATH  !! the path to the variable
    REAL(RK),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC   !! the value vector
    LOGICAL(LK),INTENT(OUT),OPTIONAL              :: FOUND !! if it was really found
    REAL(RK),DIMENSION(:),INTENT(IN),OPTIONAL     :: DEFAULT

    CALL ME%CORE%GET(ME%P, PATH, VEC, FOUND, DEFAULT)

    END SUBROUTINE JSON_FILE_GET_REAL_VEC
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_real_vec]], where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_GET_REAL_VEC(ME, PATH, VEC, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)                :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN)          :: PATH  !! the path to the variable
    REAL(RK),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC   !! the value vector
    LOGICAL(LK),INTENT(OUT),OPTIONAL              :: FOUND !! if it was really found
    REAL(RK),DIMENSION(:),INTENT(IN),OPTIONAL     :: DEFAULT

    CALL ME%GET(TO_UNICODE(PATH), VEC, FOUND, DEFAULT)

    END SUBROUTINE WRAP_JSON_FILE_GET_REAL_VEC
!*****************************************************************************************


!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/21/2019
!
!  Alternate version of [[json_file_get_real]] where `val` is `real32`.

    SUBROUTINE JSON_FILE_GET_REAL32 (ME, PATH, VAL, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)      :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH  !! the path to the variable
    REAL(REAL32),INTENT(OUT)            :: VAL   !! value
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND !! if it was really found
    REAL(REAL32),INTENT(IN),OPTIONAL    :: DEFAULT

    CALL ME%CORE%GET(ME%P, PATH, VAL, FOUND, DEFAULT)

    END SUBROUTINE JSON_FILE_GET_REAL32
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_real32]], where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_GET_REAL32 (ME, PATH, VAL, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH  !! the path to the variable
    REAL(REAL32),INTENT(OUT)             :: VAL   !! value
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND !! if it was really found
    REAL(REAL32),INTENT(IN),OPTIONAL     :: DEFAULT

    CALL ME%GET(TO_UNICODE(PATH), VAL, FOUND, DEFAULT)

    END SUBROUTINE WRAP_JSON_FILE_GET_REAL32
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/21/2019
!
!  Alternate version of [[json_file_get_real_vec]] where `vec` is `real32`.

    SUBROUTINE JSON_FILE_GET_REAL32_VEC(ME, PATH, VEC, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)                    :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)               :: PATH  !! the path to the variable
    REAL(REAL32),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC   !! the value vector
    LOGICAL(LK),INTENT(OUT),OPTIONAL                  :: FOUND !! if it was really found
    REAL(REAL32),DIMENSION(:),INTENT(IN),OPTIONAL     :: DEFAULT

    CALL ME%CORE%GET(ME%P, PATH, VEC, FOUND, DEFAULT)

    END SUBROUTINE JSON_FILE_GET_REAL32_VEC
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_real32_vec]], where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_GET_REAL32_VEC(ME, PATH, VEC, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)                    :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN)              :: PATH  !! the path to the variable
    REAL(REAL32),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC   !! the value vector
    LOGICAL(LK),INTENT(OUT),OPTIONAL                  :: FOUND !! if it was really found
    REAL(REAL32),DIMENSION(:),INTENT(IN),OPTIONAL     :: DEFAULT

    CALL ME%GET(TO_UNICODE(PATH), VEC, FOUND, DEFAULT)

    END SUBROUTINE WRAP_JSON_FILE_GET_REAL32_VEC
!*****************************************************************************************


!- 1748


!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/9/2013
!
!  Get a logical(LK) value from a JSON file.

    SUBROUTINE JSON_FILE_GET_LOGICAL(ME,PATH,VAL,FOUND,DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)  :: PATH   !! the path to the variable
    LOGICAL(LK),INTENT(OUT)              :: VAL    !! value
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND  !! if it was really found
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: DEFAULT

    CALL ME%CORE%GET(ME%P, PATH, VAL, FOUND, DEFAULT)

    END SUBROUTINE JSON_FILE_GET_LOGICAL
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_logical]], where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_GET_LOGICAL(ME,PATH,VAL,FOUND,DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH   !! the path to the variable
    LOGICAL(LK),INTENT(OUT)              :: VAL    !! value
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND  !! if it was really found
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: DEFAULT

    CALL ME%GET(TO_UNICODE(PATH), VAL, FOUND, DEFAULT)

    END SUBROUTINE WRAP_JSON_FILE_GET_LOGICAL
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/20/2014
!
!  Get a logical(LK) vector from a JSON file.

    SUBROUTINE JSON_FILE_GET_LOGICAL_VEC(ME, PATH, VEC, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)                   :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)              :: PATH  !! the path to the variable
    LOGICAL(LK),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC   !! the value vector
    LOGICAL(LK),INTENT(OUT),OPTIONAL                 :: FOUND !! if it was really found
    LOGICAL(LK),DIMENSION(:),INTENT(IN),OPTIONAL     :: DEFAULT

    CALL ME%CORE%GET(ME%P, PATH, VEC, FOUND, DEFAULT)

    END SUBROUTINE JSON_FILE_GET_LOGICAL_VEC
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_logical_vec]], where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_GET_LOGICAL_VEC(ME, PATH, VEC, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)                   :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN)             :: PATH  !! the path to the variable
    LOGICAL(LK),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC   !! the value vector
    LOGICAL(LK),INTENT(OUT),OPTIONAL                 :: FOUND !! if it was really found
    LOGICAL(LK),DIMENSION(:),INTENT(IN),OPTIONAL     :: DEFAULT

    CALL ME%GET(TO_UNICODE(PATH), VEC, FOUND, DEFAULT)

    END SUBROUTINE WRAP_JSON_FILE_GET_LOGICAL_VEC
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/9/2013
!
!  Get a character string from a json file.
!  The output val is an allocatable character string.

    SUBROUTINE JSON_FILE_GET_STRING(ME, PATH, VAL, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)                   :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)              :: PATH  !! the path to the variable
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT) :: VAL   !! value
    LOGICAL(LK),INTENT(OUT),OPTIONAL                 :: FOUND !! if it was really found
    CHARACTER(KIND=CK,LEN=*),INTENT(IN),OPTIONAL     :: DEFAULT

    CALL ME%CORE%GET(ME%P, PATH, VAL, FOUND, DEFAULT)

    END SUBROUTINE JSON_FILE_GET_STRING
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_string]], where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_GET_STRING(ME, PATH, VAL, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)                   :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN)             :: PATH  !! the path to the variable
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT) :: VAL   !! value
    LOGICAL(LK),INTENT(OUT),OPTIONAL                 :: FOUND !! if it was really found
    CHARACTER(KIND=CK,LEN=*),INTENT(IN),OPTIONAL     :: DEFAULT

    CALL ME%GET(TO_UNICODE(PATH), VAL, FOUND, DEFAULT)

    END SUBROUTINE WRAP_JSON_FILE_GET_STRING
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/19/2014
!
!  Get a string vector from a JSON file.

    SUBROUTINE JSON_FILE_GET_STRING_VEC(ME, PATH, VEC, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)                                :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)                           :: PATH  !! the path to the variable
    CHARACTER(KIND=CK,LEN=*),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC   !! value vector
    LOGICAL(LK),INTENT(OUT),OPTIONAL                              :: FOUND !! if it was really found
    CHARACTER(KIND=CK,LEN=*),DIMENSION(:),INTENT(IN),OPTIONAL     :: DEFAULT

    CALL ME%CORE%GET(ME%P, PATH, VEC, FOUND, DEFAULT)

    END SUBROUTINE JSON_FILE_GET_STRING_VEC
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_string_vec]], where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_GET_STRING_VEC(ME, PATH, VEC, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)                                :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN)                          :: PATH  !! the path to the variable
    CHARACTER(KIND=CK,LEN=*),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC   !! value vector
    LOGICAL(LK),INTENT(OUT),OPTIONAL                              :: FOUND !! if it was really found
    CHARACTER(KIND=CK,LEN=*),DIMENSION(:),INTENT(IN),OPTIONAL     :: DEFAULT

    CALL ME%GET(TO_UNICODE(PATH), VEC, FOUND, DEFAULT)

    END SUBROUTINE WRAP_JSON_FILE_GET_STRING_VEC
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/17/2016
!
!  Get an (allocatable length) string vector from a JSON file.
!  This is just a wrapper for [[json_get_alloc_string_vec_by_path]].

    SUBROUTINE JSON_FILE_GET_ALLOC_STRING_VEC(ME, PATH, VEC, ILEN, FOUND, DEFAULT, DEFAULT_ILEN)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)      :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH !! the path to the variable
    CHARACTER(KIND=CK,LEN=:),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC !! value vector
    INTEGER(IK),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: ILEN !! the actual length
!! of each character
!! string in the array
    LOGICAL(LK),INTENT(OUT),OPTIONAL :: FOUND
    CHARACTER(KIND=CK,LEN=*),DIMENSION(:),INTENT(IN),OPTIONAL :: DEFAULT
    INTEGER(IK),DIMENSION(:),INTENT(IN),OPTIONAL :: DEFAULT_ILEN !! the actual
!! length of `default`

    CALL ME%CORE%GET(ME%P, PATH, VEC, ILEN, FOUND, DEFAULT, DEFAULT_ILEN)

    END SUBROUTINE JSON_FILE_GET_ALLOC_STRING_VEC
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_get_alloc_string_vec]], where "path" is kind=CDK.
!  This is just a wrapper for [[wrap_json_get_alloc_string_vec_by_path]].

    SUBROUTINE WRAP_JSON_FILE_GET_ALLOC_STRING_VEC(ME, PATH, VEC, ILEN, FOUND, DEFAULT, DEFAULT_ILEN)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH !! the path to the variable
    CHARACTER(KIND=CK,LEN=:),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC  !! value vector
    INTEGER(IK),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: ILEN !! the actual length
!! of each character
!! string in the array
    LOGICAL(LK),INTENT(OUT),OPTIONAL :: FOUND
    CHARACTER(KIND=CK,LEN=*),DIMENSION(:),INTENT(IN),OPTIONAL :: DEFAULT
    INTEGER(IK),DIMENSION(:),INTENT(IN),OPTIONAL :: DEFAULT_ILEN !! the actual
!! length of `default`

    CALL ME%GET(TO_UNICODE(PATH), VEC, ILEN, FOUND, DEFAULT, DEFAULT_ILEN)

    END SUBROUTINE WRAP_JSON_FILE_GET_ALLOC_STRING_VEC
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Add a [[json_value]] pointer as the root object to a JSON file.
!
!--- Note
!
!  This is mostly equivalent to:
!```fortran
!    f = [[json_file]](p)
!```
!  But without the finalization calls.
!
!  And:
!```fortran
!    if (destroy_original) call [[json_file]]%destroy()
!    call [[json_file]]%add('$',p)
!```

    SUBROUTINE JSON_FILE_ADD(ME,P,DESTROY_ORIGINAL)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: P    !! pointer to the variable to add
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: DESTROY_ORIGINAL !! if the file currently contains
!! an associated pointer, it is
!! destroyed. [Default is True]

    LOGICAL(LK) :: DESTROY   !! if `me%p` is to be destroyed

    IF (PRESENT(DESTROY_ORIGINAL)) THEN
        DESTROY = DESTROY_ORIGINAL
    ELSE
        DESTROY = .TRUE. ! default
    END IF

    IF (DESTROY) CALL ME%CORE%DESTROY(ME%P)

    ME%P => P

    END SUBROUTINE JSON_FILE_ADD
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Add a [[json_value]] pointer to an object to a JSON file.

    SUBROUTINE JSON_FILE_ADD_OBJECT(ME,PATH,P,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)  :: PATH         !! the path to the variable
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: P            !! pointer to the variable to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! if the variable had to be created

    IF (.NOT. ASSOCIATED(ME%P)) CALL ME%CORE%CREATE_OBJECT(ME%P,CK_'') ! create root

    CALL ME%CORE%ADD_BY_PATH(ME%P,PATH,P,FOUND,WAS_CREATED)

    END SUBROUTINE JSON_FILE_ADD_OBJECT
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_object]], where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_ADD_OBJECT(ME,PATH,P,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: P            !! pointer to the variable to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! if the variable had to be created

    CALL ME%JSON_FILE_ADD_OBJECT(TO_UNICODE(PATH),P,FOUND,WAS_CREATED)

    END SUBROUTINE WRAP_JSON_FILE_ADD_OBJECT
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Add an integer value to a JSON file.

    SUBROUTINE JSON_FILE_ADD_INTEGER(ME,PATH,VAL,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)      :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    INTEGER(IK),INTENT(IN)              :: VAL          !! value
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: WAS_CREATED  !! if the variable had to be created

    IF (.NOT. ASSOCIATED(ME%P)) CALL ME%CORE%CREATE_OBJECT(ME%P,CK_'') ! create root

    CALL ME%CORE%ADD_BY_PATH(ME%P,PATH,VAL,FOUND,WAS_CREATED)

    END SUBROUTINE JSON_FILE_ADD_INTEGER
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_integer]], where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_ADD_INTEGER(ME,PATH,VAL,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    INTEGER(IK),INTENT(IN)               :: VAL          !! value
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! if the variable had to be created

    CALL ME%JSON_FILE_ADD_INTEGER(TO_UNICODE(PATH),VAL,FOUND,WAS_CREATED)

    END SUBROUTINE WRAP_JSON_FILE_ADD_INTEGER
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Add an integer vector to a JSON file.

    SUBROUTINE JSON_FILE_ADD_INTEGER_VEC(ME,PATH,VEC,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)  :: PATH         !! the path to the variable
    INTEGER(IK),DIMENSION(:),INTENT(IN)  :: VEC          !! the value vector
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! if the variable had to be created

    IF (.NOT. ASSOCIATED(ME%P)) CALL ME%CORE%CREATE_OBJECT(ME%P,CK_'') ! create root

    CALL ME%CORE%ADD_BY_PATH(ME%P,PATH,VEC,FOUND,WAS_CREATED)

    END SUBROUTINE JSON_FILE_ADD_INTEGER_VEC
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_integer_vec]], where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_ADD_INTEGER_VEC(ME,PATH,VEC,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    INTEGER(IK),DIMENSION(:),INTENT(IN)  :: VEC          !! the value vector
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! if the variable had to be created

    CALL ME%JSON_FILE_ADD_INTEGER_VEC(TO_UNICODE(PATH),VEC,FOUND,WAS_CREATED)

    END SUBROUTINE WRAP_JSON_FILE_ADD_INTEGER_VEC
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Add a real(RK) variable value to a JSON file.

    SUBROUTINE JSON_FILE_ADD_REAL(ME,PATH,VAL,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)      :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    REAL(RK),INTENT(IN)                 :: VAL          !! value
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: WAS_CREATED  !! if the variable had to be created

    IF (.NOT. ASSOCIATED(ME%P)) CALL ME%CORE%CREATE_OBJECT(ME%P,CK_'') ! create root

    CALL ME%CORE%ADD_BY_PATH(ME%P,PATH,VAL,FOUND,WAS_CREATED)

    END SUBROUTINE JSON_FILE_ADD_REAL
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_real]], where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_ADD_REAL(ME,PATH,VAL,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    REAL(RK),INTENT(IN)                  :: VAL          !! value
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! if the variable had to be created

    CALL ME%JSON_FILE_ADD_REAL(TO_UNICODE(PATH),VAL,FOUND,WAS_CREATED)

    END SUBROUTINE WRAP_JSON_FILE_ADD_REAL
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Add a real(RK) vector to a JSON file.

    SUBROUTINE JSON_FILE_ADD_REAL_VEC(ME,PATH,VEC,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)  :: PATH         !! the path to the variable
    REAL(RK),DIMENSION(:),INTENT(IN)     :: VEC          !! the value vector
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! if the variable had to be created

    IF (.NOT. ASSOCIATED(ME%P)) CALL ME%CORE%CREATE_OBJECT(ME%P,CK_'') ! create root

    CALL ME%CORE%ADD_BY_PATH(ME%P,PATH,VEC,FOUND,WAS_CREATED)

    END SUBROUTINE JSON_FILE_ADD_REAL_VEC
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_real_vec]], where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_ADD_REAL_VEC(ME,PATH,VEC,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    REAL(RK),DIMENSION(:),INTENT(IN)     :: VEC          !! the value vector
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! if the variable had to be created

    CALL ME%JSON_FILE_ADD_REAL_VEC(TO_UNICODE(PATH),VEC,FOUND,WAS_CREATED)

    END SUBROUTINE WRAP_JSON_FILE_ADD_REAL_VEC
!*****************************************************************************************


!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_real]] where `val` is `real32`.

    SUBROUTINE JSON_FILE_ADD_REAL32(ME,PATH,VAL,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)      :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    REAL(REAL32),INTENT(IN)             :: VAL          !! value
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: WAS_CREATED  !! if the variable had to be created

    CALL ME%CORE%ADD_BY_PATH(ME%P,PATH,VAL,FOUND,WAS_CREATED)

    END SUBROUTINE JSON_FILE_ADD_REAL32
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_real32]], where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_ADD_REAL32(ME,PATH,VAL,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    REAL(REAL32),INTENT(IN)              :: VAL          !! value
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! if the variable had to be created

    CALL ME%JSON_FILE_ADD_REAL32(TO_UNICODE(PATH),VAL,FOUND,WAS_CREATED)

    END SUBROUTINE WRAP_JSON_FILE_ADD_REAL32
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_real_vec]] where `vec` is `real32`.

    SUBROUTINE JSON_FILE_ADD_REAL32_VEC(ME,PATH,VEC,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)  :: PATH         !! the path to the variable
    REAL(REAL32),DIMENSION(:),INTENT(IN) :: VEC          !! the value vector
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! if the variable had to be created

    CALL ME%CORE%ADD_BY_PATH(ME%P,PATH,VEC,FOUND,WAS_CREATED)

    END SUBROUTINE JSON_FILE_ADD_REAL32_VEC
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_real32_vec]], where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_ADD_REAL32_VEC(ME,PATH,VEC,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    REAL(REAL32),DIMENSION(:),INTENT(IN) :: VEC          !! the value vector
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! if the variable had to be created

    CALL ME%JSON_FILE_ADD_REAL32_VEC(TO_UNICODE(PATH),VEC,FOUND,WAS_CREATED)

    END SUBROUTINE WRAP_JSON_FILE_ADD_REAL32_VEC
!*****************************************************************************************


!- 2379


!*****************************************************************************************
!> author: Jacob Williams
!
!  Add a logical(LK) value to a JSON file.

    SUBROUTINE JSON_FILE_ADD_LOGICAL(ME,PATH,VAL,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)  :: PATH         !! the path to the variable
    LOGICAL(LK),INTENT(IN)               :: VAL          !! value
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! if the variable had to be created

    IF (.NOT. ASSOCIATED(ME%P)) CALL ME%CORE%CREATE_OBJECT(ME%P,CK_'') ! create root

    CALL ME%CORE%ADD_BY_PATH(ME%P,PATH,VAL,FOUND,WAS_CREATED)

    END SUBROUTINE JSON_FILE_ADD_LOGICAL
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_logical]], where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_ADD_LOGICAL(ME,PATH,VAL,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    LOGICAL(LK),INTENT(IN)               :: VAL          !! value
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! if the variable had to be created

    CALL ME%JSON_FILE_ADD_LOGICAL(TO_UNICODE(PATH),VAL,FOUND,WAS_CREATED)

    END SUBROUTINE WRAP_JSON_FILE_ADD_LOGICAL
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Add a logical(LK) vector to a JSON file.

    SUBROUTINE JSON_FILE_ADD_LOGICAL_VEC(ME,PATH,VEC,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)  :: PATH         !! the path to the variable
    LOGICAL(LK),DIMENSION(:),INTENT(IN)  :: VEC          !! the value vector
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! if the variable had to be created

    IF (.NOT. ASSOCIATED(ME%P)) CALL ME%CORE%CREATE_OBJECT(ME%P,CK_'') ! create root

    CALL ME%CORE%ADD_BY_PATH(ME%P,PATH,VEC,FOUND,WAS_CREATED)

    END SUBROUTINE JSON_FILE_ADD_LOGICAL_VEC
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_logical_vec]], where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_ADD_LOGICAL_VEC(ME,PATH,VEC,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    LOGICAL(LK),DIMENSION(:),INTENT(IN)  :: VEC          !! the value vector
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! if the variable had to be created

    CALL ME%JSON_FILE_ADD_LOGICAL_VEC(TO_UNICODE(PATH),VEC,FOUND,WAS_CREATED)

    END SUBROUTINE WRAP_JSON_FILE_ADD_LOGICAL_VEC
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Add a character string to a json file.

    SUBROUTINE JSON_FILE_ADD_STRING(ME,PATH,VAL,FOUND,WAS_CREATED,TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)      :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: VAL          !! value
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: WAS_CREATED  !! if the variable had to be created
    LOGICAL(LK),INTENT(IN),OPTIONAL     :: TRIM_STR     !! if TRIM() should be called for the `val`
    LOGICAL(LK),INTENT(IN),OPTIONAL     :: ADJUSTL_STR  !! if ADJUSTL() should be called for the `val`
!! (note that ADJUSTL is done before TRIM)

    IF (.NOT. ASSOCIATED(ME%P)) CALL ME%CORE%CREATE_OBJECT(ME%P,CK_'') ! create root

    CALL ME%CORE%ADD_BY_PATH(ME%P,PATH,VAL,FOUND,WAS_CREATED,TRIM_STR,ADJUSTL_STR)

    END SUBROUTINE JSON_FILE_ADD_STRING
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_string]], where "path" and "val" are kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_ADD_STRING(ME,PATH,VAL,FOUND,WAS_CREATED,TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: VAL          !! value
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! if the variable had to be created
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: TRIM_STR     !! if TRIM() should be called for the `val`
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: ADJUSTL_STR  !! if ADJUSTL() should be called for the `val`
!! (note that ADJUSTL is done before TRIM)

    CALL ME%JSON_FILE_ADD_STRING(TO_UNICODE(PATH),TO_UNICODE(VAL),FOUND,&
                                    WAS_CREATED,TRIM_STR,ADJUSTL_STR)

    END SUBROUTINE WRAP_JSON_FILE_ADD_STRING
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Wrapper for [[json_file_add_string]] where "path" is kind=CDK).

    SUBROUTINE JSON_FILE_ADD_STRING_PATH_ASCII(ME,PATH,VAL,FOUND,&
                                                    WAS_CREATED,TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)  :: VAL          !! value
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! if the variable had to be created
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: TRIM_STR     !! if TRIM() should be called for the `val`
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: ADJUSTL_STR  !! if ADJUSTL() should be called for the `val`
!! (note that ADJUSTL is done before TRIM)

    IF (.NOT. ASSOCIATED(ME%P)) CALL ME%CORE%CREATE_OBJECT(ME%P,CK_'') ! create root

    CALL ME%JSON_FILE_ADD_STRING(TO_UNICODE(PATH),VAL,FOUND,&
                                    WAS_CREATED,TRIM_STR,ADJUSTL_STR)

    END SUBROUTINE JSON_FILE_ADD_STRING_PATH_ASCII
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Wrapper for [[json_file_add_string]] where "val" is kind=CDK).

    SUBROUTINE JSON_FILE_ADD_STRING_VALUE_ASCII(ME,PATH,VAL,FOUND,&
                                                    WAS_CREATED,TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)  :: PATH         !! the path to the variable
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: VAL          !! value
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! if the variable had to be created
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: TRIM_STR     !! if TRIM() should be called for the `val`
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: ADJUSTL_STR  !! if ADJUSTL() should be called for the `val`
!! (note that ADJUSTL is done before TRIM)

    IF (.NOT. ASSOCIATED(ME%P)) CALL ME%CORE%CREATE_OBJECT(ME%P,CK_'') ! create root

    CALL ME%JSON_FILE_ADD_STRING(PATH,TO_UNICODE(VAL),FOUND,&
                                    WAS_CREATED,TRIM_STR,ADJUSTL_STR)

    END SUBROUTINE JSON_FILE_ADD_STRING_VALUE_ASCII
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Add a string vector to a JSON file.

    SUBROUTINE JSON_FILE_ADD_STRING_VEC(ME,PATH,VEC,FOUND,&
                                            WAS_CREATED,ILEN,TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)                   :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)              :: PATH         !! the path to the variable
    CHARACTER(KIND=CK,LEN=*),DIMENSION(:),INTENT(IN) :: VEC          !! the value vector
    LOGICAL(LK),INTENT(OUT),OPTIONAL                 :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL                 :: WAS_CREATED  !! if the variable had to be created
    INTEGER(IK),DIMENSION(:),INTENT(IN),OPTIONAL     :: ILEN         !! the string lengths of each
!! element in `value`. If not present,
!! the full `len(value)` string is added
!! for each element.
    LOGICAL(LK),INTENT(IN),OPTIONAL                  :: TRIM_STR     !! if TRIM() should be called for each elEMENT
    LOGICAL(LK),INTENT(IN),OPTIONAL                  :: ADJUSTL_STR  !! if ADJUSTL() should be called for each ELEMENT
!! (note that ADJUSTL is done before TRIM)

    IF (.NOT. ASSOCIATED(ME%P)) CALL ME%CORE%CREATE_OBJECT(ME%P,CK_'') ! create root

    CALL ME%CORE%ADD_BY_PATH(ME%P,PATH,VEC,FOUND,WAS_CREATED,ILEN,TRIM_STR,ADJUSTL_STR)

    END SUBROUTINE JSON_FILE_ADD_STRING_VEC
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_string_vec]], where "path" and "vec" are kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_ADD_STRING_VEC(ME,PATH,VEC,FOUND,&
                                                WAS_CREATED,ILEN,TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)                   :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN)             :: PATH         !! the path to the variable
    CHARACTER(KIND=CDK,LEN=*),DIMENSION(:),INTENT(IN):: VEC          !! the value vector
    LOGICAL(LK),INTENT(OUT),OPTIONAL                 :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL                 :: WAS_CREATED  !! if the variable had to be created
    INTEGER(IK),DIMENSION(:),INTENT(IN),OPTIONAL     :: ILEN         !! the string lengths of each
!! element in `value`. If not present,
!! the full `len(value)` string is added
!! for each element.
    LOGICAL(LK),INTENT(IN),OPTIONAL                  :: TRIM_STR     !! if TRIM() should be called for each elEMENT
    LOGICAL(LK),INTENT(IN),OPTIONAL                  :: ADJUSTL_STR  !! if ADJUSTL() should be called for each ELEMENT
!! (note that ADJUSTL is done before TRIM)

    CALL ME%JSON_FILE_ADD_STRING_VEC(TO_UNICODE(PATH),TO_UNICODE(VEC),FOUND,&
                                        WAS_CREATED,ILEN,TRIM_STR,ADJUSTL_STR)

    END SUBROUTINE WRAP_JSON_FILE_ADD_STRING_VEC
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_string_vec]], where "path" is kind=CDK.

    SUBROUTINE JSON_FILE_ADD_STRING_VEC_PATH_ASCII(ME,PATH,VEC,FOUND,&
                                                    WAS_CREATED,ILEN,TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)                   :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN)             :: PATH         !! the path to the variable
    CHARACTER(KIND=CK,LEN=*),DIMENSION(:),INTENT(IN) :: VEC          !! the value vector
    LOGICAL(LK),INTENT(OUT),OPTIONAL                 :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL                 :: WAS_CREATED  !! if the variable had to be created
    INTEGER(IK),DIMENSION(:),INTENT(IN),OPTIONAL     :: ILEN         !! the string lengths of each
!! element in `value`. If not present,
!! the full `len(value)` string is added
!! for each element.
    LOGICAL(LK),INTENT(IN),OPTIONAL                  :: TRIM_STR     !! if TRIM() should be called for each elEMENT
    LOGICAL(LK),INTENT(IN),OPTIONAL                  :: ADJUSTL_STR  !! if ADJUSTL() should be called for each ELEMENT
!! (note that ADJUSTL is done before TRIM)

    CALL ME%JSON_FILE_ADD_STRING_VEC(TO_UNICODE(PATH),VEC,FOUND,&
                                        WAS_CREATED,ILEN,TRIM_STR,ADJUSTL_STR)

    END SUBROUTINE JSON_FILE_ADD_STRING_VEC_PATH_ASCII
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Alternate version of [[json_file_add_string_vec]], where "vec" is kind=CDK.

    SUBROUTINE JSON_FILE_ADD_STRING_VEC_VEC_ASCII(ME,PATH,VEC,FOUND,&
                                                    WAS_CREATED,ILEN,TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)                    :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)               :: PATH         !! the path to the variable
    CHARACTER(KIND=CDK,LEN=*),DIMENSION(:),INTENT(IN) :: VEC          !! the value vector
    LOGICAL(LK),INTENT(OUT),OPTIONAL                  :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL                  :: WAS_CREATED  !! if the variable had to be created
    INTEGER(IK),DIMENSION(:),INTENT(IN),OPTIONAL      :: ILEN         !! the string lengths of each
!! element in `value`. If not present,
!! the full `len(value)` string is added
!! for each element.
    LOGICAL(LK),INTENT(IN),OPTIONAL                   :: TRIM_STR     !! if TRIM() should be called for each eLEMENT
    LOGICAL(LK),INTENT(IN),OPTIONAL                   :: ADJUSTL_STR  !! if ADJUSTL() should be called for eacH ELEMENT
!! (note that ADJUSTL is done before TRIM)

    CALL ME%JSON_FILE_ADD_STRING_VEC(PATH,TO_UNICODE(VEC),FOUND,&
                                        WAS_CREATED,ILEN,TRIM_STR,ADJUSTL_STR)

    END SUBROUTINE JSON_FILE_ADD_STRING_VEC_VEC_ASCII
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/10/2015
!
!  Given the path string, if the variable is present in the file,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.
!
!--- See also
!  * [[json_update_integer]]

    SUBROUTINE JSON_FILE_UPDATE_INTEGER(ME,PATH,VAL,FOUND)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)      :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH
    INTEGER(IK),INTENT(IN)              :: VAL
    LOGICAL(LK),INTENT(OUT)             :: FOUND

    IF (.NOT. ME%CORE%FAILED()) CALL ME%CORE%UPDATE(ME%P,PATH,VAL,FOUND)

    END SUBROUTINE JSON_FILE_UPDATE_INTEGER
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_update_integer]], where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_UPDATE_INTEGER(ME,PATH,VAL,FOUND)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH
    INTEGER(IK),INTENT(IN)               :: VAL
    LOGICAL(LK),INTENT(OUT)              :: FOUND

    CALL ME%UPDATE(TO_UNICODE(PATH),VAL,FOUND)

    END SUBROUTINE WRAP_JSON_FILE_UPDATE_INTEGER
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/10/2015
!
!  Given the path string, if the variable is present in the file,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.
!
!--- See also
!  * [[json_update_logical]]

    SUBROUTINE JSON_FILE_UPDATE_LOGICAL(ME,PATH,VAL,FOUND)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)      :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH
    LOGICAL(LK),INTENT(IN)              :: VAL
    LOGICAL(LK),INTENT(OUT)             :: FOUND

    IF (.NOT. ME%CORE%FAILED()) CALL ME%CORE%UPDATE(ME%P,PATH,VAL,FOUND)

    END SUBROUTINE JSON_FILE_UPDATE_LOGICAL
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_update_logical]], where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_UPDATE_LOGICAL(ME,PATH,VAL,FOUND)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH
    LOGICAL(LK),INTENT(IN)               :: VAL
    LOGICAL(LK),INTENT(OUT)              :: FOUND

    CALL ME%UPDATE(TO_UNICODE(PATH),VAL,FOUND)

    END SUBROUTINE WRAP_JSON_FILE_UPDATE_LOGICAL
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/10/2015
!
!  Given the path string, if the variable is present in the file,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.

    SUBROUTINE JSON_FILE_UPDATE_REAL(ME,PATH,VAL,FOUND)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)      :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH
    REAL(RK),INTENT(IN)                 :: VAL
    LOGICAL(LK),INTENT(OUT)             :: FOUND

    IF (.NOT. ME%CORE%FAILED()) CALL ME%CORE%UPDATE(ME%P,PATH,VAL,FOUND)

    END SUBROUTINE JSON_FILE_UPDATE_REAL
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_update_real]], where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_UPDATE_REAL(ME,PATH,VAL,FOUND)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH
    REAL(RK),INTENT(IN)                  :: VAL
    LOGICAL(LK),INTENT(OUT)              :: FOUND

    CALL ME%UPDATE(TO_UNICODE(PATH),VAL,FOUND)

    END SUBROUTINE WRAP_JSON_FILE_UPDATE_REAL
!*****************************************************************************************


!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/21/2019
!
!  Alternate version of [[json_file_update_real]] where `val` is `real32`.

    SUBROUTINE JSON_FILE_UPDATE_REAL32(ME,PATH,VAL,FOUND)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)      :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH
    REAL(REAL32),INTENT(IN)             :: VAL
    LOGICAL(LK),INTENT(OUT)             :: FOUND

    CALL ME%UPDATE(PATH,REAL(VAL,RK),FOUND)

    END SUBROUTINE JSON_FILE_UPDATE_REAL32
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_update_real32]], where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_UPDATE_REAL32(ME,PATH,VAL,FOUND)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH
    REAL(REAL32),INTENT(IN)              :: VAL
    LOGICAL(LK),INTENT(OUT)              :: FOUND

    CALL ME%UPDATE(TO_UNICODE(PATH),VAL,FOUND)

    END SUBROUTINE WRAP_JSON_FILE_UPDATE_REAL32
!*****************************************************************************************


!- 2889


!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/10/2015
!
!  Given the path string, if the variable is present in the file,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.
!
!--- See also
!  * [[json_update_string]]

    SUBROUTINE JSON_FILE_UPDATE_STRING(ME,PATH,VAL,FOUND,TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)      :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: VAL
    LOGICAL(LK),INTENT(OUT)             :: FOUND
    LOGICAL(LK),INTENT(IN),OPTIONAL     :: TRIM_STR     !! if TRIM() should be called for the `val`
    LOGICAL(LK),INTENT(IN),OPTIONAL     :: ADJUSTL_STR  !! if ADJUSTL() should be called for the `val`
!! (note that ADJUSTL is done before TRIM)

    IF (.NOT. ME%CORE%FAILED()) CALL ME%CORE%UPDATE(ME%P,PATH,VAL,FOUND,TRIM_STR,ADJUSTL_STR)

    END SUBROUTINE JSON_FILE_UPDATE_STRING
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_update_string]], where "path" and "val" are kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_UPDATE_STRING(ME,PATH,VAL,FOUND,TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: VAL
    LOGICAL(LK),INTENT(OUT)              :: FOUND
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: TRIM_STR     !! if TRIM() should be called for the `val`
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: ADJUSTL_STR  !! if ADJUSTL() should be called for the `val`
!! (note that ADJUSTL is done before TRIM)

    CALL ME%UPDATE(TO_UNICODE(PATH),TO_UNICODE(VAL),FOUND,TRIM_STR,ADJUSTL_STR)

    END SUBROUTINE WRAP_JSON_FILE_UPDATE_STRING
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_update_string]], where "path" is kind=CDK.

    SUBROUTINE JSON_FILE_UPDATE_STRING_NAME_ASCII(ME,PATH,VAL,FOUND,TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH
    CHARACTER(KIND=CK, LEN=*),INTENT(IN) :: VAL
    LOGICAL(LK),INTENT(OUT)              :: FOUND
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: TRIM_STR     !! if TRIM() should be called for the `val`
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: ADJUSTL_STR  !! if ADJUSTL() should be called for the `val`
!! (note that ADJUSTL is done before TRIM)

    CALL ME%UPDATE(TO_UNICODE(PATH),VAL,FOUND,TRIM_STR,ADJUSTL_STR)

    END SUBROUTINE JSON_FILE_UPDATE_STRING_NAME_ASCII
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_update_string]], where "val" is kind=CDK.

    SUBROUTINE JSON_FILE_UPDATE_STRING_VAL_ASCII(ME,PATH,VAL,FOUND,TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CK, LEN=*),INTENT(IN) :: PATH
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: VAL
    LOGICAL(LK),INTENT(OUT)              :: FOUND
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: TRIM_STR     !! if TRIM() should be called for the `val`
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: ADJUSTL_STR  !! if ADJUSTL() should be called for the `val`
!! (note that ADJUSTL is done before TRIM)

    CALL ME%UPDATE(PATH,TO_UNICODE(VAL),FOUND,TRIM_STR,ADJUSTL_STR)

    END SUBROUTINE JSON_FILE_UPDATE_STRING_VAL_ASCII
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 6/11/2016
!
!  Traverse the JSON structure in the file.
!  This routine calls the user-specified [[json_traverse_callback_func]]
!  for each element of the structure.

    SUBROUTINE JSON_FILE_TRAVERSE(ME,TRAVERSE_CALLBACK)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)         :: ME
    PROCEDURE(JSON_TRAVERSE_CALLBACK_FUNC) :: TRAVERSE_CALLBACK

    CALL ME%CORE%TRAVERSE(ME%P,TRAVERSE_CALLBACK)

    END SUBROUTINE JSON_FILE_TRAVERSE
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 7/7/2018
!
!  Remove a variable from a JSON file.
!
!@note This is just a wrapper to [[remove_if_present]].

    SUBROUTINE JSON_FILE_REMOVE(ME,PATH)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)      :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH !! the path to the variable

    CALL ME%CORE%REMOVE_IF_PRESENT(ME%P,PATH)

    END SUBROUTINE JSON_FILE_REMOVE
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_file_remove]], where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_FILE_REMOVE(ME,PATH)

    IMPLICIT NONE

    CLASS(JSON_FILE),INTENT(INOUT)       :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH !! the path to the variable

    CALL ME%REMOVE(TO_UNICODE(PATH))

    END SUBROUTINE WRAP_JSON_FILE_REMOVE
!*****************************************************************************************

!*****************************************************************************************
    END MODULE ModLib_JSONFile
!*****************************************************************************************

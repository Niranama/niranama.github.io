!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_value_module.F90"
!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  This module provides a low-level interface for manipulation of JSON data.
!  The two public entities are [[json_value]], and [[json_core(type)]].
!  The [[json_file_module]] provides a higher-level interface to some
!  of these routines.
!
!--- License
!  * JSON-Fortran is released under a BSD-style license.
!    See the [LICENSE](https://github.com/jacobwilliams/json-fortran/blob/master/LICENSE)
!    file for details.

    MODULE ModLib_JSONValue

    USE,INTRINSIC :: ISO_FORTRAN_ENV, ONLY: IOSTAT_END,ERROR_UNIT,OUTPUT_UNIT
    USE,INTRINSIC :: IEEE_ARITHMETIC
    USE ModLib_JSONKinds
    USE ModLib_JSONParameters
    USE ModLib_JSONString

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
!- 28 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_value_module.F90" 2

!*********************************************************
!>
!  If Unicode is not enabled, then
!  JSON files are opened using access='STREAM' and
!  form='UNFORMATTED'.  This allows the file to
!  be read faster.
!
!- 38

    LOGICAL,PARAMETER :: USE_UNFORMATTED_STREAM = .TRUE.

!*********************************************************

!*********************************************************
!>
!  If Unicode is not enabled, then
!  JSON files are opened using access='STREAM' and
!  form='UNFORMATTED'.  This allows the file to
!  be read faster.
!
!- 52

    CHARACTER(KIND=CDK,LEN=*),PARAMETER :: ACCESS_SPEC = 'STREAM'

!*********************************************************

!*********************************************************
!>
!  If Unicode is not enabled, then
!  JSON files are opened using access='STREAM' and
!  form='UNFORMATTED'.  This allows the file to
!  be read faster.
!
!- 66

    CHARACTER(KIND=CDK,LEN=*),PARAMETER :: FORM_SPEC   = 'UNFORMATTED'

!*********************************************************

!*********************************************************
!>
!  Type used to construct the linked-list JSON structure.
!  Normally, this should always be a pointer variable.
!  This type should only be used by an instance of [[json_core(type)]].
!
!--- Example
!
!  The following test program:
!
!````fortran
!    program test
!     use json_module
!     implicit none
!     type(json_core) :: json
!     type(json_value),pointer :: p
!     call json%create_object(p,'')   !create the root
!     call json%add(p,'year',1805)    !add some data
!     call json%add(p,'value',1.0_RK) !add some data
!     call json%print(p,'test.json')  !write it to a file
!     call json%destroy(p)            !cleanup
!    end program test
!````
!
!  Produces the JSON file **test.json**:
!
!````json
!    {
!      "year": 1805,
!      "value": 0.1E+1
!    }
!````
!
!@warning Pointers of this type should only be allocated
!         using the methods from [[json_core(type)]].

    TYPE,PUBLIC :: JSON_VALUE

!force the constituents to be stored contiguously
![note: on Intel, the order of the variables below
! is significant to avoid the misaligned field warnings]
        SEQUENCE

        PRIVATE

!for the linked list:
        TYPE(JSON_VALUE),POINTER :: PREVIOUS => NULL()  !! previous item in the list
        TYPE(JSON_VALUE),POINTER :: NEXT     => NULL()  !! next item in the list
        TYPE(JSON_VALUE),POINTER :: PARENT   => NULL()  !! parent item of this
        TYPE(JSON_VALUE),POINTER :: CHILDREN => NULL()  !! first child item of this
        TYPE(JSON_VALUE),POINTER :: TAIL     => NULL()  !! last child item of this

        CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: NAME  !! variable name (unescaped)

        REAL(RK),ALLOCATABLE                 :: DBL_VALUE  !! real data for this variable
        LOGICAL(LK),ALLOCATABLE              :: LOG_VALUE  !! logical data for this variable
        CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: STR_VALUE  !! string data for this variable
!! (unescaped)
        INTEGER(IK),ALLOCATABLE              :: INT_VALUE  !! integer data for this variable

        INTEGER(IK) :: VAR_TYPE = JSON_UNKNOWN  !! variable type

        INTEGER(IK),PRIVATE :: N_CHILDREN = 0   !! number of children

    END TYPE JSON_VALUE
!*********************************************************

!*********************************************************
!>
!  To access the core routines for manipulation
!  of [[json_value]] pointer variables. This class allows
!  for thread safe use of the module.
!
!--- Usage
!````fortran
!    program test
!     use json_module, wp=>json_RK
!     implicit none
!     type(json_core) :: json     !<--have to declare this
!     type(json_value),pointer :: p
!     call json%create_object(p,'')   !create the root
!     call json%add(p,'year',1805)    !add some data
!     call json%add(p,'value',1.0_wp) !add some data
!     call json%print(p,'test.json')  !write it to a file
!     call json%destroy(p)            !cleanup
!    end program test
!````
    TYPE,PUBLIC :: JSON_CORE

        PRIVATE

        INTEGER(IK) :: SPACES_PER_TAB = 2    !! number of spaces for indenting

        LOGICAL(LK) :: COMPACT_REAL = .TRUE.  !! to use the "compact" form of real
!! numbers for output
        CHARACTER(KIND=CDK,LEN=:),ALLOCATABLE :: REAL_FMT   !! the format string to use
!! for converting real numbers to strings.
!! It can be set in [[json_initialize]],
!! and used in [[json_value_print]]
!! If not set, then `default_real_fmt`
!! is used instead.

        LOGICAL(LK) :: IS_VERBOSE = .FALSE.        !! if true, all exceptions are
!! immediately printed to console.

        LOGICAL(LK) :: STOP_ON_ERROR = .FALSE.     !! if true, then the program is
!! stopped immediately when an
!! exception is raised.

        LOGICAL(LK) :: EXCEPTION_THROWN = .FALSE.  !! The error flag. Will be set to true
!! when an error is thrown in the class.
!! Many of the methods will check this
!! and return immediately if it is true.
        CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: ERR_MESSAGE
!! the error message.
!! if `exception_thrown=False` then
!! this variable is not allocated.

        INTEGER(IK) :: CHAR_COUNT = 0    !! character position in the current line
        INTEGER(IK) :: LINE_COUNT = 1    !! lines read counter
        INTEGER(IK) :: PUSHED_INDEX = 0  !! used when parsing lines in file
        CHARACTER(KIND=CK,LEN=PUSHED_CHAR_SIZE) :: PUSHED_CHAR = CK_''  !! used when parsing
!! lines in file

        INTEGER(IK) :: IPOS = 1  !! for allocatable strings: next character to read

        LOGICAL(LK) :: STRICT_TYPE_CHECKING = .FALSE. !! if true, then no type conversions are done
!! in the `get` routines if the actual variable
!! type is different from the return type (for
!! example, integer to real).

        LOGICAL(LK) :: TRAILING_SPACES_SIGNIFICANT = .FALSE.    !! for name and path comparisons, if trailing
!! space is to be considered significant.

        LOGICAL(LK) :: CASE_SENSITIVE_KEYS = .TRUE.    !! if name and path comparisons
!! are case sensitive.

        LOGICAL(LK) :: NO_WHITESPACE = .FALSE. !! when printing a JSON string, don't include
!! non-significant spaces or line breaks.
!! If true, the entire structure will be
!! printed on one line.

        LOGICAL(LK) :: UNESCAPED_STRINGS = .TRUE.  !! If false, then the escaped
!! string is returned from [[json_get_string]]
!! and similar routines. If true [default],
!! then the string is returned unescaped.

        LOGICAL(LK) :: ALLOW_COMMENTS = .TRUE.  !! if true, any comments will be ignored when
!! parsing a file. The comment tokens are defined
!! by the `comment_char` character variable.
        CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: COMMENT_CHAR !! comment tokens when
!! `allow_comments` is true.
!! Examples: '`!`' or '`#`'.
!! Default is `CK_'/!#'`.

        INTEGER(IK) :: PATH_MODE = 1_IK  !! How the path strings are interpreted in the
!! `get_by_path` routines:
!!
!! * 1 -- Default mode (see [[json_get_by_path_default]])
!! * 2 -- as RFC 6901 "JSON Pointer" paths
!!   (see [[json_get_by_path_rfc6901]])
!! * 3 -- JSONPath "bracket-notation"
!!   see [[json_get_by_path_jsonpath_bracket]])

        CHARACTER(KIND=CK,LEN=1) :: PATH_SEPARATOR = DOT !! The `path` separator to use
!! in the "default" mode for
!! the paths in the various
!! `get_by_path` routines.
!! Note: if `path_mode/=1`
!! then this is ignored.

        LOGICAL(LK) :: COMPRESS_VECTORS = .FALSE. !! If true, then arrays of integers,
!! nulls, reals, & logicals are
!! printed all on one line.
!! [Note: `no_whitespace` will
!! override this option if necessary]

        LOGICAL(LK) :: ALLOW_DUPLICATE_KEYS = .TRUE. !! If False, then after parsing, if any
!! duplicate keys are found, an error is
!! thrown. A call to [[json_value_validate]]
!! will also check for duplicates. If True
!! [default] then no special checks are done

        LOGICAL(LK) :: ESCAPE_SOLIDUS = .FALSE.   !! If True then the solidus "`/`" is always escaped
!! ("`\/`") when serializing JSON.
!! If False [default], then it is not escaped.
!! Note that this option does not affect parsing
!! (both escaped and unescaped versions are still
!! valid in all cases).

        INTEGER(IK) :: NULL_TO_REAL_MODE = 2_IK   !! if `strict_type_checking=false`:
!!
!! * 1 : an exception will be raised if
!!   try to retrieve a `null` as a real.
!! * 2 : a `null` retrieved as a real
!!   will return NaN. [default]
!! * 3 : a `null` retrieved as a real
!!   will return 0.0.

        LOGICAL(LK) :: NON_NORMALS_TO_NULL = .FALSE. !! How to serialize NaN, Infinity,
!! and -Infinity real values:
!!
!! * If true : as JSON `null` values
!! * If false : as strings (e.g., "NaN",
!!   "Infinity", "-Infinity") [default]

        LOGICAL(LK) :: USE_QUIET_NAN = .TRUE. !! if true [default], `null_to_real_mode=2`
!! and [[string_to_real]] will use
!! `ieee_quiet_nan` for NaN values. If false,
!! `ieee_signaling_nan` will be used.

        LOGICAL(LK) :: STRICT_INTEGER_TYPE_CHECKING = .TRUE.
!! * If false, when parsing JSON, if an integer numeric value
!!   cannot be converted to an integer (`integer(IK)`),
!!   then an attempt is then make to convert it
!!   to a real (`real(RK)`).
!! * If true [default], an exception will be raised if an integer
!!   value cannot be read when parsing JSON.

        INTEGER :: ICHUNK = 0 !! index in `chunk` for [[pop_char]]
!! when `use_unformatted_stream=True`
        INTEGER :: FILESIZE = 0 !! the file size when when `use_unformatted_stream=True`
        CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: CHUNK   !! a chunk read from a stream file
!! when `use_unformatted_stream=True`

        CONTAINS

        PRIVATE

!>
!  Return a child of a [[json_value]] structure.
        GENERIC,PUBLIC :: GET_CHILD => JSON_VALUE_GET_CHILD_BY_INDEX, &
                                       JSON_VALUE_GET_CHILD,&
                                       JSON_VALUE_GET_CHILD_BY_NAME
        PROCEDURE,PRIVATE :: JSON_VALUE_GET_CHILD_BY_INDEX
        PROCEDURE,PRIVATE :: JSON_VALUE_GET_CHILD_BY_NAME
        PROCEDURE,PRIVATE :: JSON_VALUE_GET_CHILD

!>
!  Add objects to a linked list of [[json_value]]s.
!
!@note It might make more sense to call this `add_child`.
        GENERIC,PUBLIC :: ADD => JSON_VALUE_ADD_MEMBER, &
                                 JSON_VALUE_ADD_NULL, &
                                 JSON_VALUE_ADD_INTEGER, &
                                 JSON_VALUE_ADD_INTEGER_VEC, &

                                 JSON_VALUE_ADD_REAL32, &
                                 JSON_VALUE_ADD_REAL32_VEC, &

                                 JSON_VALUE_ADD_REAL, &
                                 JSON_VALUE_ADD_REAL_VEC, &
!- 326

                                 JSON_VALUE_ADD_LOGICAL, &
                                 JSON_VALUE_ADD_LOGICAL_VEC, &
                                 JSON_VALUE_ADD_STRING, &
                                 JSON_VALUE_ADD_STRING_VEC
!- 336


        PROCEDURE,PRIVATE :: JSON_VALUE_ADD_MEMBER
        PROCEDURE,PRIVATE :: JSON_VALUE_ADD_INTEGER
        PROCEDURE,PRIVATE :: JSON_VALUE_ADD_NULL
        PROCEDURE,PRIVATE :: JSON_VALUE_ADD_INTEGER_VEC

        PROCEDURE,PRIVATE :: JSON_VALUE_ADD_REAL32
        PROCEDURE,PRIVATE :: JSON_VALUE_ADD_REAL32_VEC

        PROCEDURE,PRIVATE :: JSON_VALUE_ADD_REAL
        PROCEDURE,PRIVATE :: JSON_VALUE_ADD_REAL_VEC
!- 351

        PROCEDURE,PRIVATE :: JSON_VALUE_ADD_LOGICAL
        PROCEDURE,PRIVATE :: JSON_VALUE_ADD_LOGICAL_VEC
        PROCEDURE,PRIVATE :: JSON_VALUE_ADD_STRING
        PROCEDURE,PRIVATE :: JSON_VALUE_ADD_STRING_VEC
!- 361


!>
!  These are like the `add` methods, except if a variable with the
!  same path is already present, then its value is simply updated.
!  Note that currently, these only work for scalar variables.
!  These routines can also change the variable's type (but an error will be
!  thrown if the existing variable is not a scalar).
!
!--- See also
!  * [[json_core(type):add_by_path]] - this one can be used to change
!    arrays and objects to scalars if so desired.
!
!@note Unlike some routines, the `found` output is not optional,
!      so it doesn't present exceptions from being thrown.
!
!@note These have been mostly supplanted by the [[json_core(type):add_by_path]]
!      methods, which do a similar thing (and can be used for
!      scalars and vectors, etc.)
        GENERIC,PUBLIC :: UPDATE => JSON_UPDATE_LOGICAL,&

                                    JSON_UPDATE_REAL32,&

                                    JSON_UPDATE_REAL,&
!- 387


                                    JSON_UPDATE_INTEGER,&
                                    JSON_UPDATE_STRING
!- 394

        PROCEDURE,PRIVATE :: JSON_UPDATE_LOGICAL

        PROCEDURE,PRIVATE :: JSON_UPDATE_REAL32

        PROCEDURE,PRIVATE :: JSON_UPDATE_REAL
!- 402

        PROCEDURE,PRIVATE :: JSON_UPDATE_INTEGER
        PROCEDURE,PRIVATE :: JSON_UPDATE_STRING
!- 408


!>
!  Add variables to a [[json_value]] linked list
!  by specifying their paths.
!
!--- Example
!
!````fortran
!    use, intrinsic :: iso_fortran_env, only: output_unit
!    use json_module, wp=>json_RK
!    type(json_core) :: json
!    type(json_value) :: p
!    call json%create_object(p,'root') ! create the root
!    ! now add some variables using the paths:
!    call json%add_by_path(p,'inputs.t',    0.0_wp  )
!    call json%add_by_path(p,'inputs.x(1)', 100.0_wp)
!    call json%add_by_path(p,'inputs.x(2)', 200.0_wp)
!    call json%print(p)  ! now print to console
!````
!
!--- Notes
!  * This uses [[json_create_by_path]]
!
!--- See also
!  * The `json_core%update` methods.
!  * [[json_create_by_path]]

        GENERIC,PUBLIC :: ADD_BY_PATH => JSON_ADD_MEMBER_BY_PATH,&
                                         JSON_ADD_INTEGER_BY_PATH,&

                                         JSON_ADD_REAL32_BY_PATH,&

                                         JSON_ADD_REAL_BY_PATH,&
!- 444

                                         JSON_ADD_LOGICAL_BY_PATH,&
                                         JSON_ADD_STRING_BY_PATH,&
                                         JSON_ADD_INTEGER_VEC_BY_PATH,&

                                         JSON_ADD_REAL32_VEC_BY_PATH,&

                                         JSON_ADD_REAL_VEC_BY_PATH,&
!- 454

                                         JSON_ADD_LOGICAL_VEC_BY_PATH,&
                                         JSON_ADD_STRING_VEC_BY_PATH
!- 462

        PROCEDURE :: JSON_ADD_MEMBER_BY_PATH
        PROCEDURE :: JSON_ADD_INTEGER_BY_PATH

        PROCEDURE :: JSON_ADD_REAL32_BY_PATH

        PROCEDURE :: JSON_ADD_REAL_BY_PATH
!- 471

        PROCEDURE :: JSON_ADD_LOGICAL_BY_PATH
        PROCEDURE :: JSON_ADD_STRING_BY_PATH
        PROCEDURE :: JSON_ADD_INTEGER_VEC_BY_PATH

        PROCEDURE :: JSON_ADD_REAL32_VEC_BY_PATH

        PROCEDURE :: JSON_ADD_REAL_VEC_BY_PATH
!- 481

        PROCEDURE :: JSON_ADD_LOGICAL_VEC_BY_PATH
        PROCEDURE :: JSON_ADD_STRING_VEC_BY_PATH
!- 489


!>
!  Create a [[json_value]] linked list using the
!  path to the variables. Optionally return a
!  pointer to the variable.
!
!  (This will create a `null` variable)
!
!--- See also
!  * [[json_core(type):add_by_path]]

        GENERIC,PUBLIC :: CREATE => JSON_CREATE_BY_PATH
        PROCEDURE :: JSON_CREATE_BY_PATH

!>
!  Get data from a [[json_value]] linked list.
!
!@note There are two versions (e.g. [[json_get_integer]] and [[json_get_integer_by_path]]).
!      The first one gets the value from the [[json_value]] passed into the routine,
!      while the second one gets the value from the [[json_value]] found by parsing the
!      path.  The path version is split up into unicode and non-unicode versions.

        GENERIC,PUBLIC :: GET => &
                                       JSON_GET_BY_PATH,             &
            JSON_GET_INTEGER,          JSON_GET_INTEGER_BY_PATH,     &
            JSON_GET_INTEGER_VEC,      JSON_GET_INTEGER_VEC_BY_PATH, &

            JSON_GET_REAL32,           JSON_GET_REAL32_BY_PATH,      &
            JSON_GET_REAL32_VEC,       JSON_GET_REAL32_VEC_BY_PATH,  &

            JSON_GET_REAL,             JSON_GET_REAL_BY_PATH,        &
            JSON_GET_REAL_VEC,         JSON_GET_REAL_VEC_BY_PATH,    &
!- 525

            JSON_GET_LOGICAL,          JSON_GET_LOGICAL_BY_PATH,     &
            JSON_GET_LOGICAL_VEC,      JSON_GET_LOGICAL_VEC_BY_PATH, &
            JSON_GET_STRING,           JSON_GET_STRING_BY_PATH,      &
            JSON_GET_STRING_VEC,       JSON_GET_STRING_VEC_BY_PATH,  &
            JSON_GET_ALLOC_STRING_VEC, JSON_GET_ALLOC_STRING_VEC_BY_PATH,&
            JSON_GET_ARRAY,            JSON_GET_ARRAY_BY_PATH

        PROCEDURE,PRIVATE :: JSON_GET_INTEGER
        PROCEDURE,PRIVATE :: JSON_GET_INTEGER_VEC

        PROCEDURE,PRIVATE :: JSON_GET_REAL32
        PROCEDURE,PRIVATE :: JSON_GET_REAL32_VEC

        PROCEDURE,PRIVATE :: JSON_GET_REAL
        PROCEDURE,PRIVATE :: JSON_GET_REAL_VEC
!- 544

        PROCEDURE,PRIVATE :: JSON_GET_LOGICAL
        PROCEDURE,PRIVATE :: JSON_GET_LOGICAL_VEC
        PROCEDURE,PRIVATE :: JSON_GET_STRING
        PROCEDURE,PRIVATE :: JSON_GET_STRING_VEC
        PROCEDURE,PRIVATE :: JSON_GET_ALLOC_STRING_VEC
        PROCEDURE,PRIVATE :: JSON_GET_ARRAY
        PROCEDURE,PRIVATE :: JSON_GET_BY_PATH
        PROCEDURE,PRIVATE :: JSON_GET_INTEGER_BY_PATH
        PROCEDURE,PRIVATE :: JSON_GET_INTEGER_VEC_BY_PATH

        PROCEDURE,PRIVATE :: JSON_GET_REAL32_BY_PATH
        PROCEDURE,PRIVATE :: JSON_GET_REAL32_VEC_BY_PATH

        PROCEDURE,PRIVATE :: JSON_GET_REAL_BY_PATH
        PROCEDURE,PRIVATE :: JSON_GET_REAL_VEC_BY_PATH
!- 563

        PROCEDURE,PRIVATE :: JSON_GET_LOGICAL_BY_PATH
        PROCEDURE,PRIVATE :: JSON_GET_LOGICAL_VEC_BY_PATH
        PROCEDURE,PRIVATE :: JSON_GET_STRING_BY_PATH
        PROCEDURE,PRIVATE :: JSON_GET_STRING_VEC_BY_PATH
        PROCEDURE,PRIVATE :: JSON_GET_ARRAY_BY_PATH
        PROCEDURE,PRIVATE :: JSON_GET_ALLOC_STRING_VEC_BY_PATH
        PROCEDURE,PRIVATE :: JSON_GET_BY_PATH_DEFAULT
        PROCEDURE,PRIVATE :: JSON_GET_BY_PATH_RFC6901
        PROCEDURE,PRIVATE :: JSON_GET_BY_PATH_JSONPATH_BRACKET

!>
!  Print the [[json_value]] to an output unit or file.
!
!--- Example
!
!````fortran
!    type(json_core) :: json
!    type(json_value) :: p
!    !...
!    call json%print(p,'test.json')  !this is [[json_print_to_filename]]
!````
        GENERIC,PUBLIC :: PRINT => JSON_PRINT_TO_CONSOLE,&
                                   JSON_PRINT_TO_UNIT,&
                                   JSON_PRINT_TO_FILENAME
        PROCEDURE :: JSON_PRINT_TO_CONSOLE
        PROCEDURE :: JSON_PRINT_TO_UNIT
        PROCEDURE :: JSON_PRINT_TO_FILENAME

!>
!  Destructor routine for a [[json_value]] pointer.
!  This must be called explicitly if it is no longer needed,
!  before it goes out of scope.  Otherwise, a memory leak will result.
!
!--- Example
!
!  Destroy the [[json_value]] pointer before the variable goes out of scope:
!````fortran
!     subroutine example1()
!     type(json_core) :: json
!     type(json_value),pointer :: p
!     call json%create_object(p,'')
!     call json%add(p,'year',2015)
!     call json%print(p)
!     call json%destroy(p)
!     end subroutine example1
!````
!
!  Note: it should NOT be called for a [[json_value]] pointer than has already been
!  added to another [[json_value]] structure, since doing so may render the
!  other structure invalid.  Consider the following example:
!````fortran
!     subroutine example2(p)
!     type(json_core) :: json
!     type(json_value),pointer,intent(out) :: p
!     type(json_value),pointer :: q
!     call json%create_object(p,'')
!     call json%add(p,'year',2015)
!     call json%create_object(q,'q')
!     call json%add(q,'val',1)
!     call json%add(p, q)  !add q to p structure
!     ! do NOT call json%destroy(q) here, because q is
!     ! now part of the output structure p.  p should be destroyed
!     ! somewhere upstream by the caller of this routine.
!     nullify(q) !OK, but not strictly necessary
!     end subroutine example2
!````
        GENERIC,PUBLIC :: DESTROY => JSON_VALUE_DESTROY,DESTROY_JSON_CORE
        PROCEDURE :: JSON_VALUE_DESTROY
        PROCEDURE :: DESTROY_JSON_CORE

!>
!  If the child variable is present, then remove it.
        GENERIC,PUBLIC :: REMOVE_IF_PRESENT => JSON_VALUE_REMOVE_IF_PRESENT
        PROCEDURE :: JSON_VALUE_REMOVE_IF_PRESENT

!>
!  Allocate a [[json_value]] pointer and make it a real variable.
!  The pointer should not already be allocated.
!
!--- Example
!
!````fortran
!    type(json_core) :: json
!    type(json_value),pointer :: p
!    call json%create_real(p,'value',1.0_RK)
!````
!
!--- Note
!  * [[json_core(type):create_real]] is just an alias
!    to this one for backward compatibility.
        GENERIC,PUBLIC :: CREATE_REAL => JSON_VALUE_CREATE_REAL
        PROCEDURE :: JSON_VALUE_CREATE_REAL

        GENERIC,PUBLIC :: CREATE_REAL => JSON_VALUE_CREATE_REAL32
        PROCEDURE :: JSON_VALUE_CREATE_REAL32

!- 663


!>
!  This is equivalent to [[json_core(type):create_real]],
!  and is here only for backward compatibility.
        GENERIC,PUBLIC :: CREATE_DOUBLE => JSON_VALUE_CREATE_REAL

        GENERIC,PUBLIC :: CREATE_DOUBLE => JSON_VALUE_CREATE_REAL32

!- 674


!>
!  Allocate a [[json_value]] pointer and make it an array variable.
!  The pointer should not already be allocated.
!
!--- Example
!
!````fortran
!    type(json_core) :: json
!    type(json_value),pointer :: p
!    call json%create_array(p,'arrayname')
!````
        GENERIC,PUBLIC :: CREATE_ARRAY => JSON_VALUE_CREATE_ARRAY
        PROCEDURE :: JSON_VALUE_CREATE_ARRAY

!>
!  Allocate a [[json_value]] pointer and make it an object variable.
!  The pointer should not already be allocated.
!
!--- Example
!
!````fortran
!    type(json_core) :: json
!    type(json_value),pointer :: p
!    call json%create_object(p,'objectname')
!````
!
!@note The name is not significant for the root structure or an array element.
!      In those cases, an empty string can be used.
        GENERIC,PUBLIC :: CREATE_OBJECT => JSON_VALUE_CREATE_OBJECT
        PROCEDURE :: JSON_VALUE_CREATE_OBJECT

!>
!  Allocate a json_value pointer and make it a null variable.
!  The pointer should not already be allocated.
!
!--- Example
!
!````fortran
!    type(json_core) :: json
!    type(json_value),pointer :: p
!    call json%create_null(p,'value')
!````
        GENERIC,PUBLIC :: CREATE_NULL => JSON_VALUE_CREATE_NULL
        PROCEDURE :: JSON_VALUE_CREATE_NULL

!>
!  Allocate a json_value pointer and make it a string variable.
!  The pointer should not already be allocated.
!
!--- Example
!
!````fortran
!    type(json_core) :: json
!    type(json_value),pointer :: p
!    call json%create_string(p,'value','foobar')
!````
        GENERIC,PUBLIC :: CREATE_STRING => JSON_VALUE_CREATE_STRING
        PROCEDURE :: JSON_VALUE_CREATE_STRING

!>
!  Allocate a json_value pointer and make it an integer variable.
!  The pointer should not already be allocated.
!
!--- Example
!
!````fortran
!    type(json_core) :: json
!    type(json_value),pointer :: p
!    call json%create_integer(p,42,'value')
!````
        GENERIC,PUBLIC :: CREATE_INTEGER => JSON_VALUE_CREATE_INTEGER
        PROCEDURE :: JSON_VALUE_CREATE_INTEGER

!>
!  Allocate a json_value pointer and make it a logical variable.
!  The pointer should not already be allocated.
!
!--- Example
!
!````fortran
!    type(json_core) :: json
!    type(json_value),pointer :: p
!    call json%create_logical(p,'value',.true.)
!````
        GENERIC,PUBLIC :: CREATE_LOGICAL => JSON_VALUE_CREATE_LOGICAL
        PROCEDURE :: JSON_VALUE_CREATE_LOGICAL

!>
!  Parse the JSON file and populate the [[json_value]] tree.
        GENERIC,PUBLIC :: LOAD => JSON_PARSE_FILE
        PROCEDURE :: JSON_PARSE_FILE

!>
!  Print the [[json_value]] structure to an allocatable string
        PROCEDURE,PUBLIC :: SERIALIZE => JSON_VALUE_TO_STRING

!>
!  The same as `serialize`, but only here for backward compatibility
        PROCEDURE,PUBLIC :: PRINT_TO_STRING => JSON_VALUE_TO_STRING

!>
!  Parse the JSON string and populate the [[json_value]] tree.
        GENERIC,PUBLIC :: DESERIALIZE => JSON_PARSE_STRING
        PROCEDURE :: JSON_PARSE_STRING

!>
!  Same as `load` and `deserialize` but only here for backward compatibility.
        GENERIC,PUBLIC :: PARSE => JSON_PARSE_FILE, &
                                   JSON_PARSE_STRING

!>
!  Throw an exception.
        GENERIC,PUBLIC :: THROW_EXCEPTION => JSON_THROW_EXCEPTION
        PROCEDURE :: JSON_THROW_EXCEPTION

!>
!  Rename a [[json_value]] variable.
        GENERIC,PUBLIC :: RENAME => JSON_VALUE_RENAME,&
                                    JSON_RENAME_BY_PATH
        PROCEDURE :: JSON_VALUE_RENAME
        PROCEDURE :: JSON_RENAME_BY_PATH
!- 802


!>
!  get info about a [[json_value]]
        GENERIC,PUBLIC :: INFO => JSON_INFO, JSON_INFO_BY_PATH
        PROCEDURE :: JSON_INFO
        PROCEDURE :: JSON_INFO_BY_PATH

!>
!  get string info about a [[json_value]]
        GENERIC,PUBLIC :: STRING_INFO => JSON_STRING_INFO
        PROCEDURE :: JSON_STRING_INFO

!>
!  get matrix info about a [[json_value]]
        GENERIC,PUBLIC :: MATRIX_INFO => JSON_MATRIX_INFO, JSON_MATRIX_INFO_BY_PATH
        PROCEDURE :: JSON_MATRIX_INFO
        PROCEDURE :: JSON_MATRIX_INFO_BY_PATH

!>
!  insert a new element after an existing one,
!  updating the JSON structure accordingly
        GENERIC,PUBLIC :: INSERT_AFTER => JSON_VALUE_INSERT_AFTER, &
                                          JSON_VALUE_INSERT_AFTER_CHILD_BY_INDEX
        PROCEDURE :: JSON_VALUE_INSERT_AFTER
        PROCEDURE :: JSON_VALUE_INSERT_AFTER_CHILD_BY_INDEX

!>
!  get the path to a JSON variable in a structure:
        GENERIC,PUBLIC :: GET_PATH => JSON_GET_PATH
        PROCEDURE :: JSON_GET_PATH

!>
!  verify if a path is valid
!  (i.e., a variable with this path exists in the file).
        GENERIC,PUBLIC :: VALID_PATH => JSON_VALID_PATH
        PROCEDURE :: JSON_VALID_PATH

        PROCEDURE,PUBLIC :: REMOVE              => JSON_VALUE_REMOVE        !! Remove a [[json_value]] from a
!! linked-list structure.
        PROCEDURE,PUBLIC :: REPLACE             => JSON_VALUE_REPLACE       !! Replace a [[json_value]] in a
!! linked-list structure.
        PROCEDURE,PUBLIC :: REVERSE             => JSON_VALUE_REVERSE       !! Reverse the order of the childrEN
!! of an array of object.
        PROCEDURE,PUBLIC :: CHECK_FOR_ERRORS    => JSON_CHECK_FOR_ERRORS    !! check for error and get error mESSAGE
        PROCEDURE,PUBLIC :: CLEAR_EXCEPTIONS    => JSON_CLEAR_EXCEPTIONS    !! clear exceptions
        PROCEDURE,PUBLIC :: COUNT               => JSON_COUNT               !! count the number of children
        PROCEDURE,PUBLIC :: CLONE               => JSON_CLONE               !! clone a JSON structure (deep coPY)
        PROCEDURE,PUBLIC :: FAILED              => JSON_FAILED              !! check for error
        PROCEDURE,PUBLIC :: GET_PARENT          => JSON_GET_PARENT          !! get pointer to json_value parent
        PROCEDURE,PUBLIC :: GET_NEXT            => JSON_GET_NEXT            !! get pointer to json_value next
        PROCEDURE,PUBLIC :: GET_PREVIOUS        => JSON_GET_PREVIOUS        !! get pointer to json_value previOUS
        PROCEDURE,PUBLIC :: GET_TAIL            => JSON_GET_TAIL            !! get pointer to json_value tail
        PROCEDURE,PUBLIC :: INITIALIZE          => JSON_INITIALIZE          !! to initialize some parsing paraMETERS
        PROCEDURE,PUBLIC :: TRAVERSE            => JSON_TRAVERSE            !! to traverse all elements of a JSON
!! structure
        PROCEDURE,PUBLIC :: PRINT_ERROR_MESSAGE => JSON_PRINT_ERROR_MESSAGE !! simply routine to print error
!! messages
        PROCEDURE,PUBLIC :: SWAP                => JSON_VALUE_SWAP          !! Swap two [[json_value]] pointers
!! in a structure (or two different
!! structures).
        PROCEDURE,PUBLIC :: IS_CHILD_OF         => JSON_VALUE_IS_CHILD_OF   !! Check if a [[json_value]] is a
!! descendant of another.
        PROCEDURE,PUBLIC :: VALIDATE            => JSON_VALUE_VALIDATE      !! Check that a [[json_value]] linKED
!! list is valid (i.e., is properly
!! constructed). This may be useful
!! if it has been constructed externally.
        PROCEDURE,PUBLIC :: CHECK_FOR_DUPLICATE_KEYS &
                                => JSON_CHECK_ALL_FOR_DUPLICATE_KEYS  !! Check entire JSON structure
!! for duplicate keys (recursively)
        PROCEDURE,PUBLIC :: CHECK_CHILDREN_FOR_DUPLICATE_KEYS &
                                => JSON_CHECK_CHILDREN_FOR_DUPLICATE_KEYS  !! Check a `json_value` object's
!! children for duplicate keys

!other private routines:
        PROCEDURE        :: NAME_EQUAL
        PROCEDURE        :: NAME_STRINGS_EQUAL
        PROCEDURE        :: JSON_VALUE_PRINT
        PROCEDURE        :: STRING_TO_INT
        PROCEDURE        :: STRING_TO_DBLE
        PROCEDURE        :: PREPARE_PARSER => JSON_PREPARE_PARSER
        PROCEDURE        :: PARSE_END => JSON_PARSE_END
        PROCEDURE        :: PARSE_VALUE
        PROCEDURE        :: PARSE_NUMBER
        PROCEDURE        :: PARSE_STRING
        PROCEDURE        :: PARSE_FOR_CHARS
        PROCEDURE        :: PARSE_OBJECT
        PROCEDURE        :: PARSE_ARRAY
        PROCEDURE        :: ANNOTATE_INVALID_JSON
        PROCEDURE        :: POP_CHAR
        PROCEDURE        :: PUSH_CHAR
        PROCEDURE        :: GET_CURRENT_LINE_FROM_FILE_STREAM
        PROCEDURE,NOPASS :: GET_CURRENT_LINE_FROM_FILE_SEQUENTIAL
        PROCEDURE        :: CONVERT
        PROCEDURE        :: TO_STRING
        PROCEDURE        :: TO_LOGICAL
        PROCEDURE        :: TO_INTEGER
        PROCEDURE        :: TO_REAL
        PROCEDURE        :: TO_NULL
        PROCEDURE        :: TO_OBJECT
        PROCEDURE        :: TO_ARRAY
        PROCEDURE,NOPASS :: JSON_VALUE_CLONE_FUNC
        PROCEDURE        :: IS_VECTOR => JSON_IS_VECTOR

    END TYPE JSON_CORE
!*********************************************************

!*********************************************************
!>
!  Structure constructor to initialize a
!  [[json_core(type)]] object
!
!--- Example
!
!```fortran
! type(json_file)  :: json_core
! json_core = json_core()
!```
    INTERFACE JSON_CORE
       MODULE PROCEDURE INITIALIZE_JSON_CORE
    END INTERFACE
!*********************************************************

!*************************************************************************************
    ABSTRACT INTERFACE

        SUBROUTINE JSON_ARRAY_CALLBACK_FUNC(JSON, ELEMENT, I, COUNT)
!! Array element callback function.  Used by [[json_get_array]]
            IMPORT :: JSON_VALUE,JSON_CORE,IK
            IMPLICIT NONE
            CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
            TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: ELEMENT
            INTEGER(IK),INTENT(IN)               :: I        !! index
            INTEGER(IK),INTENT(IN)               :: COUNT    !! size of array
        END SUBROUTINE JSON_ARRAY_CALLBACK_FUNC

        SUBROUTINE JSON_TRAVERSE_CALLBACK_FUNC(JSON,P,FINISHED)
!! Callback function used by [[json_traverse]]
            IMPORT :: JSON_VALUE,JSON_CORE,LK
            IMPLICIT NONE
            CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
            TYPE(JSON_VALUE),POINTER,INTENT(IN) :: P
            LOGICAL(LK),INTENT(OUT)             :: FINISHED  !! set true to stop traversing
        END SUBROUTINE JSON_TRAVERSE_CALLBACK_FUNC

    END INTERFACE
    PUBLIC :: JSON_ARRAY_CALLBACK_FUNC
    PUBLIC :: JSON_TRAVERSE_CALLBACK_FUNC
!*************************************************************************************

    CONTAINS
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/17/2016
!
!  Destructor for the [[json_core(type)]] type.

    SUBROUTINE DESTROY_JSON_CORE(ME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(OUT) :: ME

    END SUBROUTINE DESTROY_JSON_CORE
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/26/2016
!
!  Function constructor for a [[json_core(type)]].
!  This is just a wrapper for [[json_initialize]].
!
!@note [[initialize_json_core]], [[json_initialize]],
!      [[initialize_json_core_in_file]], and [[initialize_json_file]]
!      all have a similar interface.

    FUNCTION INITIALIZE_JSON_CORE(&
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
!- 983 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_value_module.F90" 2
                                 ) RESULT(JSON_CORE_OBJECT)

    IMPLICIT NONE

    TYPE(JSON_CORE) :: JSON_CORE_OBJECT
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
!- 989 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_value_module.F90" 2

    CALL JSON_CORE_OBJECT%INITIALIZE(&
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
!- 992 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_value_module.F90" 2
                                    )

    END FUNCTION INITIALIZE_JSON_CORE
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/4/2013
!
!  Initialize the [[json_core(type)]] instance.
!
!  The routine may be called before any of the [[json_core(type)]] methods are used in
!  order to specify certain parameters. If it is not called, then the defaults
!  are used. This routine is also called internally by various routines.
!  It can also be called to clear exceptions, or to reset some
!  of the variables (note that only the arguments present are changed).
!
!--- Modified
!  * Izaak Beekman : 02/24/2015
!
!@note [[initialize_json_core]], [[json_initialize]],
!      [[initialize_json_core_in_file]], and [[initialize_json_file]]
!      all have a similar interface.

    SUBROUTINE JSON_INITIALIZE(ME,&
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
!- 1018 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_value_module.F90" 2
                              )

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)  :: ME
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
!- 1024 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_value_module.F90" 2

    CHARACTER(KIND=CDK,LEN=10) :: W            !! max string length
    CHARACTER(KIND=CDK,LEN=10) :: D            !! real precision digits
    CHARACTER(KIND=CDK,LEN=10) :: E            !! real exponent digits
    CHARACTER(KIND=CDK,LEN=2)  :: SGN          !! sign flag: `ss` or `sp`
    CHARACTER(KIND=CDK,LEN=2)  :: RL_EDIT_DESC !! `G`, `E`, `EN`, or `ES`
    INTEGER(IK)                :: ISTAT        !! `iostat` flag for
!! write statements
    LOGICAL(LK)                :: SGN_PRNT     !! print sign flag
    CHARACTER(KIND=CK,LEN=MAX_INTEGER_STR_LEN) :: ISTR !! for integer to
!! string conversion

!reset exception to false:
    CALL ME%CLEAR_EXCEPTIONS()

!Just in case, clear these global variables also:
    ME%PUSHED_INDEX = 0
    ME%PUSHED_CHAR  = CK_''
    ME%CHAR_COUNT   = 0
    ME%LINE_COUNT   = 1
    ME%IPOS         = 1
    IF (USE_UNFORMATTED_STREAM) THEN
        ME%FILESIZE = 0
        ME%ICHUNK   = 0
        ME%CHUNK    = REPEAT(SPACE, STREAM_CHUNK_SIZE) ! default chunk size
    END IF

!- 1055


!various optional inputs:
    IF (PRESENT(SPACES_PER_TAB)) &
        ME%SPACES_PER_TAB = SPACES_PER_TAB
    IF (PRESENT(STOP_ON_ERROR)) &
        ME%STOP_ON_ERROR = STOP_ON_ERROR
    IF (PRESENT(VERBOSE)) &
        ME%IS_VERBOSE = VERBOSE
    IF (PRESENT(STRICT_TYPE_CHECKING)) &
        ME%STRICT_TYPE_CHECKING = STRICT_TYPE_CHECKING
    IF (PRESENT(TRAILING_SPACES_SIGNIFICANT)) &
        ME%TRAILING_SPACES_SIGNIFICANT = TRAILING_SPACES_SIGNIFICANT
    IF (PRESENT(CASE_SENSITIVE_KEYS)) &
        ME%CASE_SENSITIVE_KEYS = CASE_SENSITIVE_KEYS
    IF (PRESENT(NO_WHITESPACE)) &
        ME%NO_WHITESPACE = NO_WHITESPACE
    IF (PRESENT(UNESCAPE_STRINGS)) &
        ME%UNESCAPED_STRINGS = UNESCAPE_STRINGS
    IF (PRESENT(PATH_MODE)) THEN
        IF (PATH_MODE==1_IK .OR. PATH_MODE==2_IK .OR. PATH_MODE==3_IK) THEN
            ME%PATH_MODE = PATH_MODE
        ELSE
            ME%PATH_MODE = 1_IK  ! just to have a valid value
            CALL ME%THROW_EXCEPTION('Invalid path_mode.')
        END IF
    END IF

! if we are allowing comments in the file:
! [an empty string disables comments]
    IF (PRESENT(COMMENT_CHAR)) THEN
        ME%ALLOW_COMMENTS = COMMENT_CHAR/=CK_''
        ME%COMMENT_CHAR = TRIM(ADJUSTL(COMMENT_CHAR))
    END IF

! path separator:
    IF (PRESENT(PATH_SEPARATOR)) THEN
        ME%PATH_SEPARATOR = PATH_SEPARATOR
    END IF

! printing vectors in compressed form:
    IF (PRESENT(COMPRESS_VECTORS)) THEN
        ME%COMPRESS_VECTORS = COMPRESS_VECTORS
    END IF

! checking for duplicate keys:
    IF (PRESENT(ALLOW_DUPLICATE_KEYS)) THEN
        ME%ALLOW_DUPLICATE_KEYS = ALLOW_DUPLICATE_KEYS
    END IF

! if escaping the forward slash:
    IF (PRESENT(ESCAPE_SOLIDUS)) THEN
        ME%ESCAPE_SOLIDUS = ESCAPE_SOLIDUS
    END IF

! how to handle null to real conversions:
    IF (PRESENT(NULL_TO_REAL_MODE)) THEN
        SELECT CASE (NULL_TO_REAL_MODE)
        CASE(1_IK:3_IK)
            ME%NULL_TO_REAL_MODE = NULL_TO_REAL_MODE
        CASE DEFAULT
            ME%NULL_TO_REAL_MODE = 2_IK  ! just to have a valid value
            CALL INTEGER_TO_STRING(NULL_TO_REAL_MODE,INT_FMT,ISTR)
            CALL ME%THROW_EXCEPTION('Invalid null_to_real_mode: '//ISTR)
        END SELECT
    END IF

! how to handle NaN and Infinities:
    IF (PRESENT(NON_NORMAL_MODE)) THEN
        SELECT CASE (NON_NORMAL_MODE)
        CASE(1_IK) ! use strings
            ME%NON_NORMALS_TO_NULL = .FALSE.
        CASE(2_IK) ! use null
            ME%NON_NORMALS_TO_NULL = .TRUE.
        CASE DEFAULT
            CALL INTEGER_TO_STRING(NON_NORMAL_MODE,INT_FMT,ISTR)
            CALL ME%THROW_EXCEPTION('Invalid non_normal_mode: '//ISTR)
        END SELECT
    END IF

    IF (PRESENT(USE_QUIET_NAN)) THEN
        ME%USE_QUIET_NAN = USE_QUIET_NAN
    END IF

    IF (PRESENT(STRICT_INTEGER_TYPE_CHECKING)) THEN
        ME%STRICT_INTEGER_TYPE_CHECKING = STRICT_INTEGER_TYPE_CHECKING
    END IF

!Set the format for real numbers:
! [if not changing it, then it remains the same]

    IF ( (.NOT. ALLOCATED(ME%REAL_FMT)) .OR. &  ! if this hasn't been done yet
          PRESENT(COMPACT_REALS) .OR. &
          PRESENT(PRINT_SIGNS)   .OR. &
          PRESENT(REAL_FORMAT) ) THEN

!allow the special case where real format is '*':
! [this overrides the other options]
        IF (PRESENT(REAL_FORMAT)) THEN
            IF (REAL_FORMAT==STAR) THEN
                IF (PRESENT(COMPACT_REALS)) THEN
! we will also allow for compact reals with
! '*' format, if both arguments are present.
                    ME%COMPACT_REAL = COMPACT_REALS
                ELSE
                    ME%COMPACT_REAL = .FALSE.
                END IF
                ME%REAL_FMT = STAR
                RETURN
            END IF
        END IF

        IF (PRESENT(COMPACT_REALS)) ME%COMPACT_REAL = COMPACT_REALS

!set defaults
        SGN_PRNT = .FALSE.
        IF ( PRESENT( PRINT_SIGNS) ) SGN_PRNT = PRINT_SIGNS
        IF ( SGN_PRNT ) THEN
           SGN = 'sp'
        ELSE
           SGN = 'ss'
        END IF

        RL_EDIT_DESC = 'E'
        IF ( PRESENT( REAL_FORMAT ) ) THEN
           SELECT CASE ( REAL_FORMAT )
           CASE ('g','G','e','E','en','EN','es','ES')
              RL_EDIT_DESC = REAL_FORMAT
           CASE DEFAULT
              CALL ME%THROW_EXCEPTION('Invalid real format, "' // &
                        TRIM(REAL_FORMAT) // '", passed to json_initialize.'// &
                        NEW_LINE('a') // 'Acceptable formats are: "G", "E", "EN", and "ES".' )
           END SELECT
        END IF

! set the default output/input format for reals:
                      WRITE(W,'(ss,I0)',IOSTAT=ISTAT) MAX_NUMERIC_STR_LEN
        IF (ISTAT==0) WRITE(D,'(ss,I0)',IOSTAT=ISTAT) REAL_PRECISION
        IF (ISTAT==0) WRITE(E,'(ss,I0)',IOSTAT=ISTAT) REAL_EXPONENT_DIGITS
        IF (ISTAT==0) THEN
            ME%REAL_FMT = '(' // SGN // ',' // TRIM(RL_EDIT_DESC) //&
                            TRIM(W) // '.' // TRIM(D) // 'E' // TRIM(E) // ')'
        ELSE
            ME%REAL_FMT = '(' // SGN // ',' // TRIM(RL_EDIT_DESC) // &
                            '27.17E4)'  !just use this one (should never happen)
        END IF

    END IF

    END SUBROUTINE JSON_INITIALIZE
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Returns true if `name` is equal to `p%name`, using the specified
!  settings for case sensitivity and trailing whitespace.
!
!--- History
!  * 4/30/2016 : original version
!  * 8/25/2017 : now just a wrapper for [[name_strings_equal]]

    FUNCTION NAME_EQUAL(JSON,P,NAME) RESULT(IS_EQUAL)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),INTENT(IN)         :: P        !! the json object
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: NAME     !! the name to check for
    LOGICAL(LK)                         :: IS_EQUAL !! true if the string are
!! lexically equal

    IF (ALLOCATED(P%NAME)) THEN
! call the low-level routines for the name strings:
        IS_EQUAL = JSON%NAME_STRINGS_EQUAL(P%NAME,NAME)
    ELSE
        IS_EQUAL = NAME == CK_'' ! check a blank name
    END IF

    END FUNCTION NAME_EQUAL
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 8/25/2017
!
!  Returns true if the name strings `name1` is equal to `name2`, using
!  the specified settings for case sensitivity and trailing whitespace.

    FUNCTION NAME_STRINGS_EQUAL(JSON,NAME1,NAME2) RESULT(IS_EQUAL)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: NAME1     !! the name to check
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: NAME2     !! the name to check
    LOGICAL(LK)                         :: IS_EQUAL  !! true if the string are
!! lexically equal

!must be the same length if we are treating
!trailing spaces as significant, so do a
!quick test of this first:
    IF (JSON%TRAILING_SPACES_SIGNIFICANT) THEN
        IS_EQUAL = LEN(NAME1) == LEN(NAME2)
        IF (.NOT. IS_EQUAL) RETURN
    END IF

    IF (JSON%CASE_SENSITIVE_KEYS) THEN
        IS_EQUAL = NAME1 == NAME2
    ELSE
        IS_EQUAL = LOWERCASE_STRING(NAME1) == LOWERCASE_STRING(NAME2)
    END IF

    END FUNCTION NAME_STRINGS_EQUAL
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 10/31/2015
!
!  Create a deep copy of a [[json_value]] linked-list structure.
!
!--- Notes
!
!  * If `from` has children, then they are also cloned.
!  * The parent of `from` is not linked to `to`.
!  * If `from` is an element of an array, then the previous and
!    next entries are not cloned (only that element and it's children, if any).
!
!--- Example
!
!````fortran
!    program test
!     use json_module
!     implicit none
!     type(json_core) :: json
!     type(json_value),pointer :: j1, j2
!     call json%load('files/inputs/test1.json',j1)
!     call json%clone(j1,j2) !now have two independent copies
!     call json%destroy(j1)  !destroys j1, but j2 remains
!     call json%print(j2,'j2.json')
!     call json%destroy(j2)
!    end program test
!````

    SUBROUTINE JSON_CLONE(JSON,FROM,TO)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT) :: JSON
    TYPE(JSON_VALUE),POINTER :: FROM  !! this is the structure to clone
    TYPE(JSON_VALUE),POINTER :: TO    !! the clone is put here
!! (it must not already be associated)

!call the main function:
    CALL JSON%JSON_VALUE_CLONE_FUNC(FROM,TO)

    END SUBROUTINE JSON_CLONE
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 10/31/2015
!
!  Recursive deep copy function called by [[json_clone]].
!
!@note If new data is added to the [[json_value]] type,
!      then this would need to be updated.

    RECURSIVE SUBROUTINE JSON_VALUE_CLONE_FUNC(FROM,TO,PARENT,PREVIOUS,TAIL)

    IMPLICIT NONE

    TYPE(JSON_VALUE),POINTER          :: FROM     !! this is the structure to clone
    TYPE(JSON_VALUE),POINTER          :: TO       !! the clone is put here (it
!! must not already be associated)
    TYPE(JSON_VALUE),POINTER,OPTIONAL :: PARENT   !! to%parent
    TYPE(JSON_VALUE),POINTER,OPTIONAL :: PREVIOUS !! to%previous
    LOGICAL,OPTIONAL                  :: TAIL     !! if "to" is the tail of
!! its parent's children

    NULLIFY(TO)

    IF (ASSOCIATED(FROM)) THEN

        ALLOCATE(TO)

!copy over the data variables:
! [note: the allocate() statements don't work here for the
!  deferred-length characters in gfortran-4.9]
        IF (ALLOCATED(FROM%NAME))      TO%NAME = FROM%NAME
        IF (ALLOCATED(FROM%DBL_VALUE)) ALLOCATE(TO%DBL_VALUE,SOURCE=FROM%DBL_VALUE)
        IF (ALLOCATED(FROM%LOG_VALUE)) ALLOCATE(TO%LOG_VALUE,SOURCE=FROM%LOG_VALUE)
        IF (ALLOCATED(FROM%STR_VALUE)) TO%STR_VALUE = FROM%STR_VALUE
        IF (ALLOCATED(FROM%INT_VALUE)) ALLOCATE(TO%INT_VALUE,SOURCE=FROM%INT_VALUE)
        TO%VAR_TYPE   = FROM%VAR_TYPE
        TO%N_CHILDREN = FROM%N_CHILDREN

! allocate and associate the pointers as necessary:
        IF (PRESENT(PARENT))   TO%PARENT   => PARENT
        IF (PRESENT(PREVIOUS)) TO%PREVIOUS => PREVIOUS
        IF (PRESENT(TAIL)) THEN
            IF (TAIL .AND. ASSOCIATED(TO%PARENT)) TO%PARENT%TAIL => TO
        END IF

        IF (ASSOCIATED(FROM%NEXT) .AND. ASSOCIATED(TO%PARENT)) THEN
! we only clone the next entry in an array
! if the parent has also been cloned
            CALL JSON_VALUE_CLONE_FUNC(FROM     = FROM%NEXT,&
                                       TO       = TO%NEXT,&
                                       PREVIOUS = TO,&
                                       PARENT   = TO%PARENT,&
                                       TAIL     = (.NOT. ASSOCIATED(FROM%NEXT%NEXT)))
        END IF

        IF (ASSOCIATED(FROM%CHILDREN)) THEN
            CALL JSON_VALUE_CLONE_FUNC(FROM   = FROM%CHILDREN,&
                                       TO     = TO%CHILDREN,&
                                       PARENT = TO,&
                                       TAIL   = (.NOT. ASSOCIATED(FROM%CHILDREN%NEXT)))
        END IF

    END IF

    END SUBROUTINE JSON_VALUE_CLONE_FUNC
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Destroy the data within a [[json_value]], and reset type to `json_unknown`.

    PURE SUBROUTINE DESTROY_JSON_DATA(D)

    IMPLICIT NONE

    TYPE(JSON_VALUE),INTENT(INOUT) :: D

    D%VAR_TYPE = JSON_UNKNOWN

    IF (ALLOCATED(D%LOG_VALUE)) DEALLOCATE(D%LOG_VALUE)
    IF (ALLOCATED(D%INT_VALUE)) DEALLOCATE(D%INT_VALUE)
    IF (ALLOCATED(D%DBL_VALUE)) DEALLOCATE(D%DBL_VALUE)
    IF (ALLOCATED(D%STR_VALUE)) DEALLOCATE(D%STR_VALUE)

    END SUBROUTINE DESTROY_JSON_DATA
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 2/13/2014
!
!  Returns information about a [[json_value]].

    SUBROUTINE JSON_INFO(JSON,P,VAR_TYPE,N_CHILDREN,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)   :: JSON
    TYPE(JSON_VALUE),POINTER         :: P
    INTEGER(IK),INTENT(OUT),OPTIONAL :: VAR_TYPE   !! variable type
    INTEGER(IK),INTENT(OUT),OPTIONAL :: N_CHILDREN !! number of children
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT),OPTIONAL :: NAME !! variable name

    IF (.NOT. JSON%EXCEPTION_THROWN .AND. ASSOCIATED(P)) THEN

        IF (PRESENT(VAR_TYPE))    VAR_TYPE   = P%VAR_TYPE
        IF (PRESENT(N_CHILDREN))  N_CHILDREN = JSON%COUNT(P)
        IF (PRESENT(NAME)) THEN
            IF (ALLOCATED(P%NAME)) THEN
                NAME = P%NAME
            ELSE
                NAME = CK_''
            END IF
        END IF

    ELSE ! error

        IF (.NOT. JSON%EXCEPTION_THROWN) THEN
            CALL JSON%THROW_EXCEPTION('Error in json_info: '//&
                                      'pointer is not associated.' )
        END IF
        IF (PRESENT(VAR_TYPE))   VAR_TYPE   = JSON_UNKNOWN
        IF (PRESENT(N_CHILDREN)) N_CHILDREN = 0
        IF (PRESENT(NAME))       NAME       = CK_''

    END IF

    END SUBROUTINE JSON_INFO
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/18/2016
!
!  Returns information about character strings returned from a [[json_value]].

    SUBROUTINE JSON_STRING_INFO(JSON,P,ILEN,MAX_STR_LEN,FOUND)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)   :: JSON
    TYPE(JSON_VALUE),POINTER         :: P
    INTEGER(IK),DIMENSION(:),ALLOCATABLE,INTENT(OUT),OPTIONAL :: ILEN !! if `p` is an array, this
!! is the actual length
!! of each character
!! string in the array.
!! if not an array, this
!! is returned unallocated.
    INTEGER(IK),INTENT(OUT),OPTIONAL :: MAX_STR_LEN !! The maximum length required to
!! hold the string representation returned
!! by a call to a `get` routine. If a scalar,
!! this is just the length of the scalar. If
!! a vector, this is the maximum length of
!! any element.
    LOGICAL(LK),INTENT(OUT),OPTIONAL :: FOUND   !! true if there were no errors.
!! if not present, an error will
!! throw an exception

    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: CVAL !! for getting values as strings.
    LOGICAL(LK) :: INITIALIZED !! if the output array has been sized
    LOGICAL(LK) :: GET_MAX_LEN !! if we are returning the `max_str_len`
    LOGICAL(LK) :: GET_ILEN    !! if we are returning the `ilen` array
    INTEGER(IK) :: VAR_TYPE    !! variable type

    GET_MAX_LEN = PRESENT(MAX_STR_LEN)
    GET_ILEN    = PRESENT(ILEN)

    IF (.NOT. JSON%EXCEPTION_THROWN) THEN

        IF (PRESENT(FOUND)) FOUND = .TRUE.
        INITIALIZED = .FALSE.

        IF (GET_MAX_LEN) MAX_STR_LEN = 0

        SELECT CASE (P%VAR_TYPE)

        CASE (JSON_ARRAY) ! it's an array

! call routine for each element
            CALL JSON%GET(P, ARRAY_CALLBACK=GET_STRING_LENGTHS)

        CASE DEFAULT ! not an array

            IF (JSON%STRICT_TYPE_CHECKING) THEN
! only allowing strings to be returned
! as strings, so we can check size directly
                CALL JSON%INFO(P,VAR_TYPE=VAR_TYPE)
                IF (VAR_TYPE==JSON_STRING) THEN
                    IF (ALLOCATED(P%STR_VALUE) .AND. GET_MAX_LEN) &
                        MAX_STR_LEN = LEN(P%STR_VALUE)
                ELSE
! it isn't a string, so there is no length
                    CALL JSON%THROW_EXCEPTION('Error in json_string_info: '//&
                                              'When strict_type_checking is true '//&
                                              'the variable must be a character string.',&
                                              FOUND)
                END IF
            ELSE
! in this case, we have to get the value
! as a string to know what size it is.
                CALL JSON%GET(P, VALUE=CVAL)
                IF (.NOT. JSON%EXCEPTION_THROWN) THEN
                    IF (ALLOCATED(CVAL) .AND. GET_MAX_LEN) &
                        MAX_STR_LEN = LEN(CVAL)
                END IF
            END IF

        END SELECT

    END IF

    IF (JSON%EXCEPTION_THROWN) THEN
        IF (PRESENT(FOUND)) THEN
            CALL JSON%CLEAR_EXCEPTIONS()
            FOUND = .FALSE.
        END IF
        IF (GET_MAX_LEN) MAX_STR_LEN = 0
        IF (GET_ILEN) THEN
            IF (ALLOCATED(ILEN)) DEALLOCATE(ILEN)
        END IF
    END IF

    CONTAINS

        SUBROUTINE GET_STRING_LENGTHS(JSON, ELEMENT, I, COUNT)

!! callback function to call for each element in the array.

        IMPLICIT NONE

        CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
        TYPE(JSON_VALUE),POINTER,INTENT(IN) :: ELEMENT
        INTEGER(IK),INTENT(IN)              :: I        !! index
        INTEGER(IK),INTENT(IN)              :: COUNT    !! size of array

        CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: CVAL
        INTEGER(IK) :: VAR_TYPE

        IF (JSON%EXCEPTION_THROWN) RETURN

        IF (.NOT. INITIALIZED) THEN
            IF (GET_ILEN) ALLOCATE(ILEN(COUNT))
            INITIALIZED = .TRUE.
        END IF

        IF (JSON%STRICT_TYPE_CHECKING) THEN
! only allowing strings to be returned
! as strings, so we can check size directly
            CALL JSON%INFO(ELEMENT,VAR_TYPE=VAR_TYPE)
            IF (VAR_TYPE==JSON_STRING) THEN
                IF (ALLOCATED(ELEMENT%STR_VALUE)) THEN
                    IF (GET_MAX_LEN) THEN
                        IF (LEN(ELEMENT%STR_VALUE)>MAX_STR_LEN) &
                                MAX_STR_LEN = LEN(ELEMENT%STR_VALUE)
                    END IF
                    IF (GET_ILEN) ILEN(I) = LEN(ELEMENT%STR_VALUE)
                ELSE
                    IF (GET_ILEN) ILEN(I) = 0
                END IF
            ELSE
! it isn't a string, so there is no length
                CALL JSON%THROW_EXCEPTION('Error in json_string_info: '//&
                                          'When strict_type_checking is true '//&
                                          'the array must contain only '//&
                                          'character strings.',FOUND)
            END IF
        ELSE
! in this case, we have to get the value
! as a string to know what size it is.
            CALL JSON%GET(ELEMENT, VALUE=CVAL)
            IF (JSON%EXCEPTION_THROWN) RETURN
            IF (ALLOCATED(CVAL)) THEN
                IF (GET_MAX_LEN) THEN
                    IF (LEN(CVAL)>MAX_STR_LEN) MAX_STR_LEN = LEN(CVAL)
                END IF
                IF (GET_ILEN) ILEN(I) = LEN(CVAL)
            ELSE
                IF (GET_ILEN) ILEN(I) = 0
            END IF
        END IF

        END SUBROUTINE GET_STRING_LENGTHS

    END SUBROUTINE JSON_STRING_INFO
!*****************************************************************************************

!*****************************************************************************************
!
!  Returns information about a [[json_value]], given the path.
!
!--- See also
!  * [[json_info]]
!
!@note If `found` is present, no exceptions will be thrown if an
!      error occurs. Otherwise, an exception will be thrown if the
!      variable is not found.

    SUBROUTINE JSON_INFO_BY_PATH(JSON,P,PATH,FOUND,VAR_TYPE,N_CHILDREN,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: P          !! a JSON linked list
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)  :: PATH       !! path to the variable
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND      !! true if it was found
    INTEGER(IK),INTENT(OUT),OPTIONAL     :: VAR_TYPE   !! variable type
    INTEGER(IK),INTENT(OUT),OPTIONAL     :: N_CHILDREN !! number of children
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT),OPTIONAL :: NAME !! variable name

    TYPE(JSON_VALUE),POINTER :: P_VAR  !! temporary pointer
    LOGICAL(LK) :: OK  !! if the variable was found
!- 1629


    CALL JSON%GET(P,PATH,P_VAR,FOUND)

!check if it was found:
    IF (PRESENT(FOUND)) THEN
        OK = FOUND
    ELSE
        OK = .NOT. JSON%EXCEPTION_THROWN
    END IF

    IF (.NOT. OK) THEN
        IF (PRESENT(VAR_TYPE))   VAR_TYPE   = JSON_UNKNOWN
        IF (PRESENT(N_CHILDREN)) N_CHILDREN = 0
        IF (PRESENT(NAME))       NAME       = CK_''
    ELSE
!get info:

!- 1657

        CALL JSON%INFO(P_VAR,VAR_TYPE,N_CHILDREN,NAME)


    END IF

    END SUBROUTINE JSON_INFO_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_info_by_path]] where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_INFO_BY_PATH(JSON,P,PATH,FOUND,VAR_TYPE,N_CHILDREN,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: P          !! a JSON linked list
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH       !! path to the variable
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND      !! true if it was found
    INTEGER(IK),INTENT(OUT),OPTIONAL     :: VAR_TYPE   !! variable type
    INTEGER(IK),INTENT(OUT),OPTIONAL     :: N_CHILDREN !! number of children
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT),OPTIONAL :: NAME !! variable name

    CALL JSON%INFO(P,TO_UNICODE(PATH),FOUND,VAR_TYPE,N_CHILDREN,NAME)

    END SUBROUTINE WRAP_JSON_INFO_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 10/16/2015
!
!  Alternate version of [[json_info]] that returns matrix
!  information about a [[json_value]].
!
!  A [[json_value]] is a valid rank 2 matrix if all of the following are true:
!
!  * The var_type is *json_array*
!  * Each child is also a *json_array*, each of which has the same number of elements
!  * Each individual element has the same variable type (integer, logical, etc.)
!
!  The idea here is that if it is a valid matrix, it can be interoperable with
!  a Fortran rank 2 array of the same type.
!
!--- Example
!
!  The following example is an array with `var_type=json_integer`,
!  `n_sets=3`, and `set_size=4`
!
!```json
!    {
!        "matrix": [
!            [1,2,3,4],
!            [5,6,7,8],
!            [9,10,11,12]
!        ]
!    }
!```

    SUBROUTINE JSON_MATRIX_INFO(JSON,P,IS_MATRIX,VAR_TYPE,N_SETS,SET_SIZE,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)   :: JSON
    TYPE(JSON_VALUE),POINTER         :: P          !! a JSON linked list
    LOGICAL(LK),INTENT(OUT)          :: IS_MATRIX  !! true if it is a valid matrix
    INTEGER(IK),INTENT(OUT),OPTIONAL :: VAR_TYPE   !! variable type of data in the matrix
!! (if all elements have the same type)
    INTEGER(IK),INTENT(OUT),OPTIONAL :: N_SETS     !! number of data sets (i.e., matrix
!! rows if using row-major order)
    INTEGER(IK),INTENT(OUT),OPTIONAL :: SET_SIZE   !! size of each data set (i.e., matrix
!! cols if using row-major order)
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT),OPTIONAL :: NAME !! variable name

    TYPE(JSON_VALUE),POINTER :: P_ROW       !! for getting a set
    TYPE(JSON_VALUE),POINTER :: P_ELEMENT   !! for getting an element in a set
    INTEGER(IK) :: VARTYPE         !! json variable type of `p`
    INTEGER(IK) :: ROW_VARTYPE     !! json variable type of a row
    INTEGER(IK) :: ELEMENT_VARTYPE !! json variable type of an element in a row
    INTEGER(IK) :: NR              !! number of children of `p`
    INTEGER(IK) :: NC              !! number of elements in first child of `p`
    INTEGER(IK) :: ICOUNT          !! number of elements in a set
    INTEGER(IK) :: I               !! counter
    INTEGER(IK) :: J               !! counter
!- 1745


!get info about the variable:
!- 1758

    CALL JSON%INFO(P,VARTYPE,NR,NAME)


    IS_MATRIX = (VARTYPE==JSON_ARRAY)

    IF (IS_MATRIX) THEN

        MAIN : DO I=1,NR

            NULLIFY(P_ROW)
            CALL JSON%GET_CHILD(P,I,P_ROW)
            IF (.NOT. ASSOCIATED(P_ROW)) THEN
                IS_MATRIX = .FALSE.
                CALL JSON%THROW_EXCEPTION('Error in json_matrix_info: '//&
                                          'Malformed JSON linked list')
                EXIT MAIN
            END IF
            CALL JSON%INFO(P_ROW,VAR_TYPE=ROW_VARTYPE,N_CHILDREN=ICOUNT)

            IF (ROW_VARTYPE==JSON_ARRAY) THEN
                IF (I==1) NC = ICOUNT  !number of columns in first row
                IF (ICOUNT==NC) THEN   !make sure each row has the same number of columns
!see if all the variables in this row are the same type:
                    DO J=1,ICOUNT
                        NULLIFY(P_ELEMENT)
                        CALL JSON%GET_CHILD(P_ROW,J,P_ELEMENT)
                        IF (.NOT. ASSOCIATED(P_ELEMENT)) THEN
                            IS_MATRIX = .FALSE.
                            CALL JSON%THROW_EXCEPTION('Error in json_matrix_info: '//&
                                                      'Malformed JSON linked list')
                            EXIT MAIN
                        END IF
                        CALL JSON%INFO(P_ELEMENT,VAR_TYPE=ELEMENT_VARTYPE)
                        IF (I==1 .AND. J==1) VARTYPE = ELEMENT_VARTYPE  !type of first element
!in the row
                        IF (VARTYPE/=ELEMENT_VARTYPE) THEN
!not all variables are the same time
                            IS_MATRIX = .FALSE.
                            EXIT MAIN
                        END IF
                    END DO
                ELSE
                    IS_MATRIX = .FALSE.
                    EXIT MAIN
                END IF
            ELSE
                IS_MATRIX = .FALSE.
                EXIT MAIN
            END IF

        END DO MAIN

    END IF

    IF (IS_MATRIX) THEN
        IF (PRESENT(VAR_TYPE)) VAR_TYPE = VARTYPE
        IF (PRESENT(N_SETS))   N_SETS   = NR
        IF (PRESENT(SET_SIZE)) SET_SIZE = NC
    ELSE
        IF (PRESENT(VAR_TYPE)) VAR_TYPE = JSON_UNKNOWN
        IF (PRESENT(N_SETS))   N_SETS   = 0
        IF (PRESENT(SET_SIZE)) SET_SIZE = 0
    END IF

    END SUBROUTINE JSON_MATRIX_INFO
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns matrix information about a [[json_value]], given the path.
!
!--- See also
!  * [[json_matrix_info]]
!
!@note If `found` is present, no exceptions will be thrown if an
!      error occurs. Otherwise, an exception will be thrown if the
!      variable is not found.

    SUBROUTINE JSON_MATRIX_INFO_BY_PATH(JSON,P,PATH,IS_MATRIX,FOUND,&
                                        VAR_TYPE,N_SETS,SET_SIZE,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: P         !! a JSON linked list
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

    TYPE(JSON_VALUE),POINTER :: P_VAR
    LOGICAL(LK) :: OK
!- 1860


    CALL JSON%GET(P,PATH,P_VAR,FOUND)

!check if it was found:
    IF (PRESENT(FOUND)) THEN
        OK = FOUND
    ELSE
        OK = .NOT. JSON%EXCEPTION_THROWN
    END IF

    IF (.NOT. OK) THEN
        IF (PRESENT(VAR_TYPE)) VAR_TYPE = JSON_UNKNOWN
        IF (PRESENT(N_SETS))   N_SETS   = 0
        IF (PRESENT(SET_SIZE)) SET_SIZE = 0
        IF (PRESENT(NAME))     NAME     = CK_''
    ELSE

!get info about the variable:
!- 1889

        CALL JSON%MATRIX_INFO(P_VAR,IS_MATRIX,VAR_TYPE,N_SETS,SET_SIZE,NAME)

        IF (JSON%EXCEPTION_THROWN .AND. PRESENT(FOUND)) THEN
            FOUND = .FALSE.
            CALL JSON%CLEAR_EXCEPTIONS()
        END IF
    END IF

    END SUBROUTINE JSON_MATRIX_INFO_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_matrix_info_by_path]] where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_MATRIX_INFO_BY_PATH(JSON,P,PATH,IS_MATRIX,FOUND,&
                                             VAR_TYPE,N_SETS,SET_SIZE,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P          !! a JSON linked list
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH       !! path to the variable
    LOGICAL(LK),INTENT(OUT)              :: IS_MATRIX  !! true if it is a valid matrix
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND      !! true if it was found
    INTEGER(IK),INTENT(OUT),OPTIONAL     :: VAR_TYPE   !! variable type of data in
!! the matrix (if all elements have
!! the same type)
    INTEGER(IK),INTENT(OUT),OPTIONAL     :: N_SETS     !! number of data sets (i.e., matrix
!! rows if using row-major order)
    INTEGER(IK),INTENT(OUT),OPTIONAL     :: SET_SIZE   !! size of each data set (i.e., matrix
!! cols if using row-major order)
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT),OPTIONAL :: NAME !! variable name

    CALL JSON%MATRIX_INFO(P,TO_UNICODE(PATH),IS_MATRIX,FOUND,VAR_TYPE,N_SETS,SET_SIZE,NAME)

    END SUBROUTINE WRAP_JSON_MATRIX_INFO_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/29/2016
!
!  Rename a [[json_value]].

    SUBROUTINE JSON_VALUE_RENAME(JSON,P,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN) :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: NAME !! new variable name

    IF (JSON%TRAILING_SPACES_SIGNIFICANT) THEN
        P%NAME = NAME
    ELSE
        P%NAME = TRIM(NAME)
    END IF

    END SUBROUTINE JSON_VALUE_RENAME
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/29/2016
!
!  Alternate version of [[json_value_rename]], where `name` is kind=CDK.

    SUBROUTINE WRAP_JSON_VALUE_RENAME(JSON,P,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: NAME !! new variable name

    CALL JSON%RENAME(P,TO_UNICODE(NAME))

    END SUBROUTINE WRAP_JSON_VALUE_RENAME
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/4/2013
!
!  Clear exceptions in the [[json_core(type)]].

    PURE SUBROUTINE JSON_CLEAR_EXCEPTIONS(JSON)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)  :: JSON

!clear the flag and message:
    JSON%EXCEPTION_THROWN = .FALSE.
    IF (ALLOCATED(JSON%ERR_MESSAGE)) DEALLOCATE(JSON%ERR_MESSAGE)

    END SUBROUTINE JSON_CLEAR_EXCEPTIONS
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/4/2013
!
!  Throw an exception in the [[json_core(type)]].
!  This routine sets the error flag, and prevents any subsequent routine
!  from doing anything, until [[json_clear_exceptions]] is called.
!
!@note If `is_verbose` is true, this will also print a
!      traceback if the Intel compiler is used.
!
!@note If `stop_on_error` is true, then the program is stopped.

    SUBROUTINE JSON_THROW_EXCEPTION(JSON,MSG,FOUND)

#ifndef __GFORTRAN__
    USE IFCORE, ONLY: TRACEBACKQQ
#endif

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: MSG    !! the error message
    LOGICAL(LK),INTENT(INOUT),OPTIONAL  :: FOUND  !! if the caller is handling the
!! exception with an optimal return
!! argument. If so, `json%stop_on_error`
!! is ignored.

    LOGICAL(LK) :: STOP_ON_ERROR

    JSON%EXCEPTION_THROWN = .TRUE.
    JSON%ERR_MESSAGE = TRIM(MSG)
    STOP_ON_ERROR = JSON%STOP_ON_ERROR .AND. .NOT. PRESENT(FOUND)

    IF (STOP_ON_ERROR) THEN


#ifndef __GFORTRAN__
! for Intel, we raise a traceback and quit
        CALL TRACEBACKQQ(STRING=TRIM(MSG), USER_EXIT_CODE=0)
!- 2032
#endif


    ELSEIF (JSON%IS_VERBOSE) THEN

        WRITE(OUTPUT_UNIT,'(A)') '***********************'
        WRITE(OUTPUT_UNIT,'(A)') 'JSON-Fortran Exception: '//TRIM(MSG)

!#if defined __GFORTRAN__
!        call backtrace()  ! (have to compile with -fbacktrace -fall-intrinsics flags)
!#endif

#ifndef __GFORTRAN__
        CALL TRACEBACKQQ(USER_EXIT_CODE=-1)  ! print a traceback and return
#endif


        WRITE(OUTPUT_UNIT,'(A)') '***********************'

    END IF

    END SUBROUTINE JSON_THROW_EXCEPTION
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_throw_exception]], where `msg` is kind=CDK.

    SUBROUTINE WRAP_JSON_THROW_EXCEPTION(JSON,MSG,FOUND)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)  :: JSON
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: MSG    !! the error message
    LOGICAL(LK),INTENT(INOUT),OPTIONAL  :: FOUND  !! if the caller is handling the
!! exception with an optimal return
!! argument. If so, `json%stop_on_error`
!! is ignored.

    CALL JSON%THROW_EXCEPTION(TO_UNICODE(MSG),FOUND)

    END SUBROUTINE WRAP_JSON_THROW_EXCEPTION
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/4/2013
!
!  Retrieve error code from the [[json_core(type)]].
!  This should be called after `parse` to check for errors.
!  If an error is thrown, before using the class again, [[json_initialize]]
!  should be called to clean up before it is used again.
!
!--- Example
!
!````fortran
!     type(json_file) :: json
!     logical :: status_ok
!     character(kind=CK,len=:),allocatable :: error_msg
!     call json%load(filename='myfile.json')
!     call json%check_for_errors(status_ok, error_msg)
!     if (.not. status_ok) then
!         write(*,*) 'Error: '//error_msg
!         call json%clear_exceptions()
!         call json%destroy()
!     end if
!````
!
!--- See also
!  * [[json_failed]]
!  * [[json_throw_exception]]

    SUBROUTINE JSON_CHECK_FOR_ERRORS(JSON,STATUS_OK,ERROR_MSG)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(IN) :: JSON
    LOGICAL(LK),INTENT(OUT),OPTIONAL :: STATUS_OK !! true if there were no errors
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT),OPTIONAL :: ERROR_MSG !! the error message.
!! (not allocated if
!! there were no errors)

!- 2114


    IF (PRESENT(STATUS_OK)) STATUS_OK = .NOT. JSON%EXCEPTION_THROWN

    IF (PRESENT(ERROR_MSG)) THEN
        IF (JSON%EXCEPTION_THROWN) THEN
! if an exception has been thrown,
! then this will always be allocated
! [see json_throw_exception]
!- 2126

            ERROR_MSG = JSON%ERR_MESSAGE

        END IF
    END IF

    END SUBROUTINE JSON_CHECK_FOR_ERRORS
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/5/2013
!
!  Logical function to indicate if an exception has been thrown in a [[json_core(type)]].
!
!--- Example
!
!````fortran
!    type(json_core) :: json
!    type(json_value),pointer :: p
!    logical :: status_ok
!    character(len=:),allocatable :: error_msg
!    call json%load(filename='myfile.json',p)
!    if (json%failed()) then
!        call json%check_for_errors(status_ok, error_msg)
!        write(*,*) 'Error: '//error_msg
!        call json%clear_exceptions()
!        call json%destroy(p)
!    end if
!````
!
!  Note that [[json_file]] contains a wrapper for this routine, which is used like:
!````fortran
!    type(json_file) :: f
!    logical :: status_ok
!    character(len=:),allocatable :: error_msg
!    call f%load(filename='myfile.json')
!    if (f%failed()) then
!        call f%check_for_errors(status_ok, error_msg)
!        write(*,*) 'Error: '//error_msg
!        call f%clear_exceptions()
!        call f%destroy()
!    end if
!````
!
!--- See also
!  * [[json_check_for_errors]]

    PURE FUNCTION JSON_FAILED(JSON) RESULT(FAILED)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(IN) :: JSON
    LOGICAL(LK)                 :: FAILED  !! will be true if an exception
!! has been thrown.

    FAILED = JSON%EXCEPTION_THROWN

    END FUNCTION JSON_FAILED
!*****************************************************************************************

!*****************************************************************************************
!>
!  Allocate a [[json_value]] pointer variable.
!  This should be called before adding data to it.
!
!--- Example
!
!````fortran
!    type(json_value),pointer :: var
!    call json_value_create(var)
!    call json%to_real(var,1.0_RK)
!````
!
!--- Notes
!  1. This routine does not check for exceptions.
!  2. The pointer should not already be allocated, or a memory leak will occur.

    SUBROUTINE JSON_VALUE_CREATE(P)

    IMPLICIT NONE

    TYPE(JSON_VALUE),POINTER :: P

    NULLIFY(P)
    ALLOCATE(P)

    END SUBROUTINE JSON_VALUE_CREATE
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/22/2014
!
!  Destroy a [[json_value]] linked-list structure.
!
!@note The original FSON version of this
!      routine was not properly freeing the memory.
!      It was rewritten.
!
!@note This routine destroys this variable, it's children, and
!      (if `destroy_next` is true) the subsequent elements in
!      an object or array. It does not destroy the parent or
!      previous elements.
!
!@Note There is some protection here to enable destruction of
!      improperly-created linked lists. However, likely there
!      are cases not handled. Use the [[json_value_validate]]
!      method to validate a JSON structure that was manually
!      created using [[json_value]] pointers.

    PURE RECURSIVE SUBROUTINE JSON_VALUE_DESTROY(JSON,P,DESTROY_NEXT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)  :: JSON
    TYPE(JSON_VALUE),POINTER        :: P            !! variable to destroy
    LOGICAL(LK),INTENT(IN),OPTIONAL :: DESTROY_NEXT !! if true, then `p%next`
!! is also destroyed (default is true)

    LOGICAL(LK)              :: DES_NEXT  !! local copy of `destroy_next`
!! optional argument
    TYPE(JSON_VALUE),POINTER :: CHILD     !! for getting child elements
    LOGICAL                  :: CIRCULAR  !! to check to malformed linked lists

    IF (ASSOCIATED(P)) THEN

        IF (PRESENT(DESTROY_NEXT)) THEN
            DES_NEXT = DESTROY_NEXT
        ELSE
            DES_NEXT = .TRUE.
        END IF

        IF (ALLOCATED(P%NAME)) DEALLOCATE(P%NAME)

        CALL DESTROY_JSON_DATA(P)

        IF (ASSOCIATED(P%NEXT)) THEN
! check for circular references:
            IF (ASSOCIATED(P, P%NEXT)) NULLIFY(P%NEXT)
        END IF

        IF (ASSOCIATED(P%CHILDREN)) THEN
            DO WHILE (P%N_CHILDREN > 0)
                CHILD => P%CHILDREN
                IF (ASSOCIATED(CHILD)) THEN
                    P%CHILDREN => P%CHILDREN%NEXT
                    P%N_CHILDREN = P%N_CHILDREN - 1
! check children for circular references:
                    CIRCULAR = (ASSOCIATED(P%CHILDREN) .AND. &
                                ASSOCIATED(P%CHILDREN,CHILD))
                    CALL JSON%DESTROY(CHILD,DESTROY_NEXT=.FALSE.)
                    IF (CIRCULAR) EXIT
                ELSE
! it is a malformed JSON object. But, we will
! press ahead with the destroy process, since
! otherwise, there would be no way to destroy it.
                    EXIT
                END IF
            END DO
            NULLIFY(P%CHILDREN)
            NULLIFY(CHILD)
        END IF

        IF (ASSOCIATED(P%NEXT) .AND. DES_NEXT) CALL JSON%DESTROY(P%NEXT)

        NULLIFY(P%PREVIOUS)
        NULLIFY(P%PARENT)
        NULLIFY(P%TAIL)

        IF (ASSOCIATED(P)) DEALLOCATE(P)
        NULLIFY(P)

    END IF

    END SUBROUTINE JSON_VALUE_DESTROY
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 9/9/2014
!
!  Remove a [[json_value]] (and all its children)
!  from a linked-list structure, preserving the rest of the structure.
!
!--- Examples
!
!  To extract an object from one JSON structure, and add it to another:
!````fortran
!     type(json_core) :: json
!     type(json_value),pointer :: json1,json2,p
!     logical :: found
!     !create and populate json1 and json2
!     call json%get(json1,'name',p,found)  ! get pointer to name element of json1
!     call json%remove(p,destroy=.false.)  ! remove it from json1 (don't destroy)
!     call json%add(json2,p)               ! add it to json2
!````
!
!  To remove an object from a JSON structure (and destroy it):
!````fortran
!     type(json_core) :: json
!     type(json_value),pointer :: json1,p
!     logical :: found
!     !create and populate json1
!     call json%get(json1,'name',p,found)  ! get pointer to name element of json1
!     call json%remove(p)                  ! remove and destroy it
!````
!
!--- History
!  * Jacob Williams : 12/28/2014 : added destroy optional argument.
!  * Jacob Williams : 12/04/2020 : bug fix.

    SUBROUTINE JSON_VALUE_REMOVE(JSON,P,DESTROY)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)  :: JSON
    TYPE(JSON_VALUE),POINTER        :: P
    LOGICAL(LK),INTENT(IN),OPTIONAL :: DESTROY  !! Option to destroy `p` after it is removed:
!!
!! * If `destroy` is not present, it is also destroyed.
!! * If `destroy` is present and true, it is destroyed.
!! * If `destroy` is present and false, it is not destroyed.

    TYPE(JSON_VALUE),POINTER :: PARENT     !! pointer to parent
    TYPE(JSON_VALUE),POINTER :: PREVIOUS   !! pointer to previous
    TYPE(JSON_VALUE),POINTER :: NEXT       !! pointer to next
    LOGICAL(LK)              :: DESTROY_IT !! if `p` should be destroyed

    IF (ASSOCIATED(P)) THEN

!optional input argument:
        IF (PRESENT(DESTROY)) THEN
            DESTROY_IT = DESTROY
        ELSE
            DESTROY_IT = .TRUE.
        END IF

        IF (ASSOCIATED(P%PARENT)) THEN

            PARENT => P%PARENT

            IF (ASSOCIATED(P%NEXT)) THEN

!there are later items in the list:
                NEXT => P%NEXT

                IF (ASSOCIATED(P%PREVIOUS)) THEN
!there are earlier items in the list
                    PREVIOUS => P%PREVIOUS
                    PREVIOUS%NEXT => NEXT
                    NEXT%PREVIOUS => PREVIOUS
                ELSE
!this is the first item in the list
                    PARENT%CHILDREN => NEXT
                    NULLIFY(NEXT%PREVIOUS)
                END IF

            ELSE

                IF (ASSOCIATED(P%PREVIOUS)) THEN
!there are earlier items in the list:
                    PREVIOUS => P%PREVIOUS
                    NULLIFY(PREVIOUS%NEXT)
                    PARENT%TAIL => PREVIOUS
                ELSE
!this is the only item in the list:
                    NULLIFY(PARENT%CHILDREN)
                    NULLIFY(PARENT%TAIL)
                END IF

            END IF

! nullify all pointers to original structure:
            NULLIFY(P%NEXT)
            NULLIFY(P%PREVIOUS)
            NULLIFY(P%PARENT)

            PARENT%N_CHILDREN = PARENT%N_CHILDREN - 1

        END IF

        IF (DESTROY_IT) CALL JSON%DESTROY(P)

    END IF

    END SUBROUTINE JSON_VALUE_REMOVE
!*****************************************************************************************

!*****************************************************************************************
!>
!  Replace `p1` with `p2` in a JSON structure.
!
!@note The replacement is done using an insert and remove
!      See [[json_value_insert_after]] and [[json_value_remove]]
!      for details.

    SUBROUTINE JSON_VALUE_REPLACE(JSON,P1,P2,DESTROY)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)  :: JSON
    TYPE(JSON_VALUE),POINTER        :: P1       !! the item to replace
    TYPE(JSON_VALUE),POINTER        :: P2       !! item to take the place of `p1`
    LOGICAL(LK),INTENT(IN),OPTIONAL :: DESTROY  !! Should `p1` also be destroyed
!! (default is True). Normally,
!! this should be true to avoid
!! a memory leak.

    LOGICAL(LK) :: DESTROY_P1 !! if `p1` is to be destroyed

    IF (PRESENT(DESTROY)) THEN
        DESTROY_P1 = DESTROY
    ELSE
        DESTROY_P1 = .TRUE.  ! default
    END IF

    CALL JSON%INSERT_AFTER(P1,P2)
    CALL JSON%REMOVE(P1,DESTROY_P1)

    END SUBROUTINE JSON_VALUE_REPLACE
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/11/2017
!
!  Reverse the order of the children of an array or object.

    SUBROUTINE JSON_VALUE_REVERSE(JSON,P)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT) :: JSON
    TYPE(JSON_VALUE),POINTER       :: P

    TYPE(JSON_VALUE),POINTER :: TMP       !! temp variable for traversing the list
    TYPE(JSON_VALUE),POINTER :: CURRENT   !! temp variable for traversing the list
    INTEGER(IK)              :: VAR_TYPE  !! for getting the variable type

    IF (ASSOCIATED(P)) THEN

        CALL JSON%INFO(P,VAR_TYPE=VAR_TYPE)

! can only reverse objects or arrays
        IF (VAR_TYPE==JSON_OBJECT .OR. VAR_TYPE==JSON_ARRAY) THEN

            NULLIFY(TMP)
            CURRENT => P%CHILDREN
            P%TAIL => CURRENT

! Swap next and previous for all nodes:
            DO
                IF (.NOT. ASSOCIATED(CURRENT)) EXIT
                TMP              => CURRENT%PREVIOUS
                CURRENT%PREVIOUS => CURRENT%NEXT
                CURRENT%NEXT     => TMP
                CURRENT          => CURRENT%PREVIOUS
            END DO

            IF (ASSOCIATED(TMP)) THEN
                P%CHILDREN => TMP%PREVIOUS
            END IF

        END IF

    END IF

    END SUBROUTINE JSON_VALUE_REVERSE
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/26/2016
!
!  Swap two elements in a JSON structure.
!  All of the children are carried along as well.
!
!@note If both are not associated, then an error is thrown.
!
!@note The assumption here is that both variables are part of a valid
!      [[json_value]] linked list (so the normal `parent`, `previous`,
!      `next`, etc. pointers are properly associated if necessary).
!
!@warning This cannot be used to swap a parent/child pair, since that
!         could lead to a circular linkage. An exception is thrown if
!         this is tried.
!
!@warning There are also other situations where using this routine may
!         produce a malformed JSON structure, such as moving an array
!         element outside of an array. This is not checked for.
!
!@note If `p1` and `p2` have a common parent, it is always safe to swap them.

    SUBROUTINE JSON_VALUE_SWAP(JSON,P1,P2)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT) :: JSON
    TYPE(JSON_VALUE),POINTER       :: P1  !! swap with `p2`
    TYPE(JSON_VALUE),POINTER       :: P2  !! swap with `p1`

    LOGICAL                  :: SAME_PARENT !! if `p1` and `p2` have the same parent
    LOGICAL                  :: FIRST_LAST  !! if `p1` and `p2` are the first,last or
!! last,first children of a common parent
    LOGICAL                  :: ADJACENT    !! if `p1` and `p2` are adjacent
!! elements in an array
    TYPE(JSON_VALUE),POINTER :: A           !! temporary variable
    TYPE(JSON_VALUE),POINTER :: B           !! temporary variable

    IF (JSON%EXCEPTION_THROWN) RETURN

!both have to be associated:
    IF (ASSOCIATED(P1) .AND. ASSOCIATED(P2)) THEN

!simple check to make sure that they both
!aren't pointing to the same thing:
        IF (.NOT. ASSOCIATED(P1,P2)) THEN

!we will not allow swapping an item with one of its descendants:
            IF (JSON%IS_CHILD_OF(P1,P2) .OR. JSON%IS_CHILD_OF(P2,P1)) THEN
                CALL JSON%THROW_EXCEPTION('Error in json_value_swap: '//&
                                          'cannot swap an item with one of its descendants')
            ELSE

                SAME_PARENT = ( ASSOCIATED(P1%PARENT) .AND. &
                                ASSOCIATED(P2%PARENT) .AND. &
                                ASSOCIATED(P1%PARENT,P2%PARENT) )
                IF (SAME_PARENT) THEN
                    FIRST_LAST = (ASSOCIATED(P1%PARENT%CHILDREN,P1) .AND. &
                                  ASSOCIATED(P2%PARENT%TAIL,P2)) .OR. &
                                 (ASSOCIATED(P1%PARENT%TAIL,P1) .AND. &
                                  ASSOCIATED(P2%PARENT%CHILDREN,P2))
                ELSE
                    FIRST_LAST = .FALSE.
                END IF

!first, we fix children,tail pointers:

                IF (SAME_PARENT .AND. FIRST_LAST) THEN

!this is all we have to do for the parent in this case:
                    CALL SWAP_POINTERS(P1%PARENT%CHILDREN,P2%PARENT%TAIL)

                ELSE IF (SAME_PARENT .AND. .NOT. FIRST_LAST) THEN

                    IF (ASSOCIATED(P1%PARENT%CHILDREN,P1)) THEN
                        P1%PARENT%CHILDREN => P2 ! p1 is the first child of the parent
                    ELSE IF (ASSOCIATED(P1%PARENT%CHILDREN,P2)) THEN
                        P1%PARENT%CHILDREN => P1 ! p2 is the first child of the parent
                    END IF
                    IF (ASSOCIATED(P1%PARENT%TAIL,P1)) THEN
                        P1%PARENT%TAIL => P2 ! p1 is the last child of the parent
                    ELSE IF (ASSOCIATED(P1%PARENT%TAIL,P2)) THEN
                        P1%PARENT%TAIL => P1 ! p2 is the last child of the parent
                    END IF

                ELSE ! general case: different parents

                    IF (ASSOCIATED(P1%PARENT)) THEN
                        IF (ASSOCIATED(P1%PARENT%CHILDREN,P1)) P1%PARENT%CHILDREN => P2
                        IF (ASSOCIATED(P1%PARENT%TAIL,P1))     P1%PARENT%TAIL     => P2
                    END IF
                    IF (ASSOCIATED(P2%PARENT)) THEN
                        IF (ASSOCIATED(P2%PARENT%CHILDREN,P2)) P2%PARENT%CHILDREN => P1
                        IF (ASSOCIATED(P2%PARENT%TAIL,P2))     P2%PARENT%TAIL     => P1
                    END IF
                    CALL SWAP_POINTERS(P1%PARENT, P2%PARENT)

                END IF

!now, have to fix previous,next pointers:

!first, see if they are adjacent:
                ADJACENT = ASSOCIATED(P1%NEXT,P2) .OR. &
                           ASSOCIATED(P2%NEXT,P1)
                IF (ASSOCIATED(P2%NEXT,P1)) THEN    !p2,p1
                    A => P2
                    B => P1
                ELSE    !p1,p2 (or not adjacent)
                    A => P1
                    B => P2
                END IF
                IF (ASSOCIATED(A%PREVIOUS)) A%PREVIOUS%NEXT => B
                IF (ASSOCIATED(B%NEXT))     B%NEXT%PREVIOUS => A

                IF (ADJACENT) THEN
!a comes before b in the original list
                    B%PREVIOUS => A%PREVIOUS
                    A%NEXT     => B%NEXT
                    A%PREVIOUS => B
                    B%NEXT     => A
                ELSE
                    IF (ASSOCIATED(A%NEXT))       A%NEXT%PREVIOUS => B
                    IF (ASSOCIATED(B%PREVIOUS))   B%PREVIOUS%NEXT => A
                    CALL SWAP_POINTERS(A%PREVIOUS,B%PREVIOUS)
                    CALL SWAP_POINTERS(A%NEXT,    B%NEXT)
                END IF

            END IF

        ELSE
            CALL JSON%THROW_EXCEPTION('Error in json_value_swap: '//&
                                      'both pointers must be associated')
        END IF

    END IF

    CONTAINS

        PURE SUBROUTINE SWAP_POINTERS(S1,S2)

        IMPLICIT NONE

        TYPE(JSON_VALUE),POINTER,INTENT(INOUT) :: S1
        TYPE(JSON_VALUE),POINTER,INTENT(INOUT) :: S2

        TYPE(JSON_VALUE),POINTER :: TMP  !! temporary pointer

        IF (.NOT. ASSOCIATED(S1,S2)) THEN
            TMP => S1
            S1  => S2
            S2  => TMP
        END IF

        END SUBROUTINE SWAP_POINTERS

    END SUBROUTINE JSON_VALUE_SWAP
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/28/2016
!
!  Returns True if `p2` is a descendant of `p1`
!  (i.e, a child, or a child of child, etc.)

    FUNCTION JSON_VALUE_IS_CHILD_OF(JSON,P1,P2) RESULT(IS_CHILD_OF)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT) :: JSON
    TYPE(JSON_VALUE),POINTER       :: P1
    TYPE(JSON_VALUE),POINTER       :: P2
    LOGICAL(LK)                    :: IS_CHILD_OF

    IS_CHILD_OF = .FALSE.

    IF (JSON%EXCEPTION_THROWN) RETURN

    IF (ASSOCIATED(P1) .AND. ASSOCIATED(P2)) THEN
        IF (ASSOCIATED(P1%CHILDREN)) THEN
            CALL JSON%TRAVERSE(P1%CHILDREN,IS_CHILD_OF_CALLBACK)
        END IF
    END IF

    CONTAINS

    SUBROUTINE IS_CHILD_OF_CALLBACK(JSON,P,FINISHED)
!! Traverse until `p` is `p2`.
    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN) :: P
    LOGICAL(LK),INTENT(OUT)             :: FINISHED

    IS_CHILD_OF = ASSOCIATED(P,P2)
    FINISHED = IS_CHILD_OF  ! stop searching if found

    END SUBROUTINE IS_CHILD_OF_CALLBACK

    END FUNCTION JSON_VALUE_IS_CHILD_OF
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 5/2/2016
!
!  Validate a [[json_value]] linked list by checking to make sure
!  all the pointers are properly associated, arrays and objects
!  have the correct number of children, and the correct data is
!  allocated for the variable types.
!
!  It recursively traverses the entire structure and checks every element.
!
!--- History
!  * Jacob Williams, 8/26/2017 : added duplicate key check.
!
!@note It will return on the first error it encounters.
!
!@note This routine does not check or throw any exceptions.
!      If `json` is currently in a state of exception, it will
!      remain so after calling this routine.

    SUBROUTINE JSON_VALUE_VALIDATE(JSON,P,IS_VALID,ERROR_MSG)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN) :: P
    LOGICAL(LK),INTENT(OUT)             :: IS_VALID  !! True if the structure is valid.
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT) :: ERROR_MSG !! if not valid, this will contain
!! a description of the problem

    LOGICAL(LK) :: HAS_DUPLICATE !! to check for duplicate keys
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: PATH  !! path to duplicate key
    LOGICAL(LK) :: STATUS_OK !! to check for existing exception
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: EXCEPTION_MSG  !! error message for an existing exception
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: EXCEPTION_MSG2  !! error message for a new exception

    IF (ASSOCIATED(P)) THEN

        IS_VALID = .TRUE.
        CALL CHECK_IF_VALID(P,REQUIRE_PARENT=ASSOCIATED(P%PARENT))

        IF (IS_VALID .AND. .NOT. JSON%ALLOW_DUPLICATE_KEYS) THEN
! if no errors so far, also check the
! entire structure for duplicate keys:

! note: check_for_duplicate_keys does call routines
! that check and throw exceptions, so let's clear any
! first. (save message for later)
            CALL JSON%CHECK_FOR_ERRORS(STATUS_OK, EXCEPTION_MSG)
            CALL JSON%CLEAR_EXCEPTIONS()

            CALL JSON%CHECK_FOR_DUPLICATE_KEYS(P,HAS_DUPLICATE,PATH=PATH)
            IF (JSON%FAILED()) THEN
! if an exception was thrown during this call,
! then clear it but make that the error message
! returned by this routine. Normally this should
! never actually occur since we have already
! validated the structure.
                CALL JSON%CHECK_FOR_ERRORS(IS_VALID, EXCEPTION_MSG2)
                ERROR_MSG = EXCEPTION_MSG2
                CALL JSON%CLEAR_EXCEPTIONS()
                IS_VALID = .FALSE.
            ELSE
                IF (HAS_DUPLICATE) THEN
                    ERROR_MSG = 'duplicate key found: '//PATH
                    IS_VALID  = .FALSE.
                END IF
            END IF

            IF (.NOT. STATUS_OK) THEN
! restore any existing exception if necessary
                CALL JSON%THROW_EXCEPTION(EXCEPTION_MSG)
            END IF

! cleanup:
            IF (ALLOCATED(PATH))           DEALLOCATE(PATH)
            IF (ALLOCATED(EXCEPTION_MSG))  DEALLOCATE(EXCEPTION_MSG)
            IF (ALLOCATED(EXCEPTION_MSG2)) DEALLOCATE(EXCEPTION_MSG2)

        END IF

    ELSE
        ERROR_MSG = 'The pointer is not associated'
        IS_VALID = .FALSE.
    END IF

    CONTAINS

    RECURSIVE SUBROUTINE CHECK_IF_VALID(P,REQUIRE_PARENT)

        IMPLICIT NONE

        TYPE(JSON_VALUE),POINTER,INTENT(IN) :: P
        LOGICAL,INTENT(IN) :: REQUIRE_PARENT !! the first one may be a root (so no parent),
!! but all descendants must have a parent.

        INTEGER(IK) :: I !! counter
        TYPE(JSON_VALUE),POINTER :: ELEMENT
        TYPE(JSON_VALUE),POINTER :: PREVIOUS

        IF (IS_VALID .AND. ASSOCIATED(P)) THEN

! data type:
            SELECT CASE (P%VAR_TYPE)
            CASE(JSON_NULL,JSON_OBJECT,JSON_ARRAY)
                IF (ALLOCATED(P%LOG_VALUE) .OR. ALLOCATED(P%INT_VALUE) .OR. &
                    ALLOCATED(P%DBL_VALUE) .OR. ALLOCATED(P%STR_VALUE)) THEN
                    ERROR_MSG = 'incorrect data allocated for '//&
                                'json_null, json_object, or json_array variable type'
                    IS_VALID = .FALSE.
                    RETURN
                END IF
            CASE(JSON_LOGICAL)
                IF (.NOT. ALLOCATED(P%LOG_VALUE)) THEN
                    ERROR_MSG = 'log_value should be allocated for json_logical variable type'
                    IS_VALID = .FALSE.
                    RETURN
                ELSE IF (ALLOCATED(P%INT_VALUE) .OR. &
                    ALLOCATED(P%DBL_VALUE) .OR. ALLOCATED(P%STR_VALUE)) THEN
                    ERROR_MSG = 'incorrect data allocated for json_logical variable type'
                    IS_VALID = .FALSE.
                    RETURN
                END IF
            CASE(JSON_INTEGER)
                IF (.NOT. ALLOCATED(P%INT_VALUE)) THEN
                    ERROR_MSG = 'int_value should be allocated for json_integer variable type'
                    IS_VALID = .FALSE.
                    RETURN
                ELSE IF (ALLOCATED(P%LOG_VALUE) .OR. &
                    ALLOCATED(P%DBL_VALUE) .OR. ALLOCATED(P%STR_VALUE)) THEN
                    ERROR_MSG = 'incorrect data allocated for json_integer variable type'
                    IS_VALID = .FALSE.
                    RETURN
                END IF
            CASE(JSON_REAL)
                IF (.NOT. ALLOCATED(P%DBL_VALUE)) THEN
                    ERROR_MSG = 'dbl_value should be allocated for json_real variable type'
                    IS_VALID = .FALSE.
                    RETURN
                ELSE IF (ALLOCATED(P%LOG_VALUE) .OR. ALLOCATED(P%INT_VALUE) .OR. &
                    ALLOCATED(P%STR_VALUE)) THEN
                    ERROR_MSG = 'incorrect data allocated for json_real variable type'
                    IS_VALID = .FALSE.
                    RETURN
                END IF
            CASE(JSON_STRING)
                IF (.NOT. ALLOCATED(P%STR_VALUE)) THEN
                    ERROR_MSG = 'str_value should be allocated for json_string variable type'
                    IS_VALID = .FALSE.
                    RETURN
                ELSE IF (ALLOCATED(P%LOG_VALUE) .OR. ALLOCATED(P%INT_VALUE) .OR. &
                    ALLOCATED(P%DBL_VALUE)) THEN
                    ERROR_MSG = 'incorrect data allocated for json_string variable type'
                    IS_VALID = .FALSE.
                    RETURN
                END IF
            CASE DEFAULT
                ERROR_MSG = 'invalid JSON variable type'
                IS_VALID = .FALSE.
                RETURN
            END SELECT

            IF (REQUIRE_PARENT .AND. .NOT. ASSOCIATED(P%PARENT)) THEN
                ERROR_MSG = 'parent pointer is not associated'
                IS_VALID = .FALSE.
                RETURN
            END IF

            IF (.NOT. ALLOCATED(P%NAME)) THEN
                IF (ASSOCIATED(P%PARENT)) THEN
                    IF (P%PARENT%VAR_TYPE/=JSON_ARRAY) THEN
                        ERROR_MSG = 'JSON variable must have a name if not an '//&
                                    'array element or the root'
                        IS_VALID = .FALSE.
                        RETURN
                    END IF
                END IF
            END IF

            IF (ASSOCIATED(P%CHILDREN) .NEQV. ASSOCIATED(P%TAIL)) THEN
                ERROR_MSG = 'both children and tail pointers must be associated'
                IS_VALID = .FALSE.
                RETURN
            END IF

! now, check next one:
            IF (ASSOCIATED(P%NEXT)) THEN
                IF (ASSOCIATED(P,P%NEXT)) THEN
                    ERROR_MSG = 'circular linked list'
                    IS_VALID = .FALSE.
                    RETURN
                ELSE
! if it's an element in an
! array, then require a parent:
                    CALL CHECK_IF_VALID(P%NEXT,REQUIRE_PARENT=.TRUE.)
                END IF
            END IF

            IF (ASSOCIATED(P%CHILDREN)) THEN

                IF (P%VAR_TYPE/=JSON_ARRAY .AND. P%VAR_TYPE/=JSON_OBJECT) THEN
                    ERROR_MSG = 'only arrays and objects can have children'
                    IS_VALID = .FALSE.
                    RETURN
                END IF

! first validate children pointers:

                PREVIOUS => NULL()
                ELEMENT => P%CHILDREN
                DO I = 1_IK, P%N_CHILDREN
                    IF (.NOT. ASSOCIATED(ELEMENT%PARENT,P)) THEN
                        ERROR_MSG = 'child''S PARENT POINTER NOT PROPERLY ASSOCIATED'
                        IS_VALID = .FALSE.
                        RETURN
                    END IF
                    IF (I==1 .AND. ASSOCIATED(ELEMENT%PREVIOUS)) THEN
                        ERROR_MSG = 'first child shouldn''T HAVE A PREVIOUS'
                        IS_VALID = .FALSE.
                        RETURN
                    END IF
                    IF (I<P%N_CHILDREN .AND. .NOT. ASSOCIATED(ELEMENT%NEXT)) THEN
                        ERROR_MSG = 'not enough children'
                        IS_VALID = .FALSE.
                        RETURN
                    END IF
                    IF (I==P%N_CHILDREN .AND. ASSOCIATED(ELEMENT%NEXT)) THEN
                        ERROR_MSG = 'too many children'
                        IS_VALID = .FALSE.
                        RETURN
                    END IF
                    IF (I>1) THEN
                        IF (.NOT. ASSOCIATED(PREVIOUS,ELEMENT%PREVIOUS)) THEN
                            ERROR_MSG = 'previous pointer not properly associated'
                            IS_VALID = .FALSE.
                            RETURN
                        END IF
                    END IF
                    IF (I==P%N_CHILDREN .AND. &
                        .NOT. ASSOCIATED(ELEMENT%PARENT%TAIL,ELEMENT)) THEN
                        ERROR_MSG = 'parent''S TAIL POINTER NOT PROPERLY ASSOCIATED'
                        IS_VALID = .FALSE.
                        RETURN
                    END IF
                    IF (I<P%N_CHILDREN) THEN
!setup next case:
                        PREVIOUS => ELEMENT
                        ELEMENT => ELEMENT%NEXT
                    END IF
                END DO

!now check all the children:
                CALL CHECK_IF_VALID(P%CHILDREN,REQUIRE_PARENT=.TRUE.)

            END IF

        END IF

    END SUBROUTINE CHECK_IF_VALID

    END SUBROUTINE JSON_VALUE_VALIDATE
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/6/2014
!
!  Given the path string, remove the variable
!  from [[json_value]], if it exists.

    SUBROUTINE JSON_VALUE_REMOVE_IF_PRESENT(JSON,P,PATH)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH  !! the path to the variable to remove

    TYPE(JSON_VALUE),POINTER :: P_VAR
    LOGICAL(LK) :: FOUND

    CALL JSON%GET(P,PATH,P_VAR,FOUND)
    IF (FOUND) CALL JSON%REMOVE(P_VAR)

    END SUBROUTINE JSON_VALUE_REMOVE_IF_PRESENT
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_remove_if_present]], where `path` is kind=CDK.

    SUBROUTINE WRAP_JSON_VALUE_REMOVE_IF_PRESENT(JSON,P,PATH)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH

    CALL JSON%REMOVE_IF_PRESENT(P,TO_UNICODE(PATH))

    END SUBROUTINE WRAP_JSON_VALUE_REMOVE_IF_PRESENT
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/6/2014
!
!  Given the path string, if the variable is present,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.
!
!@note If the variable is not a scalar, an exception will be thrown.

    SUBROUTINE JSON_UPDATE_LOGICAL(JSON,P,PATH,VAL,FOUND)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH  !! path to the variable in the structure
    LOGICAL(LK),INTENT(IN)              :: VAL   !! the new value
    LOGICAL(LK),INTENT(OUT)             :: FOUND !! if the variable was found and was a scalar.

    TYPE(JSON_VALUE),POINTER :: P_VAR
    INTEGER(IK) :: VAR_TYPE

    CALL JSON%GET(P,PATH,P_VAR,FOUND)
    IF (FOUND) THEN

        CALL JSON%INFO(P_VAR,VAR_TYPE)
        SELECT CASE (VAR_TYPE)
        CASE (JSON_NULL,JSON_LOGICAL,JSON_INTEGER,JSON_REAL,JSON_STRING)
            CALL JSON%TO_LOGICAL(P_VAR,VAL)    !update the value
        CASE DEFAULT
            FOUND = .FALSE.
            CALL JSON%THROW_EXCEPTION('Error in json_update_logical: '//&
                                      'the variable is not a scalar value',FOUND)
        END SELECT

    ELSE
        CALL JSON%ADD_BY_PATH(P,PATH,VAL)   !add the new element
    END IF

    END SUBROUTINE JSON_UPDATE_LOGICAL
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_update_logical]], where `path` is kind=CDK.

    SUBROUTINE WRAP_JSON_UPDATE_LOGICAL(JSON,P,PATH,VAL,FOUND)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH  !! path to the variable in the structure
    LOGICAL(LK),INTENT(IN)               :: VAL   !! the new value
    LOGICAL(LK),INTENT(OUT)              :: FOUND !! if the variable was found and was a scalar.

    CALL JSON%UPDATE(P,TO_UNICODE(PATH),VAL,FOUND)

    END SUBROUTINE WRAP_JSON_UPDATE_LOGICAL
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/6/2014
!
!  Given the path string, if the variable is present,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.
!
!@note If the variable is not a scalar, an exception will be thrown.

    SUBROUTINE JSON_UPDATE_REAL(JSON,P,PATH,VAL,FOUND)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH   !! path to the variable in the structure
    REAL(RK),INTENT(IN)                 :: VAL    !! the new value
    LOGICAL(LK),INTENT(OUT)             :: FOUND  !! if the variable was found and was a scalar.

    TYPE(JSON_VALUE),POINTER :: P_VAR
    INTEGER(IK) :: VAR_TYPE

    CALL JSON%GET(P,PATH,P_VAR,FOUND)
    IF (FOUND) THEN

        CALL JSON%INFO(P_VAR,VAR_TYPE)
        SELECT CASE (VAR_TYPE)
        CASE (JSON_NULL,JSON_LOGICAL,JSON_INTEGER,JSON_REAL,JSON_STRING)
            CALL JSON%TO_REAL(P_VAR,VAL)    !update the value
        CASE DEFAULT
            FOUND = .FALSE.
            CALL JSON%THROW_EXCEPTION('Error in json_update_real: '//&
                                      'the variable is not a scalar value',FOUND)
        END SELECT

    ELSE
        CALL JSON%ADD_BY_PATH(P,PATH,VAL)   !add the new element
    END IF

    END SUBROUTINE JSON_UPDATE_REAL
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_update_real]], where `path` is kind=CDK.

    SUBROUTINE WRAP_JSON_UPDATE_REAL(JSON,P,PATH,VAL,FOUND)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH  !! path to the variable in the structure
    REAL(RK),INTENT(IN)                  :: VAL   !! the new value
    LOGICAL(LK),INTENT(OUT)              :: FOUND !! if the variable was found and was a scalar.

    CALL JSON%UPDATE(P,TO_UNICODE(PATH),VAL,FOUND)

    END SUBROUTINE WRAP_JSON_UPDATE_REAL
!*****************************************************************************************


!*****************************************************************************************
!>
!  Alternate version of [[json_update_real]], where `val` is `real32`.

    SUBROUTINE JSON_UPDATE_REAL32(JSON,P,PATH,VAL,FOUND)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH   !! path to the variable in the structure
    REAL(REAL32),INTENT(IN)             :: VAL    !! the new value
    LOGICAL(LK),INTENT(OUT)             :: FOUND  !! if the variable was found and was a scalar.

    CALL JSON%UPDATE(P,PATH,REAL(VAL,RK),FOUND)

    END SUBROUTINE JSON_UPDATE_REAL32
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_update_real32]], where `path` is kind=CDK.

    SUBROUTINE WRAP_JSON_UPDATE_REAL32(JSON,P,PATH,VAL,FOUND)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH  !! path to the variable in the structure
    REAL(REAL32),INTENT(IN)              :: VAL   !! the new value
    LOGICAL(LK),INTENT(OUT)              :: FOUND !! if the variable was found and was a scalar.

    CALL JSON%UPDATE(P,TO_UNICODE(PATH),REAL(VAL,RK),FOUND)

    END SUBROUTINE WRAP_JSON_UPDATE_REAL32
!*****************************************************************************************


!- 3206


!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/6/2014
!
!  Given the path string, if the variable is present,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.
!
!@note If the variable is not a scalar, an exception will be thrown.

    SUBROUTINE JSON_UPDATE_INTEGER(JSON,P,PATH,VAL,FOUND)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH  !! path to the variable in the structure
    INTEGER(IK),INTENT(IN)              :: VAL   !! the new value
    LOGICAL(LK),INTENT(OUT)             :: FOUND !! if the variable was found and was a scalar.

    TYPE(JSON_VALUE),POINTER :: P_VAR
    INTEGER(IK) :: VAR_TYPE

    CALL JSON%GET(P,PATH,P_VAR,FOUND)
    IF (FOUND) THEN

        CALL JSON%INFO(P_VAR,VAR_TYPE)
        SELECT CASE (VAR_TYPE)
        CASE (JSON_NULL,JSON_LOGICAL,JSON_INTEGER,JSON_REAL,JSON_STRING)
            CALL JSON%TO_INTEGER(P_VAR,VAL)    !update the value
        CASE DEFAULT
            FOUND = .FALSE.
            CALL JSON%THROW_EXCEPTION('Error in json_update_integer: '//&
                                      'the variable is not a scalar value',FOUND)
        END SELECT

    ELSE
        CALL JSON%ADD_BY_PATH(P,PATH,VAL)   !add the new element
    END IF

    END SUBROUTINE JSON_UPDATE_INTEGER
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_update_integer]], where `path` is kind=CDK.

    SUBROUTINE WRAP_JSON_UPDATE_INTEGER(JSON,P,PATH,VAL,FOUND)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH  !! path to the variable in the structure
    INTEGER(IK),INTENT(IN)               :: VAL   !! the new value
    LOGICAL(LK),INTENT(OUT)              :: FOUND !! if the variable was found and was a scalar.

    CALL JSON%UPDATE(P,TO_UNICODE(PATH),VAL,FOUND)

    END SUBROUTINE WRAP_JSON_UPDATE_INTEGER
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/6/2014
!
!  Given the path string, if the variable is present,
!  and is a scalar, then update its value.
!  If it is not present, then create it and set its value.
!
!@note If the variable is not a scalar, an exception will be thrown.

    SUBROUTINE JSON_UPDATE_STRING(JSON,P,PATH,VAL,FOUND,TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH  !! path to the variable in the structure
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: VAL   !! the new value
    LOGICAL(LK),INTENT(OUT)             :: FOUND !! if the variable was found and was a scalar.
    LOGICAL(LK),INTENT(IN),OPTIONAL     :: TRIM_STR    !! if TRIM() should be called for the `val`
!! (only used if `val` is present)
    LOGICAL(LK),INTENT(IN),OPTIONAL     :: ADJUSTL_STR !! if ADJUSTL() should be called for the `val`
!! (only used if `val` is present)
!! (note that ADJUSTL is done before TRIM)

    TYPE(JSON_VALUE),POINTER :: P_VAR
    INTEGER(IK) :: VAR_TYPE

    CALL JSON%GET(P,PATH,P_VAR,FOUND)
    IF (FOUND) THEN

        CALL JSON%INFO(P_VAR,VAR_TYPE)
        SELECT CASE (VAR_TYPE)
        CASE (JSON_NULL,JSON_LOGICAL,JSON_INTEGER,JSON_REAL,JSON_STRING)
            CALL JSON%TO_STRING(P_VAR,VAL,TRIM_STR=TRIM_STR,ADJUSTL_STR=ADJUSTL_STR) ! update the value
        CASE DEFAULT
            FOUND = .FALSE.
            CALL JSON%THROW_EXCEPTION('Error in json_update_string: '//&
                                      'the variable is not a scalar value',FOUND)
        END SELECT

    ELSE
        CALL JSON%ADD_BY_PATH(P,PATH,VAL)   !add the new element
    END IF

    END SUBROUTINE JSON_UPDATE_STRING
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_update_string]], where `path` and `value` are kind=CDK.

    SUBROUTINE WRAP_JSON_UPDATE_STRING(JSON,P,PATH,VAL,FOUND,TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH  !! path to the variable in the structure
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: VAL   !! the new value
    LOGICAL(LK),INTENT(OUT)              :: FOUND !! if the variable was found and was a scalar.
    LOGICAL(LK),INTENT(IN),OPTIONAL     :: TRIM_STR       !! if TRIM() should be called for the `val`
!! (only used if `val` is present)
    LOGICAL(LK),INTENT(IN),OPTIONAL     :: ADJUSTL_STR    !! if ADJUSTL() should be called for the `val`
!! (only used if `val` is present)
!! (note that ADJUSTL is done before TRIM)

    CALL JSON%UPDATE(P,TO_UNICODE(PATH),TO_UNICODE(VAL),FOUND,TRIM_STR,ADJUSTL_STR)

    END SUBROUTINE WRAP_JSON_UPDATE_STRING
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_update_string]], where `path` is kind=CDK.

    SUBROUTINE JSON_UPDATE_STRING_NAME_ASCII(JSON,P,PATH,VAL,FOUND,TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH  !! path to the variable in the structure
    CHARACTER(KIND=CK, LEN=*),INTENT(IN) :: VAL   !! the new value
    LOGICAL(LK),INTENT(OUT)              :: FOUND !! if the variable was found and was a scalar.
    LOGICAL(LK),INTENT(IN),OPTIONAL     :: TRIM_STR       !! if TRIM() should be called for the `val`
!! (only used if `val` is present)
    LOGICAL(LK),INTENT(IN),OPTIONAL     :: ADJUSTL_STR    !! if ADJUSTL() should be called for the `val`
!! (only used if `val` is present)
!! (note that ADJUSTL is done before TRIM)

    CALL JSON%UPDATE(P,TO_UNICODE(PATH),VAL,FOUND,TRIM_STR,ADJUSTL_STR)

    END SUBROUTINE JSON_UPDATE_STRING_NAME_ASCII
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_update_string]], where `val` is kind=CDK.

    SUBROUTINE JSON_UPDATE_STRING_VAL_ASCII(JSON,P,PATH,VAL,FOUND,TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    CHARACTER(KIND=CK, LEN=*),INTENT(IN) :: PATH  !! path to the variable in the structure
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: VAL   !! the new value
    LOGICAL(LK),INTENT(OUT)              :: FOUND !! if the variable was found and was a scalar.
    LOGICAL(LK),INTENT(IN),OPTIONAL     :: TRIM_STR       !! if TRIM() should be called for the `val`
!! (only used if `val` is present)
    LOGICAL(LK),INTENT(IN),OPTIONAL     :: ADJUSTL_STR    !! if ADJUSTL() should be called for the `val`
!! (only used if `val` is present)
!! (note that ADJUSTL is done before TRIM)

    CALL JSON%UPDATE(P,PATH,TO_UNICODE(VAL),FOUND,TRIM_STR,ADJUSTL_STR)

    END SUBROUTINE JSON_UPDATE_STRING_VAL_ASCII
!*****************************************************************************************

!*****************************************************************************************
!>
!  Adds `member` as a child of `p`.

    SUBROUTINE JSON_VALUE_ADD_MEMBER(JSON,P,MEMBER)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT) :: JSON
    TYPE(JSON_VALUE),POINTER       :: P       !! `p` must be a `json_object`
!! or a `json_array`
    TYPE(JSON_VALUE),POINTER       :: MEMBER  !! the child member
!! to add to `p`

    INTEGER(IK) :: VAR_TYPE  !! variable type of `p`

    IF (.NOT. JSON%EXCEPTION_THROWN) THEN

        IF (ASSOCIATED(P)) THEN

            CALL JSON%INFO(P,VAR_TYPE=VAR_TYPE)

            SELECT CASE (VAR_TYPE)
            CASE(JSON_OBJECT, JSON_ARRAY)

! associate the parent
                MEMBER%PARENT => P

! add to linked list
                IF (ASSOCIATED(P%CHILDREN)) THEN
                    P%TAIL%NEXT => MEMBER
                    MEMBER%PREVIOUS => P%TAIL
                ELSE
                    P%CHILDREN => MEMBER
                    MEMBER%PREVIOUS => NULL()  !first in the list
                END IF

! new member is now the last one in the list
                P%TAIL => MEMBER
                P%N_CHILDREN = P%N_CHILDREN + 1

            CASE DEFAULT
                CALL JSON%THROW_EXCEPTION('Error in json_value_add_member: '//&
                                          'can only add child to object or array')
            END SELECT

        ELSE
            CALL JSON%THROW_EXCEPTION('Error in json_value_add_member: '//&
                                      'the pointer is not associated')
        END IF

    END IF

    END SUBROUTINE JSON_VALUE_ADD_MEMBER
!*****************************************************************************************

!*****************************************************************************************
!>
!  Inserts `element` after `p`, and updates the JSON structure accordingly.
!
!--- Example
!
!````fortran
!  program test
!   use json_module
!   implicit none
!   logical(json_LK) :: found
!   type(json_core) :: json
!   type(json_value),pointer :: p,new,element
!   call json%load(file='myfile.json', p=p)
!   call json%get(p,'x(3)',element,found) ! get pointer to an array element in the file
!   call json%create_integer(new,1,'')    ! create a new element
!   call json%insert_after(element,new)   ! insert new element after x(3)
!   call json%print(p,'myfile2.json')     ! write it to a file
!   call json%destroy(p)                  ! cleanup
!  end program test
!````
!
!--- Details
!
!  * This routine can be used to insert a new element (or set of elements)
!    into an array or object at a specific index.
!    See [[json_value_insert_after_child_by_index]]
!  * Children and subsequent elements of `element` are carried along.
!  * If the inserted elements are part of an existing list, then
!    they are removed from that list.
!
!````
!              p
!       [1] - [2] - [3] - [4]
!                 |
!                [5] - [6] - [7]        n=3 elements inserted
!              element       last
!
!  Result is:
!
!       [1] - [2] - [5] - [6] - [7] - [3] - [4]
!
!````

    SUBROUTINE JSON_VALUE_INSERT_AFTER(JSON,P,ELEMENT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT) :: JSON
    TYPE(JSON_VALUE),POINTER       :: P       !! a value from a JSON structure
!! (presumably, this is a child of
!! an object or array).
    TYPE(JSON_VALUE),POINTER       :: ELEMENT !! the element to insert after `p`

    TYPE(JSON_VALUE),POINTER :: PARENT  !! the parent of `p`
    TYPE(JSON_VALUE),POINTER :: NEXT  !! temp pointer for traversing structure
    TYPE(JSON_VALUE),POINTER :: LAST  !! the last of the items being inserted
    INTEGER :: N  !! number of items being inserted

    IF (.NOT. JSON%EXCEPTION_THROWN) THEN

        PARENT => P%PARENT

! set first parent of inserted list:
        ELEMENT%PARENT => PARENT

! Count the number of inserted elements.
! and set their parents.
        N = 1 ! initialize counter
        NEXT => ELEMENT%NEXT
        LAST => ELEMENT
        DO
            IF (.NOT. ASSOCIATED(NEXT)) EXIT
            N = N + 1
            NEXT%PARENT => PARENT
            LAST => NEXT
            NEXT => NEXT%NEXT
        END DO

        IF (ASSOCIATED(PARENT)) THEN
! update parent's child counter:
            PARENT%N_CHILDREN = PARENT%N_CHILDREN + N
! if p is last of parents children then
! also have to update parent tail pointer:
            IF (ASSOCIATED(PARENT%TAIL,P)) THEN
                PARENT%TAIL => LAST
            END IF
        END IF

        IF (ASSOCIATED(ELEMENT%PREVIOUS)) THEN
! element is apparently part of an existing list,
! so have to update that as well.
            IF (ASSOCIATED(ELEMENT%PREVIOUS%PARENT)) THEN
                ELEMENT%PREVIOUS%PARENT%N_CHILDREN = &
                    ELEMENT%PREVIOUS%PARENT%N_CHILDREN - N
                ELEMENT%PREVIOUS%PARENT%TAIL => &
                    ELEMENT%PREVIOUS ! now the last one in the list
            ELSE
! this would be a memory leak if the previous entries
! are not otherwise being pointed too
! [throw an error in this case???]
            END IF
!remove element from the other list:
            ELEMENT%PREVIOUS%NEXT => NULL()
        END IF
        ELEMENT%PREVIOUS => P

        IF (ASSOCIATED(P%NEXT)) THEN
! if there are any in the list after p:
            LAST%NEXT => P%NEXT
            LAST%NEXT%PREVIOUS => ELEMENT
        ELSE
            LAST%NEXT => NULL()
        END IF
        P%NEXT => ELEMENT

    END IF

    END SUBROUTINE JSON_VALUE_INSERT_AFTER
!*****************************************************************************************

!*****************************************************************************************
!>
!  Inserts `element` after the `idx`-th child of `p`,
!  and updates the JSON structure accordingly. This is just
!  a wrapper for [[json_value_insert_after]].

    SUBROUTINE JSON_VALUE_INSERT_AFTER_CHILD_BY_INDEX(JSON,P,IDX,ELEMENT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT) :: JSON
    TYPE(JSON_VALUE),POINTER       :: P       !! a JSON object or array.
    INTEGER(IK),INTENT(IN)         :: IDX     !! the index of the child of `p` to
!! insert the new element after
!! (this is a 1-based Fortran
!! style array index)
    TYPE(JSON_VALUE),POINTER       :: ELEMENT !! the element to insert

    TYPE(JSON_VALUE),POINTER :: TMP  !! for getting the `idx`-th child of `p`

    IF (.NOT. JSON%EXCEPTION_THROWN) THEN

! get the idx-th child of p:
        CALL JSON%GET_CHILD(P,IDX,TMP)

! call json_value_insert_after:
        IF (.NOT. JSON%EXCEPTION_THROWN) CALL JSON%INSERT_AFTER(TMP,ELEMENT)

    END IF

    END SUBROUTINE JSON_VALUE_INSERT_AFTER_CHILD_BY_INDEX
!*****************************************************************************************

!*****************************************************************************************
!>
!  Add a new member (`json_value` pointer) to a JSON structure, given the path.
!
!@warning If the path points to an existing variable in the structure,
!         then this routine will destroy it and replace it with the
!         new value.

    SUBROUTINE JSON_ADD_MEMBER_BY_PATH(JSON,ME,PATH,P,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: ME           !! the JSON structure
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    TYPE(JSON_VALUE),POINTER,INTENT(IN) :: P            !! the value to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: WAS_CREATED  !! if the variable had to be created

    TYPE(JSON_VALUE),POINTER :: TMP
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: NAME  !! name of the variable

    IF ( .NOT. JSON%EXCEPTION_THROWN ) THEN

        IF (.NOT. ASSOCIATED(P)) THEN
            CALL JSON%THROW_EXCEPTION('Error in json_add_member_by_path:'//&
                                      ' Input pointer p is not associated.',FOUND)
            IF (PRESENT(FOUND)) THEN
                FOUND = .FALSE.
                CALL JSON%CLEAR_EXCEPTIONS()
            END IF
            IF ( PRESENT(WAS_CREATED) ) WAS_CREATED = .FALSE.
        ELSE

! return a pointer to the path (possibly creating it)
            CALL JSON%CREATE(ME,PATH,TMP,FOUND,WAS_CREATED)

            IF (.NOT. ASSOCIATED(TMP)) THEN

                CALL JSON%THROW_EXCEPTION('Error in json_add_member_by_path:'//&
                                          ' Unable to resolve path: '//TRIM(PATH),FOUND)
                IF (PRESENT(FOUND)) THEN
                    FOUND = .FALSE.
                    CALL JSON%CLEAR_EXCEPTIONS()
                END IF

            ELSE

                CALL JSON%INFO(TMP,NAME=NAME)

! replace it with the new one:
                CALL JSON%REPLACE(TMP,P,DESTROY=.TRUE.)
                CALL JSON%RENAME(P,NAME)

            END IF

        END IF

    ELSE
        IF ( PRESENT(FOUND) ) FOUND = .FALSE.
        IF ( PRESENT(WAS_CREATED) ) WAS_CREATED = .FALSE.
    END IF

    END SUBROUTINE JSON_ADD_MEMBER_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper to [[json_add_member_by_path]] where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_ADD_MEMBER_BY_PATH(JSON,ME,PATH,P,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: ME           !! the JSON structure
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: P            !! the value to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! if the variable had to be created

    CALL JSON%JSON_ADD_MEMBER_BY_PATH(ME,TO_UNICODE(PATH),P,FOUND,WAS_CREATED)

    END SUBROUTINE WRAP_JSON_ADD_MEMBER_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Add an integer value to a [[json_value]], given the path.
!
!@warning If the path points to an existing variable in the structure,
!         then this routine will destroy it and replace it with the
!         new value.

    SUBROUTINE JSON_ADD_INTEGER_BY_PATH(JSON,ME,PATH,VALUE,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: ME           !! the JSON structure
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    INTEGER(IK),INTENT(IN)              :: VALUE        !! the value to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: WAS_CREATED  !! if the variable had to be created

    TYPE(JSON_VALUE),POINTER :: P
    TYPE(JSON_VALUE),POINTER :: TMP
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: NAME  !! variable name

    IF ( .NOT. JSON%EXCEPTION_THROWN ) THEN

        NULLIFY(P)

! return a pointer to the path (possibly creating it)
! If the variable had to be created, then
! it will be a json_null variable.
        CALL JSON%CREATE(ME,PATH,P,FOUND,WAS_CREATED)

        IF (.NOT. ASSOCIATED(P)) THEN

            CALL JSON%THROW_EXCEPTION('Error in json_add_integer_by_path:'//&
                                      ' Unable to resolve path: '//TRIM(PATH),FOUND)
            IF (PRESENT(FOUND)) THEN
                FOUND = .FALSE.
                CALL JSON%CLEAR_EXCEPTIONS()
            END IF

        ELSE

!NOTE: a new object is created, and the old one
!      is replaced and destroyed. This is to
!      prevent memory leaks if the type is
!      being changed (for example, if an array
!      is being replaced with a scalar).

            IF (P%VAR_TYPE==JSON_INTEGER) THEN
                P%INT_VALUE = VALUE
            ELSE
                CALL JSON%INFO(P,NAME=NAME)
                CALL JSON%CREATE_INTEGER(TMP,VALUE,NAME)
                CALL JSON%REPLACE(P,TMP,DESTROY=.TRUE.)
            END IF

        END IF

    ELSE
        IF ( PRESENT(FOUND) )       FOUND = .FALSE.
        IF ( PRESENT(WAS_CREATED) ) WAS_CREATED = .FALSE.
    END IF

    END SUBROUTINE JSON_ADD_INTEGER_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper to [[json_add_integer_by_path]] where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_ADD_INTEGER_BY_PATH(JSON,ME,PATH,VALUE,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: ME          !! the JSON structure
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH        !! the path to the variable
    INTEGER(IK),INTENT(IN)               :: VALUE       !! the value to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND       !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED !! if the variable had to be created

    CALL JSON%JSON_ADD_INTEGER_BY_PATH(ME,TO_UNICODE(PATH),VALUE,FOUND,WAS_CREATED)

    END SUBROUTINE WRAP_JSON_ADD_INTEGER_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Add an real value to a [[json_value]], given the path.
!
!@warning If the path points to an existing variable in the structure,
!         then this routine will destroy it and replace it with the
!         new value.

    SUBROUTINE JSON_ADD_REAL_BY_PATH(JSON,ME,PATH,VALUE,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: ME           !! the JSON structure
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    REAL(RK),INTENT(IN)                 :: VALUE        !! the value to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: WAS_CREATED  !! if the variable had to be created

    TYPE(JSON_VALUE),POINTER :: P
    TYPE(JSON_VALUE),POINTER :: TMP
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: NAME  !! variable name

    IF ( .NOT. JSON%EXCEPTION_THROWN ) THEN

        NULLIFY(P)

! return a pointer to the path (possibly creating it)
! If the variable had to be created, then
! it will be a json_null variable.
        CALL JSON%CREATE(ME,PATH,P,FOUND,WAS_CREATED)

        IF (.NOT. ASSOCIATED(P)) THEN

            CALL JSON%THROW_EXCEPTION('Error in json_add_real_by_path:'//&
                                      ' Unable to resolve path: '//TRIM(PATH),FOUND)
            IF (PRESENT(FOUND)) THEN
                FOUND = .FALSE.
                CALL JSON%CLEAR_EXCEPTIONS()
            END IF

        ELSE

!NOTE: a new object is created, and the old one
!      is replaced and destroyed. This is to
!      prevent memory leaks if the type is
!      being changed (for example, if an array
!      is being replaced with a scalar).

            IF (P%VAR_TYPE==JSON_REAL) THEN
                P%DBL_VALUE = VALUE
            ELSE
                CALL JSON%INFO(P,NAME=NAME)
                CALL JSON%CREATE_REAL(TMP,VALUE,NAME)
                CALL JSON%REPLACE(P,TMP,DESTROY=.TRUE.)
            END IF

        END IF

    ELSE
        IF ( PRESENT(FOUND) )       FOUND = .FALSE.
        IF ( PRESENT(WAS_CREATED) ) WAS_CREATED = .FALSE.
    END IF

    END SUBROUTINE JSON_ADD_REAL_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper to [[json_add_real_by_path]] where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_ADD_REAL_BY_PATH(JSON,ME,PATH,VALUE,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: ME          !! the JSON structure
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH        !! the path to the variable
    REAL(RK),INTENT(IN)                  :: VALUE       !! the value to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND       !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED !! if the variable had to be created

    CALL JSON%JSON_ADD_REAL_BY_PATH(ME,TO_UNICODE(PATH),VALUE,FOUND,WAS_CREATED)

    END SUBROUTINE WRAP_JSON_ADD_REAL_BY_PATH
!*****************************************************************************************


!*****************************************************************************************
!>
!  Alternate version of [[json_add_real_by_path]] where value=real32.

    SUBROUTINE JSON_ADD_REAL32_BY_PATH(JSON,ME,PATH,VALUE,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: ME           !! the JSON structure
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    REAL(REAL32),INTENT(IN)             :: VALUE        !! the value to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: WAS_CREATED  !! if the variable had to be created

    CALL JSON%ADD_BY_PATH(ME,PATH,REAL(VALUE,RK),FOUND,WAS_CREATED)

    END SUBROUTINE JSON_ADD_REAL32_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper to [[json_add_real32_by_path]] where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_ADD_REAL32_BY_PATH(JSON,ME,PATH,VALUE,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: ME          !! the JSON structure
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH        !! the path to the variable
    REAL(REAL32),INTENT(IN)              :: VALUE       !! the value to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND       !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED !! if the variable had to be created

    CALL JSON%ADD_BY_PATH(ME,TO_UNICODE(PATH),REAL(VALUE,RK),FOUND,WAS_CREATED)

    END SUBROUTINE WRAP_JSON_ADD_REAL32_BY_PATH
!*****************************************************************************************


!- 3942


!*****************************************************************************************
!>
!  Add a logical value to a [[json_value]], given the path.
!
!@warning If the path points to an existing variable in the structure,
!         then this routine will destroy it and replace it with the
!         new value.

    SUBROUTINE JSON_ADD_LOGICAL_BY_PATH(JSON,ME,PATH,VALUE,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: ME           !! the JSON structure
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    LOGICAL(LK),INTENT(IN)              :: VALUE        !! the value to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: WAS_CREATED  !! if the variable had to be created

    TYPE(JSON_VALUE),POINTER :: P
    TYPE(JSON_VALUE),POINTER :: TMP
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: NAME  !! variable name

    IF ( .NOT. JSON%EXCEPTION_THROWN ) THEN

        NULLIFY(P)

! return a pointer to the path (possibly creating it)
! If the variable had to be created, then
! it will be a json_null variable.
        CALL JSON%CREATE(ME,PATH,P,FOUND,WAS_CREATED)

        IF (.NOT. ASSOCIATED(P)) THEN

            CALL JSON%THROW_EXCEPTION('Error in json_add_logical_by_path:'//&
                                      ' Unable to resolve path: '//TRIM(PATH),FOUND)
            IF (PRESENT(FOUND)) THEN
                FOUND = .FALSE.
                CALL JSON%CLEAR_EXCEPTIONS()
            END IF

        ELSE

!NOTE: a new object is created, and the old one
!      is replaced and destroyed. This is to
!      prevent memory leaks if the type is
!      being changed (for example, if an array
!      is being replaced with a scalar).

            IF (P%VAR_TYPE==JSON_LOGICAL) THEN
                P%LOG_VALUE = VALUE
            ELSE
                CALL JSON%INFO(P,NAME=NAME)
                CALL JSON%CREATE_LOGICAL(TMP,VALUE,NAME)
                CALL JSON%REPLACE(P,TMP,DESTROY=.TRUE.)
            END IF

        END IF

    ELSE
        IF ( PRESENT(FOUND) )       FOUND = .FALSE.
        IF ( PRESENT(WAS_CREATED) ) WAS_CREATED = .FALSE.
    END IF

    END SUBROUTINE JSON_ADD_LOGICAL_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper to [[json_add_logical_by_path]] where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_ADD_LOGICAL_BY_PATH(JSON,ME,PATH,VALUE,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: ME          !! the JSON structure
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH        !! the path to the variable
    LOGICAL(LK),INTENT(IN)               :: VALUE       !! the value to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND       !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED !! if the variable had to be created

    CALL JSON%JSON_ADD_LOGICAL_BY_PATH(ME,TO_UNICODE(PATH),VALUE,FOUND,WAS_CREATED)

    END SUBROUTINE WRAP_JSON_ADD_LOGICAL_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Add a string value to a [[json_value]], given the path.
!
!@warning If the path points to an existing variable in the structure,
!         then this routine will destroy it and replace it with the
!         new value.

    SUBROUTINE JSON_ADD_STRING_BY_PATH(JSON,ME,PATH,VALUE,FOUND,&
                                            WAS_CREATED,TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: ME           !! the JSON structure
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: VALUE        !! the value to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: WAS_CREATED  !! if the variable had to be created
    LOGICAL(LK),INTENT(IN),OPTIONAL     :: TRIM_STR     !! if TRIM() should be called for each element
    LOGICAL(LK),INTENT(IN),OPTIONAL     :: ADJUSTL_STR  !! if ADJUSTL() should be called for each element

    TYPE(JSON_VALUE),POINTER :: P
    TYPE(JSON_VALUE),POINTER :: TMP
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: NAME  !! variable name

    IF ( .NOT. JSON%EXCEPTION_THROWN ) THEN

        NULLIFY(P)

! return a pointer to the path (possibly creating it)
! If the variable had to be created, then
! it will be a json_null variable.
        CALL JSON%CREATE(ME,PATH,P,FOUND,WAS_CREATED)

        IF (.NOT. ASSOCIATED(P)) THEN

            CALL JSON%THROW_EXCEPTION('Error in json_add_string_by_path:'//&
                                      ' Unable to resolve path: '//TRIM(PATH),FOUND)
            IF (PRESENT(FOUND)) THEN
                FOUND = .FALSE.
                CALL JSON%CLEAR_EXCEPTIONS()
            END IF

        ELSE

!NOTE: a new object is created, and the old one
!      is replaced and destroyed. This is to
!      prevent memory leaks if the type is
!      being changed (for example, if an array
!      is being replaced with a scalar).

            IF (P%VAR_TYPE==JSON_STRING) THEN
                P%STR_VALUE = VALUE
            ELSE
                CALL JSON%INFO(P,NAME=NAME)
                CALL JSON%CREATE_STRING(TMP,VALUE,NAME,TRIM_STR,ADJUSTL_STR)
                CALL JSON%REPLACE(P,TMP,DESTROY=.TRUE.)
            END IF

        END IF

    ELSE
        IF ( PRESENT(FOUND) )       FOUND = .FALSE.
        IF ( PRESENT(WAS_CREATED) ) WAS_CREATED = .FALSE.
    END IF

    END SUBROUTINE JSON_ADD_STRING_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper to [[json_add_string_by_path]] where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_ADD_STRING_BY_PATH(JSON,ME,PATH,VALUE,FOUND,&
                                                WAS_CREATED,TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: ME          !! the JSON structure
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH        !! the path to the variable
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: VALUE       !! the value to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND       !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED !! if the variable had to be created
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: TRIM_STR    !! if TRIM() should be called for each element
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: ADJUSTL_STR !! if ADJUSTL() should be called for each element

    CALL JSON%JSON_ADD_STRING_BY_PATH(ME,TO_UNICODE(PATH),TO_UNICODE(VALUE),&
                                        FOUND,WAS_CREATED,TRIM_STR,ADJUSTL_STR)

    END SUBROUTINE WRAP_JSON_ADD_STRING_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper for [[json_add_string_by_path]] where "path" is kind=CDK.

    SUBROUTINE JSON_ADD_STRING_BY_PATH_PATH_ASCII(JSON,ME,PATH,VALUE,FOUND,&
                                                    WAS_CREATED,TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: ME           !! the JSON structure
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)  :: VALUE        !! the value to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! if the variable had to be created
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: TRIM_STR     !! if TRIM() should be called for each element
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: ADJUSTL_STR  !! if ADJUSTL() should be called for each element

    CALL JSON%JSON_ADD_STRING_BY_PATH(ME,TO_UNICODE(PATH),VALUE,FOUND,WAS_CREATED,TRIM_STR,ADJUSTL_STR)

    END SUBROUTINE JSON_ADD_STRING_BY_PATH_PATH_ASCII
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper for [[json_add_string_by_path]] where "value" is kind=CDK.

    SUBROUTINE JSON_ADD_STRING_BY_PATH_VALUE_ASCII(JSON,ME,PATH,VALUE,FOUND,&
                                                        WAS_CREATED,TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: ME           !! the JSON structure
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)  :: PATH         !! the path to the variable
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: VALUE        !! the value to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! if the variable had to be created
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: TRIM_STR     !! if TRIM() should be called for each element
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: ADJUSTL_STR  !! if ADJUSTL() should be called for each element

    CALL JSON%JSON_ADD_STRING_BY_PATH(ME,PATH,TO_UNICODE(VALUE),FOUND,WAS_CREATED,TRIM_STR,ADJUSTL_STR)

    END SUBROUTINE JSON_ADD_STRING_BY_PATH_VALUE_ASCII
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper to [[json_add_integer_by_path]] for adding an integer vector by path.

    SUBROUTINE JSON_ADD_INTEGER_VEC_BY_PATH(JSON,ME,PATH,VALUE,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: ME           !! the JSON structure
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    INTEGER(IK),DIMENSION(:),INTENT(IN) :: VALUE        !! the vector to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: WAS_CREATED  !! if the variable had to be created

    TYPE(JSON_VALUE),POINTER :: P   !! pointer to path (which may exist)
    TYPE(JSON_VALUE),POINTER :: VAR !! new variable that is created
    INTEGER(IK) :: I    !! counter
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: NAME !! the variable name
    LOGICAL(LK) :: P_FOUND  !! if the path was successfully found (or created)

    IF ( .NOT. JSON%EXCEPTION_THROWN ) THEN

!get a pointer to the variable
!(creating it if necessary)
        CALL JSON%CREATE(ME,PATH,P,FOUND=P_FOUND)
        IF (P_FOUND) THEN
            CALL JSON%INFO(P,NAME=NAME)             ! want to keep the existing name
            CALL JSON%CREATE_ARRAY(VAR,NAME)        ! create a new array variable
            CALL JSON%REPLACE(P,VAR,DESTROY=.TRUE.) ! replace p with this array (destroy p)
!populate each element of the array:
            DO I=1,SIZE(VALUE)
                CALL JSON%ADD(VAR, CK_'', VALUE(I))
            END DO
        END IF

    ELSE
        IF ( PRESENT(FOUND) )       FOUND = .FALSE.
        IF ( PRESENT(WAS_CREATED) ) WAS_CREATED = .FALSE.
    END IF

    END SUBROUTINE JSON_ADD_INTEGER_VEC_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper for [[json_add_integer_vec_by_path]] where "path" is kind=CDK).

    SUBROUTINE WRAP_JSON_ADD_INTEGER_VEC_BY_PATH(JSON,ME,PATH,VALUE,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: ME           !! the JSON structure
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    INTEGER(IK),DIMENSION(:),INTENT(IN)  :: VALUE        !! the vector to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! if the variable had to be created

    CALL JSON%JSON_ADD_INTEGER_VEC_BY_PATH(ME,TO_UNICODE(PATH),VALUE,FOUND,WAS_CREATED)

    END SUBROUTINE WRAP_JSON_ADD_INTEGER_VEC_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper to [[json_add_logical_by_path]] for adding a logical vector by path.

    SUBROUTINE JSON_ADD_LOGICAL_VEC_BY_PATH(JSON,ME,PATH,VALUE,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: ME           !! the JSON structure
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    LOGICAL(LK),DIMENSION(:),INTENT(IN) :: VALUE        !! the vector to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: WAS_CREATED  !! if the variable had to be created

    TYPE(JSON_VALUE),POINTER :: P   !! pointer to path (which may exist)
    TYPE(JSON_VALUE),POINTER :: VAR !! new variable that is created
    INTEGER(IK) :: I    !! counter
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: NAME !! the variable name
    LOGICAL(LK) :: P_FOUND  !! if the path was successfully found (or created)

    IF ( .NOT. JSON%EXCEPTION_THROWN ) THEN

!get a pointer to the variable
!(creating it if necessary)
        CALL JSON%CREATE(ME,PATH,P,FOUND=P_FOUND)
        IF (P_FOUND) THEN
            CALL JSON%INFO(P,NAME=NAME)             ! want to keep the existing name
            CALL JSON%CREATE_ARRAY(VAR,NAME)        ! create a new array variable
            CALL JSON%REPLACE(P,VAR,DESTROY=.TRUE.) ! replace p with this array (destroy p)
!populate each element of the array:
            DO I=1,SIZE(VALUE)
                CALL JSON%ADD(VAR, CK_'', VALUE(I))
            END DO
        END IF

    ELSE
        IF ( PRESENT(FOUND) )       FOUND = .FALSE.
        IF ( PRESENT(WAS_CREATED) ) WAS_CREATED = .FALSE.
    END IF

    END SUBROUTINE JSON_ADD_LOGICAL_VEC_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper for [[json_add_logical_vec_by_path]] where "path" is kind=CDK).

    SUBROUTINE WRAP_JSON_ADD_LOGICAL_VEC_BY_PATH(JSON,ME,PATH,VALUE,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: ME           !! the JSON structure
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    LOGICAL(LK),DIMENSION(:),INTENT(IN)  :: VALUE        !! the vector to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! if the variable had to be created

    CALL JSON%JSON_ADD_LOGICAL_VEC_BY_PATH(ME,TO_UNICODE(PATH),VALUE,FOUND,WAS_CREATED)

    END SUBROUTINE WRAP_JSON_ADD_LOGICAL_VEC_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper to [[json_add_real_by_path]] for adding a real vector by path.

    SUBROUTINE JSON_ADD_REAL_VEC_BY_PATH(JSON,ME,PATH,VALUE,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: ME           !! the JSON structure
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    REAL(RK),DIMENSION(:),INTENT(IN)    :: VALUE        !! the vector to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: WAS_CREATED  !! if the variable had to be created

    TYPE(JSON_VALUE),POINTER :: P   !! pointer to path (which may exist)
    TYPE(JSON_VALUE),POINTER :: VAR !! new variable that is created
    INTEGER(IK) :: I    !! counter
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: NAME !! the variable name
    LOGICAL(LK) :: P_FOUND  !! if the path was successfully found (or created)

    IF ( .NOT. JSON%EXCEPTION_THROWN ) THEN

!get a pointer to the variable
!(creating it if necessary)
        CALL JSON%CREATE(ME,PATH,P,FOUND=P_FOUND)
        IF (P_FOUND) THEN
            CALL JSON%INFO(P,NAME=NAME)             ! want to keep the existing name
            CALL JSON%CREATE_ARRAY(VAR,NAME)        ! create a new array variable
            CALL JSON%REPLACE(P,VAR,DESTROY=.TRUE.) ! replace p with this array (destroy p)
!populate each element of the array:
            DO I=1,SIZE(VALUE)
                CALL JSON%ADD(VAR, CK_'', VALUE(I))
            END DO
        END IF

    ELSE
        IF ( PRESENT(FOUND) )       FOUND = .FALSE.
        IF ( PRESENT(WAS_CREATED) ) WAS_CREATED = .FALSE.
    END IF

    END SUBROUTINE JSON_ADD_REAL_VEC_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper for [[json_add_real_vec_by_path]] where "path" is kind=CDK).

    SUBROUTINE WRAP_JSON_ADD_REAL_VEC_BY_PATH(JSON,ME,PATH,VALUE,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: ME           !! the JSON structure
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    REAL(RK),DIMENSION(:),INTENT(IN)     :: VALUE        !! the vector to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! if the variable had to be created

    CALL JSON%JSON_ADD_REAL_VEC_BY_PATH(ME,TO_UNICODE(PATH),VALUE,FOUND,WAS_CREATED)

    END SUBROUTINE WRAP_JSON_ADD_REAL_VEC_BY_PATH
!*****************************************************************************************


!*****************************************************************************************
!>
!  Wrapper to [[json_add_real_by_path]] for adding a real vector by path.

    SUBROUTINE JSON_ADD_REAL32_VEC_BY_PATH(JSON,ME,PATH,VALUE,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: ME           !! the JSON structure
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)  :: PATH         !! the path to the variable
    REAL(REAL32),DIMENSION(:),INTENT(IN) :: VALUE        !! the vector to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! if the variable had to be created

    CALL JSON%ADD_BY_PATH(ME,PATH,REAL(VALUE,RK),FOUND,WAS_CREATED)

    END SUBROUTINE JSON_ADD_REAL32_VEC_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper for [[json_add_real32_vec_by_path]] where "path" is kind=CDK).

    SUBROUTINE WRAP_JSON_ADD_REAL32_VEC_BY_PATH(JSON,ME,PATH,VALUE,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: ME           !! the JSON structure
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    REAL(REAL32),DIMENSION(:),INTENT(IN) :: VALUE        !! the vector to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! if the variable had to be created

    CALL JSON%ADD_BY_PATH(ME,TO_UNICODE(PATH),REAL(VALUE,RK),FOUND,WAS_CREATED)

    END SUBROUTINE WRAP_JSON_ADD_REAL32_VEC_BY_PATH
!*****************************************************************************************


!- 4445


!*****************************************************************************************
!>
!  Wrapper to [[json_add_string_by_path]] for adding a string vector by path.
!
!@note The `ilen` input can be used to specify the actual lengths of the
!      the strings in the array. They must all be `<= len(value)`.

    SUBROUTINE JSON_ADD_STRING_VEC_BY_PATH(JSON,ME,PATH,VALUE,FOUND,WAS_CREATED,ILEN,TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: ME           !! the JSON structure
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH         !! the path to the variable
    CHARACTER(KIND=CK,LEN=*),DIMENSION(:),INTENT(IN) :: VALUE !! the vector to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: WAS_CREATED  !! if the variable had to be created
    INTEGER(IK),DIMENSION(:),INTENT(IN),OPTIONAL :: ILEN !! the string lengths of each
!! element in `value`. If not present,
!! the full `len(value)` string is added
!! for each element.
    LOGICAL(LK),INTENT(IN),OPTIONAL     :: TRIM_STR      !! if TRIM() should be called for each element
    LOGICAL(LK),INTENT(IN),OPTIONAL     :: ADJUSTL_STR   !! if ADJUSTL() should be called for each element

    TYPE(JSON_VALUE),POINTER :: P   !! pointer to path (which may exist)
    TYPE(JSON_VALUE),POINTER :: VAR !! new variable that is created
    INTEGER(IK) :: I    !! counter
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: NAME !! the variable name
    LOGICAL(LK) :: P_FOUND  !! if the path was successfully found (or created)

    IF ( .NOT. JSON%EXCEPTION_THROWN ) THEN

! validate ilen array if present:
        IF (PRESENT(ILEN)) THEN
            IF (SIZE(ILEN)/=SIZE(VALUE)) THEN
                CALL JSON%THROW_EXCEPTION('Error in json_add_string_vec_by_path: '//&
                                          'Invalid size of ilen input vector.',FOUND)
                IF (PRESENT(FOUND)) THEN
                    FOUND = .FALSE.
                    CALL JSON%CLEAR_EXCEPTIONS()
                END IF
                IF (PRESENT(WAS_CREATED)) WAS_CREATED = .FALSE.
                RETURN
            ELSE
! also have to validate the specified lengths.
! (must not be greater than input string length)
                DO I = 1, SIZE(VALUE)
                    IF (ILEN(I)>LEN(VALUE)) THEN
                        CALL JSON%THROW_EXCEPTION('Error in json_add_string_vec_by_path: '//&
                                                  'Invalid ilen element.',FOUND)
                        IF (PRESENT(FOUND)) THEN
                            FOUND = .FALSE.
                            CALL JSON%CLEAR_EXCEPTIONS()
                        END IF
                        IF (PRESENT(WAS_CREATED)) WAS_CREATED = .FALSE.
                        RETURN
                    END IF
                END DO
            END IF
        END IF

!get a pointer to the variable
!(creating it if necessary)
        CALL JSON%CREATE(ME,PATH,P,FOUND=P_FOUND)
        IF (P_FOUND) THEN
            CALL JSON%INFO(P,NAME=NAME)             ! want to keep the existing name
            CALL JSON%CREATE_ARRAY(VAR,NAME)        ! create a new array variable
            CALL JSON%REPLACE(P,VAR,DESTROY=.TRUE.) ! replace p with this array (destroy p)
!populate each element of the array:
            DO I=1,SIZE(VALUE)
                IF (PRESENT(ILEN)) THEN
                    CALL JSON%ADD(VAR, CK_'', VALUE(I)(1:ILEN(I)), &
                                  TRIM_STR=TRIM_STR, ADJUSTL_STR=ADJUSTL_STR)
                ELSE
                    CALL JSON%ADD(VAR, CK_'', VALUE(I), &
                                  TRIM_STR=TRIM_STR, ADJUSTL_STR=ADJUSTL_STR)
                END IF
            END DO
        END IF

    ELSE
        IF ( PRESENT(FOUND) )       FOUND = .FALSE.
        IF ( PRESENT(WAS_CREATED) ) WAS_CREATED = .FALSE.
    END IF

    END SUBROUTINE JSON_ADD_STRING_VEC_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper for [[json_add_string_vec_by_path]] where "path" and "value" are kind=CDK).

    SUBROUTINE WRAP_JSON_ADD_STRING_VEC_BY_PATH(JSON,ME,PATH,VALUE,&
                                                FOUND,WAS_CREATED,ILEN,&
                                                TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                   :: JSON
    TYPE(JSON_VALUE),POINTER                         :: ME           !! the JSON structure
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN)             :: PATH         !! the path to the variable
    CHARACTER(KIND=CDK,LEN=*),DIMENSION(:),INTENT(IN):: VALUE        !! the vector to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL                 :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL                 :: WAS_CREATED  !! if the variable had to be created
    INTEGER(IK),DIMENSION(:),INTENT(IN),OPTIONAL :: ILEN !! the string lengths of each
!! element in `value`. If not present,
!! the full `len(value)` string is added
!! for each element.
    LOGICAL(LK),INTENT(IN),OPTIONAL  :: TRIM_STR         !! if TRIM() should be called for each element
    LOGICAL(LK),INTENT(IN),OPTIONAL  :: ADJUSTL_STR      !! if ADJUSTL() should be called for each element

    CALL JSON%JSON_ADD_STRING_VEC_BY_PATH(ME,TO_UNICODE(PATH),TO_UNICODE(VALUE),&
                                            FOUND,WAS_CREATED,ILEN,TRIM_STR,ADJUSTL_STR)

    END SUBROUTINE WRAP_JSON_ADD_STRING_VEC_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper for [[json_add_string_vec_by_path]] where "value" is kind=CDK).

    SUBROUTINE JSON_ADD_STRING_VEC_BY_PATH_VALUE_ASCII(JSON,ME,PATH,VALUE,&
                                                        FOUND,WAS_CREATED,ILEN,&
                                                        TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                   :: JSON
    TYPE(JSON_VALUE),POINTER                         :: ME           !! the JSON structure
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)              :: PATH         !! the path to the variable
    CHARACTER(KIND=CDK,LEN=*),DIMENSION(:),INTENT(IN):: VALUE        !! the vector to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL                 :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL                 :: WAS_CREATED  !! if the variable had to be created
    INTEGER(IK),DIMENSION(:),INTENT(IN),OPTIONAL :: ILEN !! the string lengths of each
!! element in `value`. If not present,
!! the full `len(value)` string is added
!! for each element.
    LOGICAL(LK),INTENT(IN),OPTIONAL  :: TRIM_STR         !! if TRIM() should be called for each element
    LOGICAL(LK),INTENT(IN),OPTIONAL  :: ADJUSTL_STR      !! if ADJUSTL() should be called for each element

    CALL JSON%JSON_ADD_STRING_VEC_BY_PATH(ME,PATH,TO_UNICODE(VALUE),&
                                            FOUND,WAS_CREATED,ILEN,TRIM_STR,ADJUSTL_STR)

    END SUBROUTINE JSON_ADD_STRING_VEC_BY_PATH_VALUE_ASCII
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper for [[json_add_string_vec_by_path]] where "path" is kind=CDK).

    SUBROUTINE JSON_ADD_STRING_VEC_BY_PATH_PATH_ASCII(JSON,ME,PATH,VALUE,&
                                                        FOUND,WAS_CREATED,ILEN,&
                                                        TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                   :: JSON
    TYPE(JSON_VALUE),POINTER                         :: ME           !! the JSON structure
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN)             :: PATH         !! the path to the variable
    CHARACTER(KIND=CK,LEN=*),DIMENSION(:),INTENT(IN) :: VALUE        !! the vector to add
    LOGICAL(LK),INTENT(OUT),OPTIONAL                 :: FOUND        !! if the variable was found
    LOGICAL(LK),INTENT(OUT),OPTIONAL                 :: WAS_CREATED  !! if the variable had to be created
    INTEGER(IK),DIMENSION(:),INTENT(IN),OPTIONAL :: ILEN !! the string lengths of each
!! element in `value`. If not present,
!! the full `len(value)` string is added
!! for each element.
    LOGICAL(LK),INTENT(IN),OPTIONAL  :: TRIM_STR         !! if TRIM() should be called for each element
    LOGICAL(LK),INTENT(IN),OPTIONAL  :: ADJUSTL_STR      !! if ADJUSTL() should be called for each element

    CALL JSON%JSON_ADD_STRING_VEC_BY_PATH(ME,TO_UNICODE(PATH),VALUE,&
                                            FOUND,WAS_CREATED,ILEN,TRIM_STR,ADJUSTL_STR)

    END SUBROUTINE JSON_ADD_STRING_VEC_BY_PATH_PATH_ASCII
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/19/2014
!
!  Add a real value child to the [[json_value]] variable.
!
!@note This routine is part of the public API that can be
!      used to build a JSON structure using [[json_value]] pointers.

    SUBROUTINE JSON_VALUE_ADD_REAL(JSON,P,NAME,VAL)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: NAME  !! variable name
    REAL(RK),INTENT(IN)                 :: VAL   !! real value

    TYPE(JSON_VALUE),POINTER :: VAR

!create the variable:
    CALL JSON%CREATE_REAL(VAR,VAL,NAME)

!add it:
    CALL JSON%ADD(P, VAR)

    END SUBROUTINE JSON_VALUE_ADD_REAL
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_real]] where `name` is kind=CDK.

    SUBROUTINE WRAP_JSON_VALUE_ADD_REAL(JSON,P,NAME,VAL)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: NAME  !! variable name
    REAL(RK),INTENT(IN)                  :: VAL   !! real value

    CALL JSON%ADD(P, TO_UNICODE(NAME), VAL)

    END SUBROUTINE WRAP_JSON_VALUE_ADD_REAL
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/20/2014
!
!  Add a real vector child to the [[json_value]] variable.
!
!@note This routine is part of the public API that can be
!      used to build a JSON structure using [[json_value]] pointers.

    SUBROUTINE JSON_VALUE_ADD_REAL_VEC(JSON, P, NAME, VAL)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: NAME
    REAL(RK),DIMENSION(:),INTENT(IN)    :: VAL

    TYPE(JSON_VALUE),POINTER :: VAR
    INTEGER(IK) :: I !! counter

!create the variable as an array:
    CALL JSON%CREATE_ARRAY(VAR,NAME)

!populate the array:
    DO I=1,SIZE(VAL)
        CALL JSON%ADD(VAR, CK_'', VAL(I))
    END DO

!add it:
    CALL JSON%ADD(P, VAR)

    END SUBROUTINE JSON_VALUE_ADD_REAL_VEC
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_real_vec]] where `name` is kind=CDK.

    SUBROUTINE WRAP_JSON_VALUE_ADD_REAL_VEC(JSON, P, NAME, VAL)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: NAME
    REAL(RK),DIMENSION(:),INTENT(IN)     :: VAL

    CALL JSON%ADD(P, TO_UNICODE(NAME), VAL)

    END SUBROUTINE WRAP_JSON_VALUE_ADD_REAL_VEC
!*****************************************************************************************


!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_real]] where `val` is `real32`.

    SUBROUTINE JSON_VALUE_ADD_REAL32(JSON,P,NAME,VAL)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: NAME  !! variable name
    REAL(REAL32),INTENT(IN)             :: VAL   !! real value

    CALL JSON%ADD(P,NAME,REAL(VAL,RK))

    END SUBROUTINE JSON_VALUE_ADD_REAL32
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_real32]] where `name` is kind=CDK.

    SUBROUTINE WRAP_JSON_VALUE_ADD_REAL32(JSON,P,NAME,VAL)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: NAME  !! variable name
    REAL(REAL32),INTENT(IN)              :: VAL   !! real value

    CALL JSON%ADD(P, TO_UNICODE(NAME), VAL)

    END SUBROUTINE WRAP_JSON_VALUE_ADD_REAL32
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_real_vec]] where `val` is `real32`.

    SUBROUTINE JSON_VALUE_ADD_REAL32_VEC(JSON, P, NAME, VAL)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)  :: NAME
    REAL(REAL32),DIMENSION(:),INTENT(IN) :: VAL

    CALL JSON%ADD(P,NAME,REAL(VAL,RK))

    END SUBROUTINE JSON_VALUE_ADD_REAL32_VEC
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_real32_vec]] where `name` is kind=CDK.

    SUBROUTINE WRAP_JSON_VALUE_ADD_REAL32_VEC(JSON, P, NAME, VAL)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: NAME
    REAL(REAL32),DIMENSION(:),INTENT(IN) :: VAL

    CALL JSON%ADD(P, TO_UNICODE(NAME), VAL)

    END SUBROUTINE WRAP_JSON_VALUE_ADD_REAL32_VEC
!*****************************************************************************************


!- 4868


!*****************************************************************************************
!>
!  Add a NULL value child to the [[json_value]] variable.
!
!@note This routine is part of the public API that can be
!      used to build a JSON structure using [[json_value]] pointers.

    SUBROUTINE JSON_VALUE_ADD_NULL(JSON, P, NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: NAME

    TYPE(JSON_VALUE),POINTER :: VAR

!create the variable:
    CALL JSON%CREATE_NULL(VAR,NAME)

!add it:
    CALL JSON%ADD(P, VAR)

    END SUBROUTINE JSON_VALUE_ADD_NULL
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_null]] where `name` is kind=CDK.

    SUBROUTINE WRAP_JSON_VALUE_ADD_NULL(JSON, P, NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: NAME   !! name of the variable

    CALL JSON%ADD(P, TO_UNICODE(NAME))

    END SUBROUTINE WRAP_JSON_VALUE_ADD_NULL
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/20/2014
!
!  Add an integer value child to the [[json_value]] variable.
!
!@note This routine is part of the public API that can be
!      used to build a JSON structure using [[json_value]] pointers.

    SUBROUTINE JSON_VALUE_ADD_INTEGER(JSON, P, NAME, VAL)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: NAME
    INTEGER(IK),INTENT(IN)              :: VAL

    TYPE(JSON_VALUE),POINTER :: VAR

!create the variable:
    CALL JSON%CREATE_INTEGER(VAR,VAL,NAME)

!add it:
    CALL JSON%ADD(P, VAR)

    END SUBROUTINE JSON_VALUE_ADD_INTEGER
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_integer]] where `name` is kind=CDK.

    SUBROUTINE WRAP_JSON_VALUE_ADD_INTEGER(JSON, P, NAME, VAL)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: NAME   !! name of the variable
    INTEGER(IK),INTENT(IN)               :: VAL    !! value

    CALL JSON%ADD(P, TO_UNICODE(NAME), VAL)

    END SUBROUTINE WRAP_JSON_VALUE_ADD_INTEGER
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/20/2014
!
!  Add a integer vector child to the [[json_value]] variable.
!
!@note This routine is part of the public API that can be
!      used to build a JSON structure using [[json_value]] pointers.

    SUBROUTINE JSON_VALUE_ADD_INTEGER_VEC(JSON, P, NAME, VAL)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: NAME   !! name of the variable
    INTEGER(IK),DIMENSION(:),INTENT(IN) :: VAL    !! value

    TYPE(JSON_VALUE),POINTER :: VAR
    INTEGER(IK) :: I    !! counter

!create a variable as an array:
    CALL JSON%CREATE_ARRAY(VAR,NAME)

!populate the array:
    DO I=1,SIZE(VAL)
        CALL JSON%ADD(VAR, CK_'', VAL(I))
    END DO

!add it:
    CALL JSON%ADD(P, VAR)

    END SUBROUTINE JSON_VALUE_ADD_INTEGER_VEC
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_integer_vec]] where `name` is kind=CDK.

    SUBROUTINE WRAP_JSON_VALUE_ADD_INTEGER_VEC(JSON, P, NAME, VAL)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: NAME   !! name of the variable
    INTEGER(IK),DIMENSION(:),INTENT(IN)  :: VAL    !! value

    CALL JSON%ADD(P, TO_UNICODE(NAME), VAL)

    END SUBROUTINE WRAP_JSON_VALUE_ADD_INTEGER_VEC
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/20/2014
!
!  Add a logical value child to the [[json_value]] variable.
!
!@note This routine is part of the public API that can be
!      used to build a JSON structure using [[json_value]] pointers.

    SUBROUTINE JSON_VALUE_ADD_LOGICAL(JSON, P, NAME, VAL)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: NAME   !! name of the variable
    LOGICAL(LK),INTENT(IN)              :: VAL    !! value

    TYPE(JSON_VALUE),POINTER :: VAR

!create the variable:
    CALL JSON%CREATE_LOGICAL(VAR,VAL,NAME)

!add it:
    CALL JSON%ADD(P, VAR)

    END SUBROUTINE JSON_VALUE_ADD_LOGICAL
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_logical]] where `name` is kind=CDK.

    SUBROUTINE WRAP_JSON_VALUE_ADD_LOGICAL(JSON, P, NAME, VAL)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: NAME   !! name of the variable
    LOGICAL(LK),INTENT(IN)               :: VAL    !! value

    CALL JSON%ADD(P, TO_UNICODE(NAME), VAL)

    END SUBROUTINE WRAP_JSON_VALUE_ADD_LOGICAL
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/20/2014
!
!  Add a logical vector child to the [[json_value]] variable.
!
!@note This routine is part of the public API that can be
!      used to build a JSON structure using [[json_value]] pointers.

    SUBROUTINE JSON_VALUE_ADD_LOGICAL_VEC(JSON, P, NAME, VAL)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: NAME  !! name of the vector
    LOGICAL(LK),DIMENSION(:),INTENT(IN) :: VAL   !! value

    TYPE(JSON_VALUE),POINTER :: VAR
    INTEGER(IK) :: I    !! counter

!create the variable as an array:
    CALL JSON%CREATE_ARRAY(VAR,NAME)

!populate the array:
    DO I=1,SIZE(VAL)
        CALL JSON%ADD(VAR, CK_'', VAL(I))
    END DO

!add it:
    CALL JSON%ADD(P, VAR)

    END SUBROUTINE JSON_VALUE_ADD_LOGICAL_VEC
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_logical_vec]] where `name` is kind=CDK.

    SUBROUTINE WRAP_JSON_VALUE_ADD_LOGICAL_VEC(JSON, P, NAME, VAL)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: NAME   !! name of the variable
    LOGICAL(LK),DIMENSION(:),INTENT(IN)  :: VAL    !! value

    CALL JSON%ADD(P, TO_UNICODE(NAME), VAL)

    END SUBROUTINE WRAP_JSON_VALUE_ADD_LOGICAL_VEC
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/19/2014
!
!  Add a character string child to the [[json_value]] variable.
!
!@note This routine is part of the public API that can be
!      used to build a JSON structure using [[json_value]] pointers.

    SUBROUTINE JSON_VALUE_ADD_STRING(JSON, P, NAME, VAL, TRIM_STR, ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: NAME        !! name of the variable
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: VAL         !! value
    LOGICAL(LK),INTENT(IN),OPTIONAL     :: TRIM_STR    !! if TRIM() should be called for the `val`
    LOGICAL(LK),INTENT(IN),OPTIONAL     :: ADJUSTL_STR !! if ADJUSTL() should be called for the `val`

    TYPE(JSON_VALUE),POINTER :: VAR

!create the variable:
    CALL JSON%CREATE_STRING(VAR,VAL,NAME,TRIM_STR,ADJUSTL_STR)

!add it:
    CALL JSON%ADD(P, VAR)

    END SUBROUTINE JSON_VALUE_ADD_STRING
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_string]] where `name` and `val` are kind=CDK.

    SUBROUTINE WRAP_JSON_VALUE_ADD_STRING(JSON, P, NAME, VAL, TRIM_STR, ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: NAME        !! name of the variable
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: VAL         !! value
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: TRIM_STR    !! if TRIM() should be called for the `val`
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: ADJUSTL_STR !! if ADJUSTL() should be called for the `val`

    CALL JSON%ADD(P, TO_UNICODE(NAME), TO_UNICODE(VAL), TRIM_STR, ADJUSTL_STR)

    END SUBROUTINE WRAP_JSON_VALUE_ADD_STRING
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_string]] where `name` is kind=CDK.

    SUBROUTINE JSON_VALUE_ADD_STRING_NAME_ASCII(JSON, P, NAME, VAL, TRIM_STR, ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: NAME        !! name of the variable
    CHARACTER(KIND=CK, LEN=*),INTENT(IN) :: VAL         !! value
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: TRIM_STR    !! if TRIM() should be called for the `val`
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: ADJUSTL_STR !! if ADJUSTL() should be called for the `val`

    CALL JSON%ADD(P, TO_UNICODE(NAME), VAL, TRIM_STR, ADJUSTL_STR)

    END SUBROUTINE JSON_VALUE_ADD_STRING_NAME_ASCII
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_string]] where `val` is kind=CDK.

    SUBROUTINE JSON_VALUE_ADD_STRING_VAL_ASCII(JSON, P, NAME, VAL, TRIM_STR, ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    CHARACTER(KIND=CK, LEN=*),INTENT(IN) :: NAME        !! name of the variable
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: VAL         !! value
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: TRIM_STR    !! if TRIM() should be called for the `val`
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: ADJUSTL_STR !! if ADJUSTL() should be called for the `val`

    CALL JSON%ADD(P, NAME, TO_UNICODE(VAL), TRIM_STR, ADJUSTL_STR)

    END SUBROUTINE JSON_VALUE_ADD_STRING_VAL_ASCII
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/19/2014
!
!  Add a character string vector child to the [[json_value]] variable.
!
!@note This routine is part of the public API that can be
!      used to build a JSON structure using [[json_value]] pointers.

    SUBROUTINE JSON_VALUE_ADD_STRING_VEC(JSON, P, NAME, VAL, TRIM_STR, ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                   :: JSON
    TYPE(JSON_VALUE),POINTER                         :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)              :: NAME        !! variable name
    CHARACTER(KIND=CK,LEN=*),DIMENSION(:),INTENT(IN) :: VAL         !! array of strings
    LOGICAL(LK),INTENT(IN),OPTIONAL                  :: TRIM_STR    !! if TRIM() should be called for each eleMENT
    LOGICAL(LK),INTENT(IN),OPTIONAL                  :: ADJUSTL_STR !! if ADJUSTL() should be called for each ELEMENT

    TYPE(JSON_VALUE),POINTER :: VAR
    INTEGER(IK) :: I  !! counter

!create the variable as an array:
    CALL JSON%CREATE_ARRAY(VAR,NAME)

!populate the array:
    DO I=1,SIZE(VAL)
        CALL JSON%ADD(VAR, CK_'', VAL(I), TRIM_STR, ADJUSTL_STR)
    END DO

!add it:
    CALL JSON%ADD(P, VAR)

    END SUBROUTINE JSON_VALUE_ADD_STRING_VEC
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_string_vec]] where `name` and `val` are kind=CDK.

    SUBROUTINE WRAP_JSON_VALUE_ADD_STRING_VEC(JSON, P, NAME, VAL, TRIM_STR, ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                    :: JSON
    TYPE(JSON_VALUE),POINTER                          :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN)              :: NAME
    CHARACTER(KIND=CDK,LEN=*),DIMENSION(:),INTENT(IN) :: VAL
    LOGICAL(LK),INTENT(IN),OPTIONAL                   :: TRIM_STR
    LOGICAL(LK),INTENT(IN),OPTIONAL                   :: ADJUSTL_STR

    CALL JSON%ADD(P, TO_UNICODE(NAME), TO_UNICODE(VAL), TRIM_STR, ADJUSTL_STR)

    END SUBROUTINE WRAP_JSON_VALUE_ADD_STRING_VEC
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_string_vec]] where `name` is kind=CDK.

    SUBROUTINE JSON_VALUE_ADD_STRING_VEC_NAME_ASCII(JSON, P, NAME, VAL, TRIM_STR, ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                    :: JSON
    TYPE(JSON_VALUE),POINTER                          :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN)              :: NAME
    CHARACTER(KIND=CK, LEN=*),DIMENSION(:),INTENT(IN) :: VAL
    LOGICAL(LK),INTENT(IN),OPTIONAL                   :: TRIM_STR
    LOGICAL(LK),INTENT(IN),OPTIONAL                   :: ADJUSTL_STR

    CALL JSON%ADD(P, TO_UNICODE(NAME), VAL, TRIM_STR, ADJUSTL_STR)

    END SUBROUTINE JSON_VALUE_ADD_STRING_VEC_NAME_ASCII
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_add_string_vec]] where `val` is kind=CDK.

    SUBROUTINE JSON_VALUE_ADD_STRING_VEC_VAL_ASCII(JSON, P, NAME, VAL, TRIM_STR, ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                    :: JSON
    TYPE(JSON_VALUE),POINTER                          :: P
    CHARACTER(KIND=CK, LEN=*),INTENT(IN)              :: NAME
    CHARACTER(KIND=CDK,LEN=*),DIMENSION(:),INTENT(IN) :: VAL
    LOGICAL(LK),INTENT(IN),OPTIONAL                   :: TRIM_STR
    LOGICAL(LK),INTENT(IN),OPTIONAL                   :: ADJUSTL_STR

    CALL JSON%ADD(P, NAME, TO_UNICODE(VAL), TRIM_STR, ADJUSTL_STR)

    END SUBROUTINE JSON_VALUE_ADD_STRING_VEC_VAL_ASCII
!*****************************************************************************************

!*****************************************************************************************
!>
!  Count the number of children in the object or array.
!
!--- History
!  * JW : 1/4/2014 : Original routine removed.
!    Now using `n_children` variable.
!    Renamed from `json_value_count`.

    FUNCTION JSON_COUNT(JSON,P) RESULT(COUNT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN) :: P      !! this should normally be a `json_object`
!! or a `json_array`. For any other
!! variable type this will return 0.
    INTEGER(IK)                         :: COUNT  !! number of children in `p`.

    IF (ASSOCIATED(P)) THEN
        COUNT = P%N_CHILDREN
    ELSE
        CALL JSON%THROW_EXCEPTION('Error in json_count: '//&
                                  'pointer is not associated.')
    END IF

    END FUNCTION JSON_COUNT
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 10/16/2015
!
!  Returns a pointer to the parent of a [[json_value]].
!  If there is no parent, then a `null()` pointer is returned.

    SUBROUTINE JSON_GET_PARENT(JSON,P,PARENT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: P        !! JSON object
    TYPE(JSON_VALUE),POINTER,INTENT(OUT) :: PARENT   !! pointer to `parent`

    IF (ASSOCIATED(P)) THEN
        PARENT => P%PARENT
    ELSE
        NULLIFY(PARENT)
        CALL JSON%THROW_EXCEPTION('Error in json_get_parent: '//&
                                  'pointer is not associated.')
    END IF

    END SUBROUTINE JSON_GET_PARENT
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 10/31/2015
!
!  Returns a pointer to the next of a [[json_value]].
!  If there is no next, then a `null()` pointer is returned.

    SUBROUTINE JSON_GET_NEXT(JSON,P,NEXT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: P       !! JSON object
    TYPE(JSON_VALUE),POINTER,INTENT(OUT) :: NEXT    !! pointer to `next`

    IF (ASSOCIATED(P)) THEN
        NEXT => P%NEXT
    ELSE
        NULLIFY(NEXT)
        CALL JSON%THROW_EXCEPTION('Error in json_get_next: '//&
                                  'pointer is not associated.')
    END IF

    END SUBROUTINE JSON_GET_NEXT
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 10/31/2015
!
!  Returns a pointer to the previous of a [[json_value]].
!  If there is no previous, then a `null()` pointer is returned.

    SUBROUTINE JSON_GET_PREVIOUS(JSON,P,PREVIOUS)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: P        !! JSON object
    TYPE(JSON_VALUE),POINTER,INTENT(OUT) :: PREVIOUS !! pointer to `previous`

    IF (ASSOCIATED(P)) THEN
        PREVIOUS => P%PREVIOUS
    ELSE
        NULLIFY(PREVIOUS)
        CALL JSON%THROW_EXCEPTION('Error in json_get_previous: '//&
                                  'pointer is not associated.')
    END IF

    END SUBROUTINE JSON_GET_PREVIOUS
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 10/31/2015
!
!  Returns a pointer to the tail of a [[json_value]]
!  (the last child of an array of object).
!  If there is no tail, then a `null()` pointer is returned.

    SUBROUTINE JSON_GET_TAIL(JSON,P,TAIL)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: P        !! JSON object
    TYPE(JSON_VALUE),POINTER,INTENT(OUT) :: TAIL     !! pointer to `tail`

    IF (ASSOCIATED(P)) THEN
        TAIL => P%TAIL
    ELSE
        NULLIFY(TAIL)
        CALL JSON%THROW_EXCEPTION('Error in json_get_tail: '//&
                                  'pointer is not associated.')
    END IF

    END SUBROUTINE JSON_GET_TAIL
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns a child in the object or array given the index.

    SUBROUTINE JSON_VALUE_GET_CHILD_BY_INDEX(JSON, P, IDX, CHILD, FOUND)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN) :: P      !! object or array JSON data
    INTEGER(IK),INTENT(IN)              :: IDX    !! index of the child
!! (this is a 1-based Fortran
!! style array index).
    TYPE(JSON_VALUE),POINTER            :: CHILD  !! pointer to the child
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND  !! true if the value was found
!! (if not present, an exception
!! will be thrown if it was not
!! found.  If present and not
!! found, no exception will be
!! thrown).

    INTEGER(IK) :: I  !! counter

    NULLIFY(CHILD)

    IF (.NOT. JSON%EXCEPTION_THROWN) THEN

        IF (ASSOCIATED(P%CHILDREN)) THEN

! If getting first or last child, we can do this quickly.
! Otherwise, traverse the list.
            IF (IDX==1) THEN

                CHILD => P%CHILDREN  ! first one

            ELSEIF (IDX==P%N_CHILDREN) THEN

                IF (ASSOCIATED(P%TAIL)) THEN
                    CHILD => P%TAIL  ! last one
                ELSE
                    CALL JSON%THROW_EXCEPTION('Error in json_value_get_child_by_index:'//&
                                              ' child%tail is not associated.',FOUND)
                END IF

            ELSEIF (IDX<1 .OR. IDX>P%N_CHILDREN) THEN

                CALL JSON%THROW_EXCEPTION('Error in json_value_get_child_by_index:'//&
                                          ' idx is out of range.',FOUND)

            ELSE

! if idx is closer to the end, we traverse the list backward from tail,
! otherwise we traverse it forward from children:

                IF (P%N_CHILDREN-IDX < IDX) THEN  ! traverse backward

                    CHILD => P%TAIL

                    DO I = 1, P%N_CHILDREN - IDX

                        IF (ASSOCIATED(CHILD%PREVIOUS)) THEN
                            CHILD => CHILD%PREVIOUS
                        ELSE
                            CALL JSON%THROW_EXCEPTION('Error in json_value_get_child_by_index:'//&
                                                      ' child%previous is not associated.',FOUND)
                            NULLIFY(CHILD)
                            EXIT
                        END IF

                    END DO

                ELSE  ! traverse forward

                    CHILD => P%CHILDREN

                    DO I = 1, IDX - 1

                        IF (ASSOCIATED(CHILD%NEXT)) THEN
                            CHILD => CHILD%NEXT
                        ELSE
                            CALL JSON%THROW_EXCEPTION('Error in json_value_get_child_by_index:'//&
                                                      ' child%next is not associated.',FOUND)
                            NULLIFY(CHILD)
                            EXIT
                        END IF

                    END DO

                END IF

            END IF

        ELSE

            CALL JSON%THROW_EXCEPTION('Error in json_value_get_child_by_index:'//&
                                      ' p%children is not associated.',FOUND)

        END IF

! found output:
        IF (JSON%EXCEPTION_THROWN) THEN
            IF (PRESENT(FOUND)) THEN
                CALL JSON%CLEAR_EXCEPTIONS()
                FOUND = .FALSE.
            END IF
        ELSE
            IF (PRESENT(FOUND)) FOUND = .TRUE.
        END IF

    ELSE
        IF (PRESENT(FOUND)) FOUND = .FALSE.
    END IF

    END SUBROUTINE JSON_VALUE_GET_CHILD_BY_INDEX
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns pointer to the first child of the object
!  (or `null()` if it is not associated).

    SUBROUTINE JSON_VALUE_GET_CHILD(JSON, P, CHILD)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN) :: P      !! object or array JSON data
    TYPE(JSON_VALUE),POINTER            :: CHILD  !! pointer to the child

    IF (ASSOCIATED(P)) THEN
        CHILD => P%CHILDREN
    ELSE
        NULLIFY(CHILD)
        CALL JSON%THROW_EXCEPTION('Error in json_value_get_child: '//&
                                  'pointer is not associated.')
    END IF

    END SUBROUTINE JSON_VALUE_GET_CHILD
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns a child in the object or array given the name string.
!
!  The name search can be case-sensitive or not, and can have significant trailing
!  whitespace or not, depending on the settings in the [[json_core(type)]] class.
!
!@note The `name` input is not a path, and is not parsed like it is in [[json_get_by_path]].

    SUBROUTINE JSON_VALUE_GET_CHILD_BY_NAME(JSON, P, NAME, CHILD, FOUND)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN) :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: NAME   !! the name of a child of `p`
    TYPE(JSON_VALUE),POINTER            :: CHILD  !! pointer to the child
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND  !! true if the value was found
!! (if not present, an exception
!! will be thrown if it was not
!! found.  If present and not
!! found, no exception will be
!! thrown).

    INTEGER(IK) :: I,N_CHILDREN
    LOGICAL :: ERROR

    NULLIFY(CHILD)

    IF (.NOT. JSON%EXCEPTION_THROWN) THEN

        IF (ASSOCIATED(P)) THEN

            ERROR = .TRUE.   ! will be false if it is found
            IF (P%VAR_TYPE==JSON_OBJECT) THEN
                N_CHILDREN = JSON%COUNT(P)
                CHILD => P%CHILDREN    !start with first one
                DO I=1, N_CHILDREN
                    IF (.NOT. ASSOCIATED(CHILD)) THEN
                        CALL JSON%THROW_EXCEPTION(&
                            'Error in json_value_get_child_by_name: '//&
                            'Malformed JSON linked list',FOUND)
                        EXIT
                    END IF
                    IF (ALLOCATED(CHILD%NAME)) THEN
!name string matching routine:
                        IF (JSON%NAME_EQUAL(CHILD,NAME)) THEN
                            ERROR = .FALSE.
                            EXIT
                        END IF
                    END IF
                    CHILD => CHILD%NEXT
                END DO
            END IF

            IF (ERROR) THEN
!did not find anything:
                CALL JSON%THROW_EXCEPTION(&
                    'Error in json_value_get_child_by_name: '//&
                    'child variable '//TRIM(NAME)//' was not found.',FOUND)
                NULLIFY(CHILD)
            END IF

        ELSE
            CALL JSON%THROW_EXCEPTION(&
                'Error in json_value_get_child_by_name: '//&
                'pointer is not associated.',FOUND)
        END IF

! found output:
        IF (JSON%EXCEPTION_THROWN) THEN
            IF (PRESENT(FOUND)) THEN
                CALL JSON%CLEAR_EXCEPTIONS()
                FOUND = .FALSE.
            END IF
        ELSE
            IF (PRESENT(FOUND)) FOUND = .TRUE.
        END IF

    ELSE
        IF (PRESENT(FOUND)) FOUND = .FALSE.
    END IF

    END SUBROUTINE JSON_VALUE_GET_CHILD_BY_NAME
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 8/25/2017
!
!  Checks a JSON object for duplicate child names.
!
!  It uses the specified settings for name matching (see [[name_strings_equal]]).
!
!@note This will only check for one duplicate,
!      it will return the first one that it finds.

    SUBROUTINE JSON_CHECK_CHILDREN_FOR_DUPLICATE_KEYS(JSON,P,HAS_DUPLICATE,NAME,PATH)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT) :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN) :: P  !! the object to search. If `p` is
!! not a `json_object`, then `has_duplicate`
!! will be false.
    LOGICAL(LK),INTENT(OUT) :: HAS_DUPLICATE  !! true if there is at least
!! two children have duplicate
!! `name` values.
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT),OPTIONAL :: NAME !! the duplicate name
!! (unallocated if no
!! duplicate was found)
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT),OPTIONAL :: PATH !! the full path to the
!! duplicate name
!! (unallocated if no
!! duplicate was found)

    INTEGER(IK)              :: I           !! counter
    INTEGER(IK)              :: J           !! counter
    TYPE(JSON_VALUE),POINTER :: CHILD       !! pointer to a child of `p`
    INTEGER(IK)              :: N_CHILDREN  !! number of children of `p`
    LOGICAL(LK)              :: FOUND       !! flag for `get_child`

    TYPE :: ALLOC_STR
!! so we can have an array of allocatable strings
        CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: STR  !! name string
    END TYPE ALLOC_STR
    TYPE(ALLOC_STR),DIMENSION(:),ALLOCATABLE :: NAMES !! array of all the
!! child name strings

! initialize:
    HAS_DUPLICATE =.FALSE.

    IF (.NOT. JSON%EXCEPTION_THROWN) THEN

        IF (ASSOCIATED(P)) THEN

            IF (P%VAR_TYPE==JSON_OBJECT) THEN

! number of items to check:
                N_CHILDREN = JSON%COUNT(P)
                ALLOCATE(NAMES(N_CHILDREN))

! first get a list of all the name keys:
                DO I=1, N_CHILDREN
                    CALL JSON%GET_CHILD(P,I,CHILD,FOUND) ! get by index
                    IF (.NOT. FOUND) THEN
                        CALL JSON%THROW_EXCEPTION(&
                            'Error in json_check_children_for_duplicate_keys: '//&
                            'Malformed JSON linked list')
                        EXIT
                    END IF
                    IF (ALLOCATED(CHILD%NAME)) THEN
                        NAMES(I)%STR = CHILD%NAME
                    ELSE
                        CALL JSON%THROW_EXCEPTION(&
                            'Error in json_check_children_for_duplicate_keys: '//&
                            'Object child name is not allocated')
                        EXIT
                    END IF
                END DO

                IF (.NOT. JSON%EXCEPTION_THROWN) THEN
! now check the list for duplicates:
                    MAIN: DO I=1,N_CHILDREN
                        DO J=1,I-1
                            IF (JSON%NAME_STRINGS_EQUAL(NAMES(I)%STR,NAMES(J)%STR)) THEN
                                HAS_DUPLICATE = .TRUE.
                                IF (PRESENT(NAME)) THEN
                                    NAME = NAMES(I)%STR
                                END IF
                                IF (PRESENT(PATH)) THEN
                                    CALL JSON%GET_CHILD(P,NAMES(I)%STR,CHILD,FOUND) ! get by name
                                    IF (FOUND) THEN
                                        CALL JSON%GET_PATH(CHILD,PATH,FOUND)
                                        IF (.NOT. FOUND) THEN
! should never happen since we know it is there
                                            CALL JSON%THROW_EXCEPTION(&
                                                    'Error in json_check_children_for_duplicate_keys: '//&
                                                    'Could not get path')
                                        END IF
                                    ELSE
! should never happen since we know it is there
                                        CALL JSON%THROW_EXCEPTION(&
                                            'Error in json_check_children_for_duplicate_keys: '//&
                                            'Could not get child: '//TRIM(NAMES(I)%STR))
                                    END IF
                                END IF
                                EXIT MAIN
                            END IF
                        END DO
                    END DO MAIN
                END IF

! cleanup
                DO I=1,N_CHILDREN
                    IF (ALLOCATED(NAMES(I)%STR)) DEALLOCATE(NAMES(I)%STR)
                END DO
                IF (ALLOCATED(NAMES)) DEALLOCATE(NAMES)

            END IF

        END IF

    END IF

    END SUBROUTINE JSON_CHECK_CHILDREN_FOR_DUPLICATE_KEYS
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 8/25/2017
!
!  Checks a JSON structure for duplicate child names.
!  This one recursively traverses the entire structure
!  (calling [[json_check_children_for_duplicate_keys]]
!  recursively for each element).
!
!@note This will only check for one duplicate,
!      it will return the first one that it finds.

    SUBROUTINE JSON_CHECK_ALL_FOR_DUPLICATE_KEYS(JSON,P,HAS_DUPLICATE,NAME,PATH)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT) :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN) :: P  !! the object to search. If `p` is
!! not a `json_object`, then `has_duplicate`
!! will be false.
    LOGICAL(LK),INTENT(OUT) :: HAS_DUPLICATE  !! true if there is at least
!! one duplicate `name` key anywhere
!! in the structure.
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT),OPTIONAL :: NAME !! the duplicate name
!! (unallocated if no
!! duplicates were found)
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT),OPTIONAL :: PATH !! the full path to the
!! duplicate name
!! (unallocated if no
!! duplicate was found)

    HAS_DUPLICATE = .FALSE.
    IF (.NOT. JSON%EXCEPTION_THROWN) THEN
        CALL JSON%TRAVERSE(P,DUPLICATE_KEY_FUNC)
    END IF

    CONTAINS

        SUBROUTINE DUPLICATE_KEY_FUNC(JSON,P,FINISHED)

!! Callback function to check each element
!! for duplicate child names.

        IMPLICIT NONE

        CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
        TYPE(JSON_VALUE),POINTER,INTENT(IN) :: P
        LOGICAL(LK),INTENT(OUT)             :: FINISHED

!- 5854

        CALL JSON%CHECK_CHILDREN_FOR_DUPLICATE_KEYS(P,HAS_DUPLICATE,NAME,PATH)


        FINISHED = HAS_DUPLICATE .OR. JSON%EXCEPTION_THROWN

        END SUBROUTINE DUPLICATE_KEY_FUNC

    END SUBROUTINE JSON_CHECK_ALL_FOR_DUPLICATE_KEYS
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_get_child_by_name]] where `name` is kind=CDK.

    SUBROUTINE WRAP_JSON_VALUE_GET_CHILD_BY_NAME(JSON, P, NAME, CHILD, FOUND)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: NAME
    TYPE(JSON_VALUE),POINTER             :: CHILD
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND

    CALL JSON%GET(P,TO_UNICODE(NAME),CHILD,FOUND)

    END SUBROUTINE WRAP_JSON_VALUE_GET_CHILD_BY_NAME
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 2/12/2014
!
!  Print the [[json_value]] structure to an allocatable string.

    SUBROUTINE JSON_VALUE_TO_STRING(JSON,P,STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                   :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)              :: P
    CHARACTER(KIND=CK,LEN=:),INTENT(OUT),ALLOCATABLE :: STR  !! prints structure to this string

    INTEGER(IK) :: ILOC  !! used to keep track of size of str
!! since it is being allocated in chunks.

    STR = REPEAT(SPACE, PRINT_STR_CHUNK_SIZE)
    ILOC = 0_IK
    CALL JSON%JSON_VALUE_PRINT(P, IUNIT=UNIT2STR, STR=STR, ILOC=ILOC, INDENT=1_IK, COLON=.TRUE.)

! trim the string if necessary:
    IF (LEN(STR)>ILOC) STR = STR(1:ILOC)

    END SUBROUTINE JSON_VALUE_TO_STRING
!*****************************************************************************************

!*****************************************************************************************
!>
!  Print the [[json_value]] structure to the console (`output_unit`).
!
!--- Note
!  * Just a wrapper for [[json_print_to_unit]].

    SUBROUTINE JSON_PRINT_TO_CONSOLE(JSON,P)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN) :: P

    CALL JSON%PRINT(P,INT(OUTPUT_UNIT,IK))

    END SUBROUTINE JSON_PRINT_TO_CONSOLE
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 6/20/2014
!
!  Print the [[json_value]] structure to a file.

    SUBROUTINE JSON_PRINT_TO_UNIT(JSON,P,IUNIT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: P
    INTEGER(IK),INTENT(IN)               :: IUNIT   !! the file unit (the file must
!! already have been opened, can't be -1).

    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: DUMMY  !! dummy for `str` argument
!! to [[json_value_print]]
    INTEGER(IK)                          :: IDUMMY !! dummy for `iloc` argument
!! to [[json_value_print]]

    IF (IUNIT/=UNIT2STR) THEN
        IDUMMY = 0_IK
        CALL JSON%JSON_VALUE_PRINT(P,IUNIT,STR=DUMMY,ILOC=IDUMMY,INDENT=1_IK,COLON=.TRUE.)
    ELSE
        CALL JSON%THROW_EXCEPTION('Error in json_print_to_unit: iunit must not be -1.')
    END IF

    END SUBROUTINE JSON_PRINT_TO_UNIT
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/23/2014
!
!  Print the [[json_value]] structure to a file.

    SUBROUTINE JSON_PRINT_TO_FILENAME(JSON,P,FILENAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: FILENAME  !! the filename to print to
!! (should not already be open)

    INTEGER(IK) :: IUNIT  !! file unit for `open` statement
    INTEGER(IK) :: ISTAT  !! `iostat` code for `open` statement

    OPEN(NEWUNIT=IUNIT,FILE=FILENAME,STATUS='REPLACE',IOSTAT=ISTAT  )
    IF (ISTAT==0) THEN
        CALL JSON%PRINT(P,IUNIT)
        CLOSE(IUNIT,IOSTAT=ISTAT)
    ELSE
        CALL JSON%THROW_EXCEPTION('Error in json_print_to_filename: could not open file: '//&
                              TRIM(FILENAME))
    END IF

    END SUBROUTINE JSON_PRINT_TO_FILENAME
!*****************************************************************************************

!*****************************************************************************************
!>
!  Print the JSON structure to a string or a file.
!
!--- Notes
!  * This is an internal routine called by the various wrapper routines.
!  * The reason the `str` argument is non-optional is because of a
!    bug in v4.9 of the gfortran compiler.

    RECURSIVE SUBROUTINE JSON_VALUE_PRINT(JSON,P,IUNIT,STR,INDENT,&
                                          NEED_COMMA,COLON,IS_ARRAY_ELEMENT,&
                                          IS_COMPRESSED_VECTOR,ILOC)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: P
    INTEGER(IK),INTENT(IN)               :: IUNIT             !! file unit to write to (the
!! file is assumed to be open)
    INTEGER(IK),INTENT(IN),OPTIONAL      :: INDENT            !! indention level
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: IS_ARRAY_ELEMENT  !! if this is an array element
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: NEED_COMMA        !! if it needs a comma after it
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: COLON             !! if the colon was just written
    CHARACTER(KIND=CK,LEN=:),INTENT(INOUT),ALLOCATABLE :: STR
!! if `iunit==unit2str` (-1) then
!! the structure is printed to this
!! string rather than a file. This mode
!! is used by [[json_value_to_string]].
    INTEGER(IK),INTENT(INOUT) :: ILOC  !! current index in `str`. should be set to 0 initially.
!! [only used when `str` is used.]
    LOGICAL(LK),INTENT(IN),OPTIONAL :: IS_COMPRESSED_VECTOR  !! if True, this is an element
!! from an array being printed
!! on one line [default is False]

    CHARACTER(KIND=CK,LEN=MAX_NUMERIC_STR_LEN) :: TMP !! for value to string conversions
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: S_INDENT !! the string of spaces for
!! indenting (see `tab` and `spaces`)
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: S !! the string appended to `str`
    TYPE(JSON_VALUE),POINTER :: ELEMENT !! for getting children
    INTEGER(IK) :: TAB           !! number of `tabs` for indenting
    INTEGER(IK) :: SPACES        !! number of spaces for indenting
    INTEGER(IK) :: I             !! counter
    INTEGER(IK) :: COUNT         !! number of children
    LOGICAL(LK) :: PRINT_COMMA   !! if the comma will be printed after the value
    LOGICAL(LK) :: WRITE_FILE    !! if we are writing to a file
    LOGICAL(LK) :: WRITE_STRING  !! if we are writing to a string
    LOGICAL(LK) :: IS_ARRAY      !! if this is an element in an array
    LOGICAL(LK) :: IS_VECTOR     !! if all elements of a vector
!! are scalars of the same type
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: STR_ESCAPED !! escaped version of
!! `name` or `str_value`

    IF (.NOT. JSON%EXCEPTION_THROWN) THEN

        IF (.NOT. ASSOCIATED(P)) THEN
! note: a null() pointer will trigger this error.
! However, if the pointer is undefined, then this will
! crash (if this wasn't here it would crash below when
! we try to access the contents)
            CALL JSON%THROW_EXCEPTION('Error in json_value_print: '//&
                                      'the pointer is not associated')
            RETURN
        END IF

        IF (PRESENT(IS_COMPRESSED_VECTOR)) THEN
            IS_VECTOR = IS_COMPRESSED_VECTOR
        ELSE
            IS_VECTOR = .FALSE.
        END IF

!whether to write a string or a file (one or the other):
        WRITE_STRING = (IUNIT==UNIT2STR)
        WRITE_FILE = .NOT. WRITE_STRING

!if the comma will be printed after the value
! [comma not printed for the last elements]
        IF (PRESENT(NEED_COMMA)) THEN
            PRINT_COMMA = NEED_COMMA
        ELSE
            PRINT_COMMA = .FALSE.
        END IF

!number of "tabs" to indent:
        IF (PRESENT(INDENT) .AND. .NOT. JSON%NO_WHITESPACE) THEN
            TAB = INDENT
        ELSE
            TAB = 0
        END IF
!convert to number of spaces:
        SPACES = TAB*JSON%SPACES_PER_TAB

!if this is an element in an array:
        IF (PRESENT(IS_ARRAY_ELEMENT)) THEN
            IS_ARRAY = IS_ARRAY_ELEMENT
        ELSE
            IS_ARRAY = .FALSE.
        END IF

!if the colon was the last thing written
        IF (PRESENT(COLON)) THEN
            S_INDENT = CK_''
        ELSE
            S_INDENT = REPEAT(SPACE, SPACES)
        END IF

        SELECT CASE (P%VAR_TYPE)

        CASE (JSON_OBJECT)

            COUNT = JSON%COUNT(P)

            IF (COUNT==0) THEN    !special case for empty object

                S = S_INDENT//START_OBJECT//END_OBJECT
                CALL WRITE_IT( COMMA=PRINT_COMMA )

            ELSE

                S = S_INDENT//START_OBJECT
                CALL WRITE_IT()

!if an object is in an array, there is an extra tab:
                IF (IS_ARRAY) THEN
                    IF ( .NOT. JSON%NO_WHITESPACE) TAB = TAB+1
                    SPACES = TAB*JSON%SPACES_PER_TAB
                END IF

                NULLIFY(ELEMENT)
                ELEMENT => P%CHILDREN
                DO I = 1, COUNT

                    IF (.NOT. ASSOCIATED(ELEMENT)) THEN
                        CALL JSON%THROW_EXCEPTION('Error in json_value_print: '//&
                                                  'Malformed JSON linked list')
                        RETURN
                    END IF

! print the name
                    IF (ALLOCATED(ELEMENT%NAME)) THEN
                        CALL ESCAPE_STRING(ELEMENT%NAME,STR_ESCAPED,JSON%ESCAPE_SOLIDUS)
                        IF (JSON%NO_WHITESPACE) THEN
!compact printing - no extra space
                            S = REPEAT(SPACE, SPACES)//QUOTATION_MARK//&
                                          STR_ESCAPED//QUOTATION_MARK//COLON_CHAR
                            CALL WRITE_IT(ADVANCE=.FALSE.)
                        ELSE
                            S = REPEAT(SPACE, SPACES)//QUOTATION_MARK//&
                                          STR_ESCAPED//QUOTATION_MARK//COLON_CHAR//SPACE
                            CALL WRITE_IT(ADVANCE=.FALSE.)
                        END IF
                    ELSE
                        CALL JSON%THROW_EXCEPTION('Error in json_value_print:'//&
                                                  ' element%name not allocated')
                        NULLIFY(ELEMENT)
                        RETURN
                    END IF

! recursive print of the element
                    CALL JSON%JSON_VALUE_PRINT(ELEMENT, IUNIT=IUNIT, INDENT=TAB + 1_IK, &
                                    NEED_COMMA=I<COUNT, COLON=.TRUE., STR=STR, ILOC=ILOC)
                    IF (JSON%EXCEPTION_THROWN) RETURN

! get the next child the list:
                    ELEMENT => ELEMENT%NEXT

                END DO

! [one fewer tab if it isn't an array element]
                IF (.NOT. IS_ARRAY) THEN
                    S = REPEAT(SPACE, MAX(0_IK,SPACES-JSON%SPACES_PER_TAB))//END_OBJECT
                ELSE
                    S = S_INDENT//END_OBJECT
                END IF
                CALL WRITE_IT( COMMA=PRINT_COMMA )
                NULLIFY(ELEMENT)

            END IF

        CASE (JSON_ARRAY)

            COUNT = JSON%COUNT(P)

            IF (COUNT==0) THEN    ! special case for empty array

                S = S_INDENT//START_ARRAY//END_ARRAY
                CALL WRITE_IT( COMMA=PRINT_COMMA )

            ELSE

! if every child is the same type & a scalar:
                IS_VECTOR = JSON%IS_VECTOR(P)
                IF (JSON%FAILED()) RETURN

                S = S_INDENT//START_ARRAY
                CALL WRITE_IT( ADVANCE=(.NOT. IS_VECTOR) )

!if an array is in an array, there is an extra tab:
                IF (IS_ARRAY) THEN
                    IF ( .NOT. JSON%NO_WHITESPACE) TAB = TAB+1
                    SPACES = TAB*JSON%SPACES_PER_TAB
                END IF

                NULLIFY(ELEMENT)
                ELEMENT => P%CHILDREN
                DO I = 1, COUNT

                    IF (.NOT. ASSOCIATED(ELEMENT)) THEN
                        CALL JSON%THROW_EXCEPTION('Error in json_value_print: '//&
                                                  'Malformed JSON linked list')
                        RETURN
                    END IF

! recursive print of the element
                    IF (IS_VECTOR) THEN
                        CALL JSON%JSON_VALUE_PRINT(ELEMENT, IUNIT=IUNIT, INDENT=0_IK,&
                                        NEED_COMMA=I<COUNT, IS_ARRAY_ELEMENT=.FALSE., &
                                        STR=STR, ILOC=ILOC,&
                                        IS_COMPRESSED_VECTOR = .TRUE.)
                    ELSE
                        CALL JSON%JSON_VALUE_PRINT(ELEMENT, IUNIT=IUNIT, INDENT=TAB,&
                                        NEED_COMMA=I<COUNT, IS_ARRAY_ELEMENT=.TRUE., &
                                        STR=STR, ILOC=ILOC)
                    END IF
                    IF (JSON%EXCEPTION_THROWN) RETURN

! get the next child the list:
                    ELEMENT => ELEMENT%NEXT

                END DO

!indent the closing array character:
                IF (IS_VECTOR) THEN
                    S = END_ARRAY
                    CALL WRITE_IT( COMMA=PRINT_COMMA )
                ELSE
                    S = REPEAT(SPACE, MAX(0_IK,SPACES-JSON%SPACES_PER_TAB))//END_ARRAY
                    CALL WRITE_IT( COMMA=PRINT_COMMA )
                END IF
                NULLIFY(ELEMENT)

            END IF

        CASE (JSON_NULL)

            S = S_INDENT//NULL_STR
            CALL WRITE_IT( COMMA=PRINT_COMMA, &
                           ADVANCE=(.NOT. IS_VECTOR),&
                           SPACE_AFTER_COMMA=IS_VECTOR )

        CASE (JSON_STRING)

            IF (ALLOCATED(P%STR_VALUE)) THEN
! have to escape the string for printing:
                CALL ESCAPE_STRING(P%STR_VALUE,STR_ESCAPED,JSON%ESCAPE_SOLIDUS)
                S = S_INDENT//QUOTATION_MARK//STR_ESCAPED//QUOTATION_MARK
                CALL WRITE_IT( COMMA=PRINT_COMMA, &
                               ADVANCE=(.NOT. IS_VECTOR),&
                               SPACE_AFTER_COMMA=IS_VECTOR )
            ELSE
                CALL JSON%THROW_EXCEPTION('Error in json_value_print:'//&
                                          ' p%value_string not allocated')
                RETURN
            END IF

        CASE (JSON_LOGICAL)

            IF (P%LOG_VALUE) THEN
                S = S_INDENT//TRUE_STR
                CALL WRITE_IT( COMMA=PRINT_COMMA, &
                               ADVANCE=(.NOT. IS_VECTOR),&
                               SPACE_AFTER_COMMA=IS_VECTOR )
            ELSE
                S = S_INDENT//FALSE_STR
                CALL WRITE_IT( COMMA=PRINT_COMMA, &
                               ADVANCE=(.NOT. IS_VECTOR),&
                               SPACE_AFTER_COMMA=IS_VECTOR )
            END IF

        CASE (JSON_INTEGER)

            CALL INTEGER_TO_STRING(P%INT_VALUE,INT_FMT,TMP)

            S = S_INDENT//TRIM(TMP)
            CALL WRITE_IT( COMMA=PRINT_COMMA, &
                           ADVANCE=(.NOT. IS_VECTOR),&
                           SPACE_AFTER_COMMA=IS_VECTOR )

        CASE (JSON_REAL)

            IF (ALLOCATED(JSON%REAL_FMT)) THEN
                CALL REAL_TO_STRING(P%DBL_VALUE,JSON%REAL_FMT,JSON%COMPACT_REAL,JSON%NON_NORMALS_TO_NULL,TMP)
            ELSE
!use the default format (user has not called initialize() or specified one):
                CALL REAL_TO_STRING(P%DBL_VALUE,DEFAULT_REAL_FMT,JSON%COMPACT_REAL,JSON%NON_NORMALS_TO_NULL,TMP)
            END IF

            S = S_INDENT//TRIM(TMP)
            CALL WRITE_IT( COMMA=PRINT_COMMA, &
                           ADVANCE=(.NOT. IS_VECTOR),&
                           SPACE_AFTER_COMMA=IS_VECTOR )

        CASE DEFAULT

            CALL INTEGER_TO_STRING(P%VAR_TYPE,INT_FMT,TMP)
            CALL JSON%THROW_EXCEPTION('Error in json_value_print: '//&
                                      'unknown data type: '//TRIM(TMP))

        END SELECT

    END IF

    CONTAINS

        SUBROUTINE WRITE_IT(ADVANCE,COMMA,SPACE_AFTER_COMMA)

!! write the string `s` to the file (or the output string)

        IMPLICIT NONE

        LOGICAL(LK),INTENT(IN),OPTIONAL :: ADVANCE           !! to add line break or not
        LOGICAL(LK),INTENT(IN),OPTIONAL :: COMMA             !! print comma after the string
        LOGICAL(LK),INTENT(IN),OPTIONAL :: SPACE_AFTER_COMMA !! print a space after the comma

        LOGICAL(LK) :: ADD_COMMA       !! if a delimiter is to be added after string
        LOGICAL(LK) :: ADD_LINE_BREAK  !! if a line break is to be added after string
        LOGICAL(LK) :: ADD_SPACE       !! if a space is to be added after the comma
        INTEGER(IK) :: N               !! length of actual string `s` appended to `str`
        INTEGER(IK) :: ROOM_LEFT       !! number of characters left in `str`
        INTEGER(IK) :: N_CHUNKS_TO_ADD !! number of chunks to add to `str` for appending `s`

        IF (PRESENT(COMMA)) THEN
            ADD_COMMA = COMMA
        ELSE
            ADD_COMMA = .FALSE. !default is not to add comma
        END IF
        IF (JSON%NO_WHITESPACE) THEN
            ADD_SPACE = .FALSE.
        ELSE
            IF (PRESENT(SPACE_AFTER_COMMA)) THEN
                ADD_SPACE = SPACE_AFTER_COMMA
            ELSE
                ADD_SPACE = .FALSE. !default is not to add space
            END IF
        END IF
        IF (PRESENT(ADVANCE)) THEN
            IF (JSON%NO_WHITESPACE) THEN
! overrides input value:
                ADD_LINE_BREAK = .FALSE.
            ELSE
                ADD_LINE_BREAK = ADVANCE
            END IF
        ELSE
            ADD_LINE_BREAK = .NOT. JSON%NO_WHITESPACE ! default is to advance if
! we are printing whitespace
        END IF

! string to print:
        IF (ADD_COMMA) THEN
            IF (ADD_SPACE) THEN
                S = S // DELIMITER // SPACE
            ELSE
                S = S // DELIMITER
            END IF
        END IF

        IF (WRITE_FILE) THEN

            IF (ADD_LINE_BREAK) THEN
                WRITE(IUNIT,FMT='(A)') S
            ELSE
                WRITE(IUNIT,FMT='(A)',ADVANCE='NO') S
            END IF

        ELSE    !write string

            IF (ADD_LINE_BREAK) S = S // NEWLINE

            N = LEN(S)
            ROOM_LEFT = LEN(STR)-ILOC
            IF (ROOM_LEFT < N) THEN
! need to add another chunk to fit this string:
                N_CHUNKS_TO_ADD = MAX(1_IK, CEILING( REAL(LEN(S)-ROOM_LEFT,RK) / REAL(CHUNK_SIZE,RK), IK ) )
                STR = STR // REPEAT(SPACE, PRINT_STR_CHUNK_SIZE*N_CHUNKS_TO_ADD)
            END IF
! append s to str:
            STR(ILOC+1:ILOC+N) = S
            ILOC = ILOC + N

        END IF

        END SUBROUTINE WRITE_IT

    END SUBROUTINE JSON_VALUE_PRINT
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns true if all the children are the same type (and a scalar).
!  Note that integers and reals are considered the same type for this purpose.
!  This routine is used for the `compress_vectors` option.

    FUNCTION JSON_IS_VECTOR(JSON, P) RESULT(IS_VECTOR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT) :: JSON
    TYPE(JSON_VALUE),POINTER       :: P
    LOGICAL(LK)                    :: IS_VECTOR  !! if all elements of a vector
!! are scalars of the same type

    INTEGER(IK) :: VAR_TYPE_PREV !! for getting the variable type of children
    INTEGER(IK) :: VAR_TYPE !! for getting the variable type of children
    TYPE(JSON_VALUE),POINTER :: ELEMENT !! for getting children
    INTEGER(IK) :: I !! counter
    INTEGER(IK) :: COUNT !! number of children

    INTEGER(IK),PARAMETER :: JSON_INVALID = -1_IK  !! to initialize the flag. an invalid value
    INTEGER(IK),PARAMETER :: JSON_NUMERIC = -2_IK  !! indicates `json_integer` or `json_real`

    IF (JSON%COMPRESS_VECTORS) THEN
! check to see if every child is the same type,
! and a scalar:
        IS_VECTOR = .TRUE.
        VAR_TYPE_PREV = JSON_INVALID
        COUNT = JSON%COUNT(P)
        ELEMENT => P%CHILDREN
        DO I = 1_IK, COUNT
            IF (.NOT. ASSOCIATED(ELEMENT)) THEN
                CALL JSON%THROW_EXCEPTION('Error in json_is_vector: '//&
                                          'Malformed JSON linked list')
                RETURN
            END IF
! check variable type of all the children.
! They must all be the same, and a scalar.
            CALL JSON%INFO(ELEMENT,VAR_TYPE=VAR_TYPE)
! special check for numeric values:
            IF (VAR_TYPE==JSON_INTEGER .OR. VAR_TYPE==JSON_REAL) VAR_TYPE = JSON_NUMERIC
            IF (VAR_TYPE==JSON_OBJECT .OR. &
                VAR_TYPE==JSON_ARRAY .OR. &
                (I>1_IK .AND. VAR_TYPE/=VAR_TYPE_PREV)) THEN
                IS_VECTOR = .FALSE.
                EXIT
            END IF
            VAR_TYPE_PREV = VAR_TYPE
! get the next child the list:
            ELEMENT => ELEMENT%NEXT
        END DO
    ELSE
        IS_VECTOR = .FALSE.
    END IF

    END FUNCTION JSON_IS_VECTOR
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns true if the `path` is present in the `p` JSON structure.
!
!@note Just a wrapper for [[json_get_by_path]], so it uses the
!      specified `path_mode` and other settings.

    FUNCTION JSON_VALID_PATH(JSON, P, PATH) RESULT(FOUND)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: P      !! a JSON linked list
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)  :: PATH   !! path to the variable
    LOGICAL(LK)                          :: FOUND  !! true if it was found

    TYPE(JSON_VALUE),POINTER :: TMP  !! pointer to the variable specified by `path`

    CALL JSON%GET(P, PATH, TMP, FOUND)

    END FUNCTION JSON_VALID_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_valid_path]] where "path" is kind=CDK.

    FUNCTION WRAP_JSON_VALID_PATH(JSON, P, PATH) RESULT(FOUND)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: P      !! a JSON linked list
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH   !! path to the variable
    LOGICAL(LK)                          :: FOUND  !! true if it was found

    FOUND = JSON%VALID_PATH(P, TO_UNICODE(PATH))

    END FUNCTION WRAP_JSON_VALID_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns the [[json_value]] pointer given the path string.
!
!  It uses one of three methods:
!
!  * The original JSON-Fortran defaults
!  * [RFC 6901](https://tools.ietf.org/html/rfc6901)
!  * [JSONPath](http://goessner.net/articles/JsonPath/) "bracket-notation"

    SUBROUTINE JSON_GET_BY_PATH(JSON, ME, PATH, P, FOUND)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: ME     !! a JSON linked list
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)  :: PATH   !! path to the variable
    TYPE(JSON_VALUE),POINTER,INTENT(OUT) :: P      !! pointer to the variable
!! specified by `path`
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND  !! true if it was found

    CHARACTER(KIND=CK,LEN=MAX_INTEGER_STR_LEN),ALLOCATABLE :: PATH_MODE_STR !! string version
!! of `json%path_mode`

    NULLIFY(P)

    IF (.NOT. JSON%EXCEPTION_THROWN) THEN

        SELECT CASE (JSON%PATH_MODE)
        CASE(1_IK)
            CALL JSON%JSON_GET_BY_PATH_DEFAULT(ME, PATH, P, FOUND)
        CASE(2_IK)
            CALL JSON%JSON_GET_BY_PATH_RFC6901(ME, PATH, P, FOUND)
        CASE(3_IK)
            CALL JSON%JSON_GET_BY_PATH_JSONPATH_BRACKET(ME, PATH, P, FOUND)
        CASE DEFAULT
            CALL INTEGER_TO_STRING(JSON%PATH_MODE,INT_FMT,PATH_MODE_STR)
            CALL JSON%THROW_EXCEPTION('Error in json_get_by_path: Unsupported path_mode: '//&
                                        TRIM(PATH_MODE_STR))
            IF (PRESENT(FOUND)) FOUND = .FALSE.
        END SELECT

        IF (PRESENT(FOUND)) THEN
            IF (.NOT. FOUND) CALL JSON%CLEAR_EXCEPTIONS()
        END IF

    ELSE
        IF (PRESENT(FOUND)) FOUND = .FALSE.
    END IF

    END SUBROUTINE JSON_GET_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns the [[json_value]] pointer given the path string,
!  If necessary, by creating the variables as needed.
!
!  By default, the leaf node and any empty array elements
!  are created as `json_null` values.
!
!  It only works for `path_mode=1` or `path_mode=3`.
!  An error will be thrown for `path_mode=2` (RFC 6901).
!
!--- See also
!  * [[json_get_by_path]]

    SUBROUTINE JSON_CREATE_BY_PATH(JSON,ME,PATH,P,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: ME           !! a JSON linked list
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)  :: PATH         !! path to the variable
    TYPE(JSON_VALUE),POINTER,INTENT(OUT),OPTIONAL :: P   !! pointer to the variable
!! specify by `path`
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! true if there were no errors
!! (variable found or created)
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! true if it was actually created
!! (as opposed to already being there)

    TYPE(JSON_VALUE),POINTER :: TMP
    CHARACTER(KIND=CK,LEN=MAX_INTEGER_STR_LEN) :: PATH_MODE_STR !! string version
!! of `json%path_mode`

    IF (PRESENT(P)) NULLIFY(P)

    IF (.NOT. JSON%EXCEPTION_THROWN) THEN

        SELECT CASE (JSON%PATH_MODE)
        CASE(1_IK)
            CALL JSON%JSON_GET_BY_PATH_DEFAULT(ME,PATH,TMP,FOUND,&
                                                CREATE_IT=.TRUE.,&
                                                WAS_CREATED=WAS_CREATED)
            IF (PRESENT(P)) P => TMP
        CASE(3_IK)
           CALL JSON%JSON_GET_BY_PATH_JSONPATH_BRACKET(ME,PATH,TMP,FOUND,&
                                                       CREATE_IT=.TRUE.,&
                                                       WAS_CREATED=WAS_CREATED)
           IF (PRESENT(P)) P => TMP

        CASE DEFAULT

            IF (JSON%PATH_MODE==2_IK) THEN
! the problem here is there isn't really a way to disambiguate
! the array elements, so '/a/0' could be 'a(1)' or 'a.0'.
                CALL JSON%THROW_EXCEPTION('Error in json_create_by_path: '//&
                                          'Create by path not supported in RFC 6901 path mode.')
            ELSE
                CALL INTEGER_TO_STRING(JSON%PATH_MODE,INT_FMT,PATH_MODE_STR)
                CALL JSON%THROW_EXCEPTION('Error in json_create_by_path: Unsupported path_mode: '//&
                                            TRIM(PATH_MODE_STR))
            END IF
            IF (PRESENT(FOUND)) THEN
                CALL JSON%CLEAR_EXCEPTIONS()
                FOUND = .FALSE.
            END IF
            IF (PRESENT(WAS_CREATED)) WAS_CREATED = .FALSE.
        END SELECT

    ELSE
        IF (PRESENT(WAS_CREATED)) WAS_CREATED = .FALSE.
        IF (PRESENT(FOUND)) FOUND = .FALSE.
    END IF

    END SUBROUTINE JSON_CREATE_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_create_by_path]] where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_CREATE_BY_PATH(JSON,ME,PATH,P,FOUND,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: ME           !! a JSON linked list
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH         !! path to the variable
    TYPE(JSON_VALUE),POINTER,INTENT(OUT),OPTIONAL :: P   !! pointer to the variable
!! specify by `path`
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND        !! true if there were no errors
!! (variable found or created)
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED  !! true if it was actually created
!! (as opposed to already being there)

    CALL JSON%CREATE(ME,TO_UNICODE(PATH),P,FOUND,WAS_CREATED)

    END SUBROUTINE WRAP_JSON_CREATE_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Rename a [[json_value]], given the path.
!
!@note this is a wrapper for [[json_value_rename]].

    SUBROUTINE JSON_RENAME_BY_PATH(JSON, ME, PATH, NAME, FOUND)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)  :: PATH  !! path to the variable to rename
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)  :: NAME  !! the new name
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND !! if there were no errors

    TYPE(JSON_VALUE),POINTER :: P

    IF ( JSON%EXCEPTION_THROWN ) THEN
        IF ( PRESENT(FOUND) ) FOUND = .FALSE.
        RETURN
    END IF

    NULLIFY(P)
    CALL JSON%GET(ME=ME, PATH=PATH, P=P)

    IF (.NOT. ASSOCIATED(P)) THEN
        CALL JSON%THROW_EXCEPTION('Error in json_rename_by_path:'//&
                                  ' Unable to resolve path: '//TRIM(PATH),FOUND)
    ELSE
        CALL JSON%RENAME(P,NAME)
        NULLIFY(P)
    END IF

    IF (JSON%EXCEPTION_THROWN) THEN
        IF (PRESENT(FOUND)) THEN
            FOUND = .FALSE.
            CALL JSON%CLEAR_EXCEPTIONS()
        END IF
    ELSE
        IF (PRESENT(FOUND)) FOUND = .TRUE.
    END IF

    END SUBROUTINE JSON_RENAME_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_rename_by_path]], where "path" and "name" are kind=CDK

    SUBROUTINE WRAP_JSON_RENAME_BY_PATH(JSON, ME, PATH, NAME, FOUND)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)        :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)   :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN)  :: PATH
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN)  :: NAME
    LOGICAL(LK),INTENT(OUT),OPTIONAL      :: FOUND

    CALL JSON%RENAME(ME,TO_UNICODE(PATH),TO_UNICODE(NAME),FOUND)

    END SUBROUTINE WRAP_JSON_RENAME_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_rename_by_path]], where "name" is kind=CDK

    SUBROUTINE JSON_RENAME_BY_PATH_NAME_ASCII(JSON, ME, PATH, NAME, FOUND)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)        :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)   :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)   :: PATH
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN)  :: NAME
    LOGICAL(LK),INTENT(OUT),OPTIONAL      :: FOUND

    CALL JSON%RENAME(ME,PATH,TO_UNICODE(NAME),FOUND)

    END SUBROUTINE JSON_RENAME_BY_PATH_NAME_ASCII
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_rename_by_path]], where "path" is kind=CDK

    SUBROUTINE JSON_RENAME_BY_PATH_PATH_ASCII(JSON, ME, PATH, NAME, FOUND)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)        :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)   :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN)  :: PATH
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)   :: NAME
    LOGICAL(LK),INTENT(OUT),OPTIONAL      :: FOUND

    CALL JSON%RENAME(ME,TO_UNICODE(PATH),NAME,FOUND)

    END SUBROUTINE JSON_RENAME_BY_PATH_PATH_ASCII
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns the [[json_value]] pointer given the path string.
!
!--- Example
!
!````fortran
!    type(json_core) :: json
!    type(json_value),pointer :: dat,p
!    logical :: found
!    !...
!    call json%initialize(path_mode=1) ! this is the default so not strictly necessary.
!    call json%get(dat,'data(2).version',p,found)
!````
!
!--- Notes
!  The syntax used here is a subset of the
!  [http://goessner.net/articles/JsonPath/](JSONPath) "dot–notation".
!  The following special characters are used to denote paths:
!
!  * `$`           - root
!  * `@`           - this
!  * `.`           - child object member (note this can be changed using `json%path_separator`)
!  * `[]` or `()`  - child array element (note that indices are 1-based)
!
!  Thus, if any of these characters are present in the name key,
!  this routine cannot be used to get the value.
!  In that case, the `get_child` methods would need to be used.
!  Or, the alternate [[json_get_by_path_rfc6901]] could be used.
!
!--- See also
!  * [[json_get_by_path_rfc6901]]
!  * [[json_get_by_path_jsonpath_bracket]]
!
!@note The syntax is inherited from FSON, and is basically a subset
!      of JSONPath "dot-notation", with the additional allowance of
!      () for array elements.
!
!@note JSON `null` values are used here for unknown variables when `create_it` is True.
!      So, it is possible that an existing null variable can be converted to another
!      type (object or array) if a child is specified in the path. Doing it this way
!      to avoid having to use another type (say `json_unknown`) that would have to be
!      converted to null once all the variables have been created (user would have
!      had to do this).
!
!@warning See (**) in code. I think we need to protect for memory leaks when
!         changing the type of a variable that already exists.

    SUBROUTINE JSON_GET_BY_PATH_DEFAULT(JSON,ME,PATH,P,FOUND,CREATE_IT,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: ME          !! a JSON linked list
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)  :: PATH        !! path to the variable
    TYPE(JSON_VALUE),POINTER,INTENT(OUT) :: P           !! pointer to the variable
!! specify by `path`
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND       !! true if it was found
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: CREATE_IT   !! if a variable is not present
!! in the path, then it is created.
!! the leaf node is returned as
!! a `null` json type and can be
!! changed by the caller.
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED !! if `create_it` is true, this
!! will be true if the variable
!! was actually created. Otherwise
!! it will be false.

    INTEGER(IK)              :: I           !! counter of characters in `path`
    INTEGER(IK)              :: LENGTH      !! significant length of `path`
    INTEGER(IK)              :: CHILD_I     !! index for getting children
    CHARACTER(KIND=CK,LEN=1) :: C           !! a character in the `path`
    LOGICAL(LK)              :: ARRAY       !! flag when searching for array index in `path`
    TYPE(JSON_VALUE),POINTER :: TMP         !! temp variables for getting child objects
    LOGICAL(LK)              :: CHILD_FOUND !! if the child value was found
    LOGICAL(LK)              :: CREATE      !! if the object is to be created
    LOGICAL(LK)              :: CREATED     !! if `create` is true, then this will be
!! true if the leaf object had to be created
    INTEGER(IK)              :: J           !! counter of children when creating object
    LOGICAL(LK)              :: STATUS_OK   !! integer to string conversion flag

    NULLIFY(P)

    IF (.NOT. JSON%EXCEPTION_THROWN) THEN

        IF (PRESENT(CREATE_IT)) THEN
            CREATE = CREATE_IT
        ELSE
            CREATE = .FALSE.
        END IF

! default to assuming relative to me
        P => ME

        CHILD_I = 1
        ARRAY = .FALSE.
        CREATED = .FALSE.

!keep trailing space or not:
        IF (JSON%TRAILING_SPACES_SIGNIFICANT) THEN
            LENGTH = LEN(PATH)
        ELSE
            LENGTH = LEN_TRIM(PATH)
        END IF

        DO I=1, LENGTH

            C = PATH(I:I)

            SELECT CASE (C)
            CASE (ROOT)

! root
                DO WHILE (ASSOCIATED (P%PARENT))
                    P => P%PARENT
                END DO
                CHILD_I = I + 1
                IF (CREATE) CREATED = .FALSE. ! should always exist

            CASE (THIS)

! this
                P => ME
                CHILD_I = I + 1
                IF (CREATE) CREATED = .FALSE. ! should always exist

            CASE (START_ARRAY,START_ARRAY_ALT)

! start looking for the array element index
                ARRAY = .TRUE.

! get child member from p
                IF (CHILD_I < I) THEN
                    NULLIFY(TMP)
                    IF (CREATE) THEN

! Example:
!    'aaa.bbb(1)'
!     -> and aaa is a null, need to make it an object
!
!  What about the case: aaa.bbb(1)(3) ?
!  Is that already handled?

                        IF (P%VAR_TYPE==JSON_NULL) THEN             ! (**)
! if p was also created, then we need to
! convert it into an object here:
                            P%VAR_TYPE = JSON_OBJECT
                        END IF

! don't want to throw exceptions in this case
                        CALL JSON%GET_CHILD(P, PATH(CHILD_I:I-1), TMP, CHILD_FOUND)
                        IF (.NOT. CHILD_FOUND) THEN
! have to create this child
! [make it an array]
                            CALL JSON_VALUE_CREATE(TMP)
                            CALL JSON%TO_ARRAY(TMP,PATH(CHILD_I:I-1))
                            CALL JSON%ADD(P,TMP)
                            CREATED = .TRUE.
                        ELSE
                            CREATED = .FALSE.
                        END IF
                    ELSE
! call the normal way
                        CALL JSON%GET_CHILD(P, PATH(CHILD_I:I-1), TMP)
                    END IF
                    P => TMP
                ELSE
                    CHILD_I = I + 1     ! say, '@('
                    CYCLE
                END IF
                IF (.NOT. ASSOCIATED(P)) THEN
                    CALL JSON%THROW_EXCEPTION('Error in json_get_by_path_default:'//&
                                              ' Error getting array element',FOUND)
                    EXIT
                END IF
                CHILD_I = I + 1

            CASE (END_ARRAY,END_ARRAY_ALT)

                IF (.NOT. ARRAY) THEN
                    CALL JSON%THROW_EXCEPTION('Error in json_get_by_path_default:'//&
                                              ' Unexpected '//C,FOUND)
                    EXIT
                END IF
                ARRAY = .FALSE.
                CALL STRING_TO_INTEGER(PATH(CHILD_I:I-1),CHILD_I,STATUS_OK)
                IF (.NOT. STATUS_OK) THEN
                    CALL JSON%THROW_EXCEPTION('Error in json_get_by_path_default:'//&
                                              ' Could not convert array index to integer: '//&
                                              TRIM(PATH(CHILD_I:I-1)),FOUND)
                    EXIT
                END IF

                NULLIFY(TMP)
                IF (CREATE) THEN
! don't want to throw exceptions in this case
                    CALL JSON%GET_CHILD(P, CHILD_I, TMP, CHILD_FOUND)
                    IF (.NOT. CHILD_FOUND) THEN

                        IF (P%VAR_TYPE==JSON_NULL) THEN            ! (**)
! if p was also created, then we need to
! convert it into an array here:
                            P%VAR_TYPE = JSON_ARRAY
                        END IF

! have to create this element
! [make it a null]
! (and any missing ones before it)
                        DO J = 1, CHILD_I
                            NULLIFY(TMP)
                            CALL JSON%GET_CHILD(P, J, TMP, CHILD_FOUND)
                            IF (.NOT. CHILD_FOUND) THEN
                                CALL JSON_VALUE_CREATE(TMP)
                                CALL JSON%TO_NULL(TMP)  ! array element doesn't need a name
                                CALL JSON%ADD(P,TMP)
                                IF (J==CHILD_I) CREATED = .TRUE.
                            ELSE
                                IF (J==CHILD_I) CREATED = .FALSE.
                            END IF
                        END DO

                    ELSE
                        CREATED = .FALSE.
                    END IF

                ELSE
! call the normal way:
                    CALL JSON%GET_CHILD(P, CHILD_I, TMP)
                END IF

                P => TMP

                CHILD_I = I + 1

            CASE DEFAULT

                IF (C==JSON%PATH_SEPARATOR) THEN

! get child member from p
                    IF (CHILD_I < I) THEN
                        NULLIFY(TMP)
                        IF (CREATE) THEN
                            IF (P%VAR_TYPE==JSON_NULL) THEN            ! (**)
! if p was also created, then we need to
! convert it into an object here:
                                P%VAR_TYPE = JSON_OBJECT
                            END IF

! don't want to throw exceptions in this case
                            CALL JSON%GET_CHILD(P, PATH(CHILD_I:I-1), TMP, CHILD_FOUND)
                            IF (.NOT. CHILD_FOUND) THEN
! have to create this child
! [make it an object]
                                CALL JSON_VALUE_CREATE(TMP)
                                CALL JSON%TO_OBJECT(TMP,PATH(CHILD_I:I-1))
                                CALL JSON%ADD(P,TMP)
                                CREATED = .TRUE.
                            ELSE
                                CREATED = .FALSE.
                            END IF
                        ELSE
! call the normal way
                            CALL JSON%GET_CHILD(P, PATH(CHILD_I:I-1), TMP)
                        END IF
                        P => TMP
                    ELSE
                        CHILD_I = I + 1     ! say '$.', '@.', or ').'
                        CYCLE
                    END IF

                    IF (.NOT. ASSOCIATED(P)) THEN
                        CALL JSON%THROW_EXCEPTION('Error in json_get_by_path_default:'//&
                                                  ' Error getting child member.',FOUND)
                        EXIT
                    END IF

                    CHILD_I = I + 1

                END IF

            END SELECT

        END DO

        IF (JSON%EXCEPTION_THROWN) THEN

            IF (PRESENT(FOUND)) THEN
                NULLIFY(P) ! just in case
                FOUND = .FALSE.
                CALL JSON%CLEAR_EXCEPTIONS()
            END IF

        ELSE

! grab the last child if present in the path
            IF (CHILD_I <= LENGTH) THEN
                NULLIFY(TMP)
                IF (CREATE) THEN
                    IF (P%VAR_TYPE==JSON_NULL) THEN            ! (**)
! if p was also created, then we need to
! convert it into an object here:
                        P%VAR_TYPE = JSON_OBJECT
                    END IF

                    CALL JSON%GET_CHILD(P, PATH(CHILD_I:I-1), TMP, CHILD_FOUND)
                    IF (.NOT. CHILD_FOUND) THEN
! have to create this child
! (make it a null since it is the leaf)
                        CALL JSON_VALUE_CREATE(TMP)
                        CALL JSON%TO_NULL(TMP,PATH(CHILD_I:I-1))
                        CALL JSON%ADD(P,TMP)
                        CREATED = .TRUE.
                    ELSE
                        CREATED = .FALSE.
                    END IF
                ELSE
! call the normal way
                    CALL JSON%GET_CHILD(P, PATH(CHILD_I:I-1), TMP)
                END IF
                P => TMP
            ELSE
! we already have p
                IF (CREATE .AND. CREATED) THEN
! make leaf p a null, but only
! if it wasn't there
                    CALL JSON%TO_NULL(P)
                END IF
            END IF

! error checking
            IF (ASSOCIATED(P)) THEN
                IF (PRESENT(FOUND)) FOUND = .TRUE.    !everything seems to be ok
            ELSE
                CALL JSON%THROW_EXCEPTION('Error in json_get_by_path_default:'//&
                                          ' variable not found: '//TRIM(PATH),FOUND)
                IF (PRESENT(FOUND)) THEN
                    FOUND = .FALSE.
                    CALL JSON%CLEAR_EXCEPTIONS()
                END IF
            END IF

        END IF

! if it had to be created:
        IF (PRESENT(WAS_CREATED)) WAS_CREATED = CREATED

    ELSE
        IF (PRESENT(FOUND)) FOUND = .FALSE.
        IF (PRESENT(WAS_CREATED)) WAS_CREATED = .FALSE.
    END IF

    END SUBROUTINE JSON_GET_BY_PATH_DEFAULT
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 2/4/2017
!
!  Returns the [[json_value]] pointer given the path string,
!  using the "JSON Pointer" path specification defined by RFC 6901.
!
!  Note that trailing whitespace significance and case sensitivity
!  are user-specified. To fully conform to the RFC 6901 standard,
!  should probably set (via `initialize`):
!
!  * `case_sensitive_keys = .true.`         [this is the default setting]
!  * `trailing_spaces_significant = .true.` [this is *not* the default setting]
!  * `allow_duplicate_keys = .false.`       [this is *not* the default setting]
!
!--- Example
!
!````fortran
!    type(json_core) :: json
!    type(json_value),pointer :: dat,p
!    logical :: found
!    !...
!    call json%initialize(path_mode=2)
!    call json%get(dat,'/data/2/version',p,found)
!````
!
!--- See also
!  * [[json_get_by_path_default]]
!  * [[json_get_by_path_jsonpath_bracket]]
!
!--- Reference
!  * [JavaScript Object Notation (JSON) Pointer](https://tools.ietf.org/html/rfc6901)
!
!@note Not doing anything special about the `-` character to index an array.
!      This is considered a normal error.
!
!@note Unlike in the default path mode, the array indices here are 0-based
!      (in accordance with the RFC 6901 standard)
!
!@warning Not checking if the member that is referenced is unique.
!         (according to the standard, evaluation of non-unique references
!         should fail). Like [[json_get_by_path_default]], this one will just return
!         the first instance it encounters. This might be changed in the future.
!
!@warning I think the standard indicates that the input paths should use
!         escaped JSON strings (currently we are assuming they are not escaped).

    SUBROUTINE JSON_GET_BY_PATH_RFC6901(JSON, ME, PATH, P, FOUND)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: ME     !! a JSON linked list
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)  :: PATH   !! path to the variable
!! (an RFC 6901 "JSON Pointer")
    TYPE(JSON_VALUE),POINTER,INTENT(OUT) :: P      !! pointer to the variable
!! specify by `path`
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND  !! true if it was found

    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: TOKEN  !! a token in the path (between the `/` characters)
    INTEGER(IK)              :: I                  !! counter
    INTEGER(IK)              :: ISLASH_CURR        !! location of current '/' character in the path
    INTEGER(IK)              :: ISLASH_NEXT        !! location of next '/' character in the path
    INTEGER(IK)              :: ILEN               !! length of `path` string
    TYPE(JSON_VALUE),POINTER :: TMP                !! temporary variable for traversing the structure
    INTEGER(IK)              :: IVAL               !! integer array index value (0-based)
    LOGICAL(LK)              :: STATUS_OK          !! error flag
    LOGICAL(LK)              :: CHILD_FOUND        !! for getting child values

    NULLIFY(P)

    IF (.NOT. JSON%EXCEPTION_THROWN) THEN

        P => ME ! initialize

        IF (PATH/=CK_'') THEN

            IF (PATH(1:1)==SLASH) THEN  ! the first character must be a slash

                ISLASH_CURR = 1   ! initialize current slash index

!keep trailing space or not:
                IF (JSON%TRAILING_SPACES_SIGNIFICANT) THEN
                    ILEN = LEN(PATH)
                ELSE
                    ILEN = LEN_TRIM(PATH)
                END IF

                DO

! get the next token by finding the slashes
!
!  1   2 3
!  /abc/d/efg

                    IF (ISLASH_CURR==ILEN) THEN
!the last token is an empty string
                        TOKEN = CK_''
                        ISLASH_NEXT = 0  ! will signal to stop
                    ELSE

!      .
! '/123/567/'

! index in remaining string:
                        ISLASH_NEXT = INDEX(PATH(ISLASH_CURR+1:ILEN),SLASH)
                        IF (ISLASH_NEXT<=0) THEN
!last token:
                            TOKEN = PATH(ISLASH_CURR+1:ILEN)
                        ELSE
! convert to actual index in path:
                            ISLASH_NEXT = ISLASH_CURR + INDEX(PATH(ISLASH_CURR+1:ILEN),SLASH)
                            IF (ISLASH_NEXT>ISLASH_CURR+1) THEN
                                TOKEN = PATH(ISLASH_CURR+1:ISLASH_NEXT-1)
                            ELSE
!empty token:
                                TOKEN = CK_''
                            END IF
                        END IF

                    END IF

! remove trailing spaces in the token here if necessary:
                    IF (.NOT. JSON%TRAILING_SPACES_SIGNIFICANT) &
                        TOKEN = TRIM(TOKEN)

! decode the token:
                    TOKEN = DECODE_RFC6901(TOKEN)

! now, parse the token:

! first see if there is a child with this name
                    CALL JSON%GET_CHILD(P,TOKEN,TMP,CHILD_FOUND)
                    IF (CHILD_FOUND) THEN
! it was found
                        P => TMP
                    ELSE
! No key with this name.
! Is it an integer? If so,
! it might be an array index.
                        STATUS_OK = (LEN(TOKEN)>0)
                        IF (STATUS_OK) THEN
                            DO I=1,LEN(TOKEN)
! It must only contain (0..9) characters
! (it must be unsigned)
                                IF (SCAN(TOKEN(I:I),CK_'0123456789')<1) THEN
                                    STATUS_OK = .FALSE.
                                    EXIT
                                END IF
                            END DO
                            IF (STATUS_OK) THEN
                                IF (LEN(TOKEN)>1 .AND. TOKEN(1:1)==CK_'0') THEN
! leading zeros not allowed for some reason
                                    STATUS_OK = .FALSE.
                                END IF
                            END IF
                            IF (STATUS_OK) THEN
! if we make it this far, it should be
! convertible to an integer, so do it.
                                CALL STRING_TO_INTEGER(TOKEN,IVAL,STATUS_OK)
                            END IF
                        END IF
                        IF (STATUS_OK) THEN
! ival is an array index (0-based)
                            CALL JSON%GET_CHILD(P,IVAL+1_IK,TMP,CHILD_FOUND)
                            IF (CHILD_FOUND) THEN
                                P => TMP
                            ELSE
! not found
                                STATUS_OK = .FALSE.
                            END IF
                        END IF
                        IF (.NOT. STATUS_OK) THEN
                            CALL JSON%THROW_EXCEPTION('Error in json_get_by_path_rfc6901: '//&
                                                      'invalid path specification: '//TRIM(PATH),FOUND)
                            EXIT
                        END IF
                    END IF

                    IF (ISLASH_NEXT<=0) EXIT ! finished

! set up for next token:
                    ISLASH_CURR = ISLASH_NEXT

                END DO

            ELSE
                CALL JSON%THROW_EXCEPTION('Error in json_get_by_path_rfc6901: '//&
                                            'invalid path specification: '//TRIM(PATH),FOUND)
            END IF
        END IF

        IF (JSON%EXCEPTION_THROWN) THEN
            NULLIFY(P)
            IF (PRESENT(FOUND)) THEN
                FOUND = .FALSE.
                CALL JSON%CLEAR_EXCEPTIONS()
            END IF
        ELSE
            IF (PRESENT(FOUND)) FOUND = .TRUE.
        END IF

    ELSE
        IF (PRESENT(FOUND)) FOUND = .FALSE.
    END IF

    END SUBROUTINE JSON_GET_BY_PATH_RFC6901
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 9/2/2017
!
!  Returns the [[json_value]] pointer given the path string,
!  using the "JSON Pointer" path specification defined by the
!  JSONPath "bracket-notation".
!
!  The first character `$` is optional, and signifies the root
!  of the structure. If it is not present, then the first key
!  is taken to be in the `me` object.
!
!  Single or real quotes may be used.
!
!--- Example
!
!````fortran
!    type(json_core) :: json
!    type(json_value),pointer :: dat,p
!    logical :: found
!    !...
!    call json%initialize(path_mode=3)
!    call json%get(dat,"$['store']['book'][1]['title']",p,found)
!````
!
!--- See also
!  * [[json_get_by_path_default]]
!  * [[json_get_by_path_rfc6901]]
!
!--- Reference
!  * [JSONPath](http://goessner.net/articles/JsonPath/)
!
!@note Uses 1-based array indices (same as [[json_get_by_path_default]],
!      but unlike [[json_get_by_path_rfc6901]] which uses 0-based indices).
!
!@note When `create_it=True`, if the variable already exists and is a type
!      that is not compatible with the usage in the `path`, then it is
!      destroyed and replaced with what is specified in the `path`. Note that
!      this applies the all variables in the path as it is created. Currently,
!      this behavior is different from [[json_get_by_path_default]].
!
!@note JSON `null` values are used here for unknown variables
!      when `create_it` is True.
!
!@warning Note that if using single quotes, this routine cannot parse
!         a key containing `']`. If using real quotes, this routine
!         cannot parse a key containing `"]`. If the key contains both
!         `']` and `"]`, there is no way to parse it using this routine.

    SUBROUTINE JSON_GET_BY_PATH_JSONPATH_BRACKET(JSON,ME,PATH,P,FOUND,CREATE_IT,WAS_CREATED)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: ME          !! a JSON linked list
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)  :: PATH        !! path to the variable
!! (using JSONPath
!! "bracket-notation")
    TYPE(JSON_VALUE),POINTER,INTENT(OUT) :: P           !! pointer to the variable
!! specify by `path`
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND       !! true if it was found
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: CREATE_IT   !! if a variable is not present
!! in the path, then it is created.
!! the leaf node is returned as
!! a `null` json type and can be
!! changed by the caller.
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: WAS_CREATED !! if `create_it` is true, this
!! will be true if the variable
!! was actually created. Otherwise
!! it will be false.

    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: TOKEN  !! a token in the path
!! (between the `['']` or
!! `[]` characters)
    INTEGER(IK)              :: ISTART             !! location of current '['
!! character in the path
    INTEGER(IK)              :: IEND               !! location of current ']'
!! character in the path
    INTEGER(IK)              :: IVAL               !! integer array index value
    LOGICAL(LK)              :: STATUS_OK          !! error flag
    TYPE(JSON_VALUE),POINTER :: TMP                !! temporary variable for
!! traversing the structure
    INTEGER(IK)              :: I                  !! counter
    INTEGER(IK)              :: ILEN               !! length of `path` string
    LOGICAL(LK)              :: REAL_QUOTES      !! if the keys are enclosed in `"`,
!! rather than `'` tokens.
    LOGICAL(LK)              :: CREATE             !! if the object is to be created
    LOGICAL(LK)              :: CREATED            !! if `create` is true, then this will be
!! true if the leaf object had to be created
    INTEGER(IK)              :: J                  !! counter of children when creating object

!TODO instead of reallocating `token` all the time, just
!     allocate a big size and keep track of the length,
!     then just reallocate only if necessary.
!     [would probably be inefficient if there was a very large token,
!     and then a bunch of small ones... but for similarly-sized ones
!     it should be way more efficient since it would avoid most
!     reallocations.]

    NULLIFY(P)

    IF (.NOT. JSON%EXCEPTION_THROWN) THEN

        IF (PRESENT(CREATE_IT)) THEN
            CREATE = CREATE_IT
        ELSE
            CREATE = .FALSE.
        END IF

        P => ME ! initialize
        CREATED = .FALSE.

        IF (PATH==CK_'') THEN
            CALL JSON%THROW_EXCEPTION('Error in json_get_by_path_jsonpath_bracket: '//&
                                      'invalid path specification: '//TRIM(PATH),FOUND)
        ELSE

            IF (PATH(1:1)==ROOT .OR. PATH(1:1)==START_ARRAY) THEN ! the first character must be
! a `$` (root) or a `[`
! (element of `me`)

                IF (PATH(1:1)==ROOT) THEN
! go to the root
                    DO WHILE (ASSOCIATED (P%PARENT))
                        P => P%PARENT
                    END DO
                    IF (CREATE) CREATED = .FALSE. ! should always exist
                END IF

!path length (don't need trailing spaces:)
                ILEN = LEN_TRIM(PATH)

                IF (ILEN>1) THEN

                    ISTART = 2   ! initialize first '[' location index

                    DO

                        IF (ISTART>ILEN) EXIT  ! finished

! must be the next start bracket:
                        IF (PATH(ISTART:ISTART) /= START_ARRAY) THEN
                            CALL JSON%THROW_EXCEPTION(&
                                    'Error in json_get_by_path_jsonpath_bracket: '//&
                                    'expecting "[", found: "'//TRIM(PATH(ISTART:ISTART))//&
                                    '" in path: '//TRIM(PATH),FOUND)
                            EXIT
                        END IF

! get the next token by checking:
!
! * [''] -- is the token after istart a quote?
!           if so, then search for the next `']`
!
! * [1] -- if not, then maybe it is a number,
!          so search for the next `]`

! verify length of remaining string
                        IF (ISTART+2<=ILEN) THEN

                            REAL_QUOTES = PATH(ISTART+1:ISTART+1) == QUOTATION_MARK   ! ["

                            IF (REAL_QUOTES .OR. PATH(ISTART+1:ISTART+1)==SINGLE_QUOTE) THEN  ! ['

! it might be a key value: ['abc']

                                ISTART = ISTART + 1 ! move counter to ' index
                                IF (REAL_QUOTES) THEN
                                    IEND = ISTART + INDEX(PATH(ISTART+1:ILEN),&
                                           QUOTATION_MARK//END_ARRAY)  ! "]
                                ELSE
                                    IEND = ISTART + INDEX(PATH(ISTART+1:ILEN),&
                                           SINGLE_QUOTE//END_ARRAY)  ! ']
                                END IF
                                IF (IEND>ISTART) THEN

!     istart  iend
!       |       |
! ['p']['abcdefg']

                                    IF (IEND>ISTART+1) THEN
                                        TOKEN = PATH(ISTART+1:IEND-1)
                                    ELSE
                                        TOKEN = CK_''  ! blank string
                                    END IF
! remove trailing spaces in
! the token here if necessary:
                                    IF (.NOT. JSON%TRAILING_SPACES_SIGNIFICANT) &
                                        TOKEN = TRIM(TOKEN)

                                    IF (CREATE) THEN
! have a token, create it if necessary

! we need to convert it into an object here
! (e.g., if p was also just created)
! and destroy its data to prevent a memory leak
                                        CALL JSON%CONVERT(P,JSON_OBJECT)

! don't want to throw exceptions in this case
                                        CALL JSON%GET_CHILD(P,TOKEN,TMP,STATUS_OK)
                                        IF (.NOT. STATUS_OK) THEN
! have to create this child
! [make it a null since we don't
! know what it is yet]
                                            CALL JSON_VALUE_CREATE(TMP)
                                            CALL JSON%TO_NULL(TMP,TOKEN)
                                            CALL JSON%ADD(P,TMP)
                                            STATUS_OK = .TRUE.
                                            CREATED = .TRUE.
                                        ELSE
! it was already there.
                                            CREATED = .FALSE.
                                        END IF
                                    ELSE
! have a token, see if it is valid:
                                        CALL JSON%GET_CHILD(P,TOKEN,TMP,STATUS_OK)
                                    END IF

                                    IF (STATUS_OK) THEN
! it was found
                                        P => TMP
                                    ELSE
                                        CALL JSON%THROW_EXCEPTION(&
                                                'Error in json_get_by_path_jsonpath_bracket: '//&
                                                'invalid token found: "'//TOKEN//&
                                                '" in path: '//TRIM(PATH),FOUND)
                                        EXIT
                                    END IF
                                    IEND = IEND + 1 ! move counter to ] index
                                ELSE
                                    CALL JSON%THROW_EXCEPTION(&
                                            'Error in json_get_by_path_jsonpath_bracket: '//&
                                            'invalid path: '//TRIM(PATH),FOUND)
                                    EXIT
                                END IF

                            ELSE

! it might be an integer value: [123]

                                IEND = ISTART + INDEX(PATH(ISTART+1:ILEN),END_ARRAY)   ! ]
                                IF (IEND>ISTART+1) THEN

! this should be an integer:
                                    TOKEN = PATH(ISTART+1:IEND-1)

! verify that there are no spaces or other
! characters in the string:
                                    STATUS_OK = .TRUE.
                                    DO I=1,LEN(TOKEN)
! It must only contain (0..9) characters
! (it must be unsigned)
                                        IF (SCAN(TOKEN(I:I),CK_'0123456789')<1) THEN
                                            STATUS_OK = .FALSE.
                                            EXIT
                                        END IF
                                    END DO
                                    IF (STATUS_OK) THEN
                                        CALL STRING_TO_INTEGER(TOKEN,IVAL,STATUS_OK)
                                        IF (STATUS_OK) STATUS_OK = IVAL>0  ! assuming 1-based array indices
                                    END IF

                                    IF (STATUS_OK) THEN

! have a valid integer to use as an index
! see if this element is really there:
                                        CALL JSON%GET_CHILD(P,IVAL,TMP,STATUS_OK)

                                        IF (CREATE .AND. .NOT. STATUS_OK) THEN

! have to create it:

                                            IF (.NOT.(P%VAR_TYPE==JSON_OBJECT .OR. P%VAR_TYPE==JSON_ARRAY)) THEN
! we need to convert it into an array here
! (e.g., if p was also just created)
! and destroy its data to prevent a memory leak
                                                CALL JSON%CONVERT(P,JSON_ARRAY)
                                            END IF

! have to create this element
! [make it a null]
! (and any missing ones before it)
                                            DO J = 1, IVAL
                                                NULLIFY(TMP)
                                                CALL JSON%GET_CHILD(P, J, TMP, STATUS_OK)
                                                IF (.NOT. STATUS_OK) THEN
                                                    CALL JSON_VALUE_CREATE(TMP)
                                                    CALL JSON%TO_NULL(TMP)  ! array element doesn't need a name
                                                    CALL JSON%ADD(P,TMP)
                                                    IF (J==IVAL) CREATED = .TRUE.
                                                ELSE
                                                    IF (J==IVAL) CREATED = .FALSE.
                                                END IF
                                            END DO
                                            STATUS_OK = .TRUE.

                                        ELSE
                                            CREATED = .FALSE.
                                        END IF

                                        IF (STATUS_OK) THEN
! found it
                                            P => TMP
                                        ELSE
! not found
                                            CALL JSON%THROW_EXCEPTION(&
                                                    'Error in json_get_by_path_jsonpath_bracket: '//&
                                                    'invalid array index found: "'//TOKEN//&
                                                    '" in path: '//TRIM(PATH),FOUND)
                                            EXIT
                                        END IF
                                    ELSE
                                        CALL JSON%THROW_EXCEPTION(&
                                                'Error in json_get_by_path_jsonpath_bracket: '//&
                                                'invalid token: "'//TOKEN//&
                                                '" in path: '//TRIM(PATH),FOUND)
                                        EXIT
                                    END IF

                                ELSE
                                    CALL JSON%THROW_EXCEPTION(&
                                            'Error in json_get_by_path_jsonpath_bracket: '//&
                                            'invalid path: '//TRIM(PATH),FOUND)
                                    EXIT
                                END IF

                            END IF

                        ELSE
                            CALL JSON%THROW_EXCEPTION(&
                                    'Error in json_get_by_path_jsonpath_bracket: '//&
                                    'invalid path: '//TRIM(PATH),FOUND)
                            EXIT
                        END IF

! set up for next token:
                        ISTART = IEND + 1

                    END DO

                END IF

            ELSE
                CALL JSON%THROW_EXCEPTION(&
                        'Error in json_get_by_path_jsonpath_bracket: '//&
                        'expecting "'//ROOT//'", found: "'//PATH(1:1)//&
                        '" in path: '//TRIM(PATH),FOUND)
            END IF

        END IF

        IF (JSON%EXCEPTION_THROWN) THEN
            NULLIFY(P)
            IF (PRESENT(FOUND)) THEN
                FOUND = .FALSE.
                CALL JSON%CLEAR_EXCEPTIONS()
            END IF
        ELSE
            IF (PRESENT(FOUND)) FOUND = .TRUE.
        END IF

! if it had to be created:
        IF (PRESENT(WAS_CREATED)) WAS_CREATED = CREATED

    ELSE
        IF (PRESENT(FOUND)) FOUND = .FALSE.
        IF (PRESENT(WAS_CREATED)) WAS_CREATED = .FALSE.
    END IF

    END SUBROUTINE JSON_GET_BY_PATH_JSONPATH_BRACKET
!*****************************************************************************************

!*****************************************************************************************
!>
!  Convert an existing JSON variable `p` to a different variable type.
!  The existing variable (and its children) is destroyed. It is replaced
!  in the structure by a new variable of type `var_type`
!  (which can be a `json_null`, `json_object` or `json_array`).
!
!@note This is an internal routine used when creating variables by path.

    SUBROUTINE CONVERT(JSON,P,VAR_TYPE)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT) :: JSON
    TYPE(JSON_VALUE),POINTER :: P  !! the variable to convert
    INTEGER(IK),INTENT(IN) :: VAR_TYPE !! the variable type to convert `p` to

    TYPE(JSON_VALUE),POINTER :: TMP  !! temporary variable
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: NAME !! the name of a JSON variable

    LOGICAL :: CONVERT_IT  !! if `p` needs to be converted

    CONVERT_IT = P%VAR_TYPE /= VAR_TYPE

    IF (CONVERT_IT) THEN

        CALL JSON%INFO(P,NAME=NAME) ! get existing name

        SELECT CASE (VAR_TYPE)
        CASE(JSON_OBJECT)
            CALL JSON%CREATE_OBJECT(TMP,NAME)
        CASE(JSON_ARRAY)
            CALL JSON%CREATE_ARRAY(TMP,NAME)
        CASE(JSON_NULL)
            CALL JSON%CREATE_NULL(TMP,NAME)
        CASE DEFAULT
            CALL JSON%THROW_EXCEPTION('Error in convert: invalid var_type value.')
            RETURN
        END SELECT

        CALL JSON%REPLACE(P,TMP,DESTROY=.TRUE.)
        P => TMP
        NULLIFY(TMP)

    END IF

    END SUBROUTINE CONVERT
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_by_path]] where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_GET_BY_PATH(JSON, ME, PATH, P, FOUND)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH
    TYPE(JSON_VALUE),POINTER,INTENT(OUT) :: P
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND

    CALL JSON%GET(ME, TO_UNICODE(PATH), P, FOUND)

    END SUBROUTINE WRAP_JSON_GET_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns the path to a JSON object that is part
!  of a linked list structure.
!
!  The path returned would be suitable for input to
!  [[json_get_by_path]] and related routines.
!
!@note If an error occurs (which in this case means a malformed
!      JSON structure) then an exception will be thrown, unless
!      `found` is present, which will be set to `false`. `path`
!      will be a blank string.
!
!@note If `json%path_mode/=1`, then the `use_alt_array_tokens`
!      and `path_sep` inputs are ignored if present.
!
!@note [http://goessner.net/articles/JsonPath/](JSONPath) (`path_mode=3`)
!      does not specify whether or not the keys should be escaped (this routine
!      assumes not, as does http://jsonpath.com).
!      Also, we are using Fortran-style 1-based array indices,
!      not 0-based, to agree with the assumption in `path_mode=1`

    SUBROUTINE JSON_GET_PATH(JSON, P, PATH, FOUND, USE_ALT_ARRAY_TOKENS, PATH_SEP)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                   :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)              :: P     !! a JSON linked list object
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT) :: PATH  !! path to the variable
    LOGICAL(LK),INTENT(OUT),OPTIONAL                 :: FOUND !! true if there were no problems
    LOGICAL(LK),INTENT(IN),OPTIONAL :: USE_ALT_ARRAY_TOKENS   !! if true, then '()' are used for array elements
!! otherwise, '[]' are used [default]
!! (only used if `path_mode=1`)
    CHARACTER(KIND=CK,LEN=1),INTENT(IN),OPTIONAL :: PATH_SEP  !! character to use for path separator
!! (otherwise use `json%path_separator`)
!! (only used if `path_mode=1`)

    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE       :: NAME         !! variable name
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE       :: PARENT_NAME  !! variable's parent name
    CHARACTER(KIND=CK,LEN=MAX_INTEGER_STR_LEN) :: ISTR         !! for integer to string conversion
!! (array indices)
    TYPE(JSON_VALUE),POINTER :: TMP            !! for traversing the structure
    TYPE(JSON_VALUE),POINTER :: ELEMENT        !! for traversing the structure
    INTEGER(IK)              :: VAR_TYPE       !! JSON variable type flag
    INTEGER(IK)              :: I              !! counter
    INTEGER(IK)              :: N_CHILDREN     !! number of children for parent
    LOGICAL(LK)              :: USE_BRACKETS   !! to use '[]' characters for arrays
    LOGICAL(LK)              :: PARENT_IS_ROOT !! if the parent is the root
    CHARACTER(KIND=CK,LEN=1) :: ARRAY_START    !! for `path_mode=1`, the character to start arrays
    CHARACTER(KIND=CK,LEN=1) :: ARRAY_END      !! for `path_mode=1`, the character to end arrays
    LOGICAL                  :: CONSECUTIVE_ARRAYS      !! check for array of array case
    INTEGER(IK)              :: PARENTS_PARENT_VAR_TYPE !! `var_type` for parent's parent

!optional input:
    IF (PRESENT(USE_ALT_ARRAY_TOKENS)) THEN
        USE_BRACKETS = .NOT. USE_ALT_ARRAY_TOKENS
    ELSE
        USE_BRACKETS = .TRUE.
    END IF

    IF (JSON%PATH_MODE==1_IK) THEN
        IF (USE_BRACKETS) THEN
            ARRAY_START = START_ARRAY
            ARRAY_END   = END_ARRAY
        ELSE
            ARRAY_START = START_ARRAY_ALT
            ARRAY_END   = END_ARRAY_ALT
        END IF
    END IF

! initialize:
    CONSECUTIVE_ARRAYS = .FALSE.

    IF (ASSOCIATED(P)) THEN

!traverse the structure via parents up to the root
        TMP => P
        DO

            IF (.NOT. ASSOCIATED(TMP)) EXIT !finished

!get info about the current variable:
            CALL JSON%INFO(TMP,NAME=NAME)
            IF (JSON%PATH_MODE==2_IK) THEN
                NAME = ENCODE_RFC6901(NAME)
            END IF

! if tmp a child of an object, or an element of an array
            IF (ASSOCIATED(TMP%PARENT)) THEN

!get info about the parent:
                CALL JSON%INFO(TMP%PARENT,VAR_TYPE=VAR_TYPE,&
                               N_CHILDREN=N_CHILDREN,NAME=PARENT_NAME)
                IF (JSON%PATH_MODE==2_IK) THEN
                    PARENT_NAME = ENCODE_RFC6901(PARENT_NAME)
                END IF
                IF (ASSOCIATED(TMP%PARENT%PARENT)) THEN
                    CALL JSON%INFO(TMP%PARENT%PARENT,VAR_TYPE=PARENTS_PARENT_VAR_TYPE)
                    CONSECUTIVE_ARRAYS = PARENTS_PARENT_VAR_TYPE == JSON_ARRAY .AND. &
                                         VAR_TYPE == JSON_ARRAY
                ELSE
                    CONSECUTIVE_ARRAYS = .FALSE.
                END IF

                SELECT CASE (VAR_TYPE)
                CASE (JSON_ARRAY)

!get array index of this element:
                    ELEMENT => TMP%PARENT%CHILDREN
                    DO I = 1, N_CHILDREN
                        IF (.NOT. ASSOCIATED(ELEMENT)) THEN
                            CALL JSON%THROW_EXCEPTION('Error in json_get_path: '//&
                                                      'malformed JSON structure. ',FOUND)
                            EXIT
                        END IF
                        IF (ASSOCIATED(ELEMENT,TMP)) THEN
                            EXIT
                        ELSE
                            ELEMENT => ELEMENT%NEXT
                        END IF
                        IF (I==N_CHILDREN) THEN ! it wasn't found (should never happen)
                            CALL JSON%THROW_EXCEPTION('Error in json_get_path: '//&
                                                      'malformed JSON structure. ',FOUND)
                            EXIT
                        END IF
                    END DO
                    SELECT CASE(JSON%PATH_MODE)
                    CASE(3_IK)
! JSONPath "bracket-notation"
! example: `$['key'][1]`
! [note: this uses 1-based indices]
                        CALL INTEGER_TO_STRING(I,INT_FMT,ISTR)
                        IF (CONSECUTIVE_ARRAYS) THEN
                            CALL ADD_TO_PATH(START_ARRAY//TRIM(ADJUSTL(ISTR))//END_ARRAY,CK_'')
                        ELSE
                            CALL ADD_TO_PATH(START_ARRAY//SINGLE_QUOTE//PARENT_NAME//&
                                             SINGLE_QUOTE//END_ARRAY//&
                                             START_ARRAY//TRIM(ADJUSTL(ISTR))//END_ARRAY,CK_'')
                        END IF
                    CASE(2_IK)
! rfc6901
! Example: '/key/0'
                        CALL INTEGER_TO_STRING(I-1_IK,INT_FMT,ISTR) ! 0-based index
                        IF (CONSECUTIVE_ARRAYS) THEN
                            CALL ADD_TO_PATH(TRIM(ADJUSTL(ISTR)))
                        ELSE
                            CALL ADD_TO_PATH(PARENT_NAME//SLASH//TRIM(ADJUSTL(ISTR)))
                        END IF
                    CASE(1_IK)
! default
! Example: `key[1]`
                        CALL INTEGER_TO_STRING(I,INT_FMT,ISTR)
                        IF (CONSECUTIVE_ARRAYS) THEN
                            CALL ADD_TO_PATH(ARRAY_START//TRIM(ADJUSTL(ISTR))//ARRAY_END,PATH_SEP)
                        ELSE
                            CALL ADD_TO_PATH(PARENT_NAME//ARRAY_START//&
                                             TRIM(ADJUSTL(ISTR))//ARRAY_END,PATH_SEP)
                        END IF
                    END SELECT

                    IF (.NOT. CONSECUTIVE_ARRAYS) TMP => TMP%PARENT  ! already added parent name

                CASE (JSON_OBJECT)

                    IF (.NOT. CONSECUTIVE_ARRAYS) THEN
! idea is not to print the array name if
! it was already printed with the array

!process parent on the next pass
                        SELECT CASE(JSON%PATH_MODE)
                        CASE(3_IK)
                            CALL ADD_TO_PATH(START_ARRAY//SINGLE_QUOTE//NAME//&
                                            SINGLE_QUOTE//END_ARRAY,CK_'')
                        CASE DEFAULT
                            CALL ADD_TO_PATH(NAME,PATH_SEP)
                        END SELECT

                    END IF

                CASE DEFAULT

                    CALL JSON%THROW_EXCEPTION('Error in json_get_path: '//&
                                              'malformed JSON structure. '//&
                                              'A variable that is not an object '//&
                                              'or array should not have a child.',FOUND)
                    EXIT

                END SELECT

            ELSE
!the last one:
                SELECT CASE(JSON%PATH_MODE)
                CASE(3_IK)
                    CALL ADD_TO_PATH(START_ARRAY//SINGLE_QUOTE//NAME//&
                                     SINGLE_QUOTE//END_ARRAY,CK_'')
                CASE DEFAULT
                    CALL ADD_TO_PATH(NAME,PATH_SEP)
                END SELECT
            END IF

            IF (ASSOCIATED(TMP%PARENT)) THEN
!check if the parent is the root:
                PARENT_IS_ROOT = (.NOT. ASSOCIATED(TMP%PARENT%PARENT))
                IF (PARENT_IS_ROOT) EXIT
            END IF

!go to parent:
            TMP => TMP%PARENT

        END DO

    ELSE
        CALL JSON%THROW_EXCEPTION('Error in json_get_path: '//&
                                  'input pointer is not associated',FOUND)
    END IF

!for errors, return blank string:
    IF (JSON%EXCEPTION_THROWN .OR. .NOT. ALLOCATED(PATH)) THEN
        PATH = CK_''
    ELSE
        SELECT CASE (JSON%PATH_MODE)
        CASE(3_IK)
! add the outer level object identifier:
            PATH = ROOT//PATH
        CASE(2_IK)
! add the root slash:
            PATH = SLASH//PATH
        END SELECT
    END IF

!optional output:
    IF (PRESENT(FOUND)) THEN
        IF (JSON%EXCEPTION_THROWN) THEN
            FOUND = .FALSE.
            CALL JSON%CLEAR_EXCEPTIONS()
        ELSE
            FOUND = .TRUE.
        END IF
    END IF

    CONTAINS

        SUBROUTINE ADD_TO_PATH(STR,PATH_SEP)
!! prepend the string to the path
        IMPLICIT NONE
        CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: STR  !! string to prepend to `path`
        CHARACTER(KIND=CK,LEN=*),INTENT(IN),OPTIONAL :: PATH_SEP
!! path separator (default is '.').
!! (ignored if `json%path_mode/=1`)

        SELECT CASE (JSON%PATH_MODE)
        CASE(3_IK)
! in this case, the options are ignored
            IF (.NOT. ALLOCATED(PATH)) THEN
                PATH = STR
            ELSE
                PATH = STR//PATH
            END IF
        CASE(2_IK)
! in this case, the options are ignored
            IF (.NOT. ALLOCATED(PATH)) THEN
                PATH = STR
            ELSE
                PATH = STR//SLASH//PATH
            END IF
        CASE(1_IK)
! default path format
            IF (.NOT. ALLOCATED(PATH)) THEN
                PATH = STR
            ELSE
! shouldn't add the path_sep for cases like x[1][2]
! [if current is an array element, and the previous was
! also an array element] so check for that here:
                IF (.NOT. ( STR(LEN(STR):LEN(STR))==ARRAY_END .AND. &
                            PATH(1:1)==ARRAY_START )) THEN
                    IF (PRESENT(PATH_SEP)) THEN
! use user specified:
                        PATH = STR//PATH_SEP//PATH
                    ELSE
! use the default:
                        PATH = STR//JSON%PATH_SEPARATOR//PATH
                    END IF
                ELSE
                    PATH = STR//PATH
                END IF
            END IF
        END SELECT

        END SUBROUTINE ADD_TO_PATH

    END SUBROUTINE JSON_GET_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Wrapper for [[json_get_path]] where "path" and "path_sep" are kind=CDK.

    SUBROUTINE WRAP_JSON_GET_PATH(JSON, P, PATH, FOUND, USE_ALT_ARRAY_TOKENS, PATH_SEP)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                    :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)               :: P     !! a JSON linked list object
    CHARACTER(KIND=CDK,LEN=:),ALLOCATABLE,INTENT(OUT) :: PATH  !! path to the variable
    LOGICAL(LK),INTENT(OUT),OPTIONAL                  :: FOUND !! true if there were no problems
    LOGICAL(LK),INTENT(IN),OPTIONAL :: USE_ALT_ARRAY_TOKENS    !! if true, then '()' are used
!! for array elements otherwise,
!! '[]' are used [default]
    CHARACTER(KIND=CDK,LEN=1),INTENT(IN),OPTIONAL :: PATH_SEP  !! character to use for path
!! separator (default is '.')

    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: CK_PATH  !! path to the variable

! call the main routine:
    IF (PRESENT(PATH_SEP)) THEN
        CALL JSON%GET_PATH(P,CK_PATH,FOUND,USE_ALT_ARRAY_TOKENS,TO_UNICODE(PATH_SEP))
    ELSE
        CALL JSON%GET_PATH(P,CK_PATH,FOUND,USE_ALT_ARRAY_TOKENS)
    END IF

! from unicode:
    PATH = CK_PATH

    END SUBROUTINE WRAP_JSON_GET_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Convert a string into an integer.
!
!@note Replacement for the `parse_integer` function in the original code.

    FUNCTION STRING_TO_INT(JSON,STR) RESULT(IVAL)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: STR   !! a string
    INTEGER(IK)                         :: IVAL  !! `str` converted to an integer

    LOGICAL(LK) :: STATUS_OK !! error flag for [[string_to_integer]]

! call the core routine:
    CALL STRING_TO_INTEGER(STR,IVAL,STATUS_OK)

    IF (.NOT. STATUS_OK) THEN
        IVAL = 0
        CALL JSON%THROW_EXCEPTION('Error in string_to_int: '//&
                                    'string cannot be converted to an integer: '//&
                                    TRIM(STR))
    END IF

    END FUNCTION STRING_TO_INT
!*****************************************************************************************

!*****************************************************************************************
!>
!  Convert a string into a `real(RK)` value.

    FUNCTION STRING_TO_DBLE(JSON,STR) RESULT(RVAL)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: STR   !! a string
    REAL(RK)                            :: RVAL  !! `str` converted to a `real(RK)`

    LOGICAL(LK) :: STATUS_OK  !! error flag for [[string_to_real]]

    CALL STRING_TO_REAL(STR,JSON%USE_QUIET_NAN,RVAL,STATUS_OK)

    IF (.NOT. STATUS_OK) THEN    !if there was an error
        RVAL = 0.0_RK
        CALL JSON%THROW_EXCEPTION('Error in string_to_dble: '//&
                                  'string cannot be converted to a real: '//&
                                  TRIM(STR))
    END IF

    END FUNCTION STRING_TO_DBLE
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get an integer value from a [[json_value]].

    SUBROUTINE JSON_GET_INTEGER(JSON, ME, VALUE)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN) :: ME
    INTEGER(IK),INTENT(OUT)             :: VALUE  !! the integer value

    LOGICAL(LK) :: STATUS_OK !! for [[string_to_integer]]

    VALUE = 0_IK
    IF ( JSON%EXCEPTION_THROWN ) RETURN

    IF (ME%VAR_TYPE == JSON_INTEGER) THEN
        VALUE = ME%INT_VALUE
    ELSE
        IF (JSON%STRICT_TYPE_CHECKING) THEN
            IF (ALLOCATED(ME%NAME)) THEN
                CALL JSON%THROW_EXCEPTION('Error in json_get_integer:'//&
                    ' Unable to resolve value to integer: '//ME%NAME)
            ELSE
                CALL JSON%THROW_EXCEPTION('Error in json_get_integer:'//&
                    ' Unable to resolve value to integer')
            END IF
        ELSE
!type conversions
            SELECT CASE(ME%VAR_TYPE)
            CASE (JSON_REAL)
                VALUE = INT(ME%DBL_VALUE, IK)
            CASE (JSON_LOGICAL)
                IF (ME%LOG_VALUE) THEN
                    VALUE = 1_IK
                ELSE
                    VALUE = 0_IK
                END IF
            CASE (JSON_STRING)
                CALL STRING_TO_INTEGER(ME%STR_VALUE,VALUE,STATUS_OK)
                IF (.NOT. STATUS_OK) THEN
                    VALUE = 0_IK
                    IF (ALLOCATED(ME%NAME)) THEN
                        CALL JSON%THROW_EXCEPTION('Error in json_get_integer:'//&
                            ' Unable to convert string value to integer: '//&
                            ME%NAME//' = '//TRIM(ME%STR_VALUE))
                    ELSE
                        CALL JSON%THROW_EXCEPTION('Error in json_get_integer:'//&
                            ' Unable to convert string value to integer: '//&
                            TRIM(ME%STR_VALUE))
                    END IF
                END IF
            CASE DEFAULT
                IF (ALLOCATED(ME%NAME)) THEN
                    CALL JSON%THROW_EXCEPTION('Error in json_get_integer:'//&
                        ' Unable to resolve value to integer: '//ME%NAME)
                ELSE
                    CALL JSON%THROW_EXCEPTION('Error in json_get_integer:'//&
                        ' Unable to resolve value to integer')
                END IF
            END SELECT
        END IF
    END IF

    END SUBROUTINE JSON_GET_INTEGER
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get an integer value from a [[json_value]], given the path string.

    SUBROUTINE JSON_GET_INTEGER_BY_PATH(JSON, ME, PATH, VALUE, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN) :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH
    INTEGER(IK),INTENT(OUT)             :: VALUE
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND
    INTEGER(IK),INTENT(IN),OPTIONAL     :: DEFAULT !! default value if not found

    INTEGER(IK),PARAMETER :: DEFAULT_IF_NOT_SPECIFIED = 0_IK
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: ROUTINE = CK_'json_get_integer_by_path'

!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_get_scalar_by_path.inc" 1
    TYPE(JSON_VALUE),POINTER :: P

    IF (PRESENT(DEFAULT)) THEN
        VALUE = DEFAULT
    ELSE
        VALUE = DEFAULT_IF_NOT_SPECIFIED
    END IF

    IF ( JSON%EXCEPTION_THROWN ) THEN
       CALL FLAG_NOT_FOUND(FOUND)
       RETURN
    END IF

    NULLIFY(P)
    CALL JSON%GET(ME=ME, PATH=PATH, P=P)

    IF (.NOT. ASSOCIATED(P)) THEN
        CALL JSON%THROW_EXCEPTION('Error in '//ROUTINE//':'//&
                                  ' Unable to resolve path: '// TRIM(PATH),FOUND)
    ELSE
        CALL JSON%GET(P,VALUE)
    END IF

    IF ( JSON%EXCEPTION_THROWN ) THEN
        IF ( PRESENT(FOUND) .OR. PRESENT(DEFAULT)) THEN
            CALL FLAG_NOT_FOUND(FOUND)
            IF (PRESENT(DEFAULT)) VALUE = DEFAULT
            CALL JSON%CLEAR_EXCEPTIONS()
        END IF
    ELSE
        IF ( PRESENT(FOUND) ) FOUND = .TRUE.
    END IF
!- 8210 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_value_module.F90" 2

    END SUBROUTINE JSON_GET_INTEGER_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_integer_by_path]], where "path" is kind=CDK.

    SUBROUTINE WRAP_JSON_GET_INTEGER_BY_PATH(JSON, ME, PATH, VALUE, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH
    INTEGER(IK),INTENT(OUT)              :: VALUE
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND
    INTEGER(IK),INTENT(IN),OPTIONAL      :: DEFAULT !! default value if not found

    CALL JSON%GET(ME, TO_UNICODE(PATH), VALUE, FOUND, DEFAULT)

    END SUBROUTINE WRAP_JSON_GET_INTEGER_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 5/14/2014
!
!  Get an integer vector from a [[json_value]].

    SUBROUTINE JSON_GET_INTEGER_VEC(JSON, ME, VEC)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                   :: JSON
    TYPE(JSON_VALUE),POINTER                         :: ME
    INTEGER(IK),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC

    LOGICAL(LK) :: INITIALIZED

    IF ( JSON%EXCEPTION_THROWN ) RETURN

! check for 0-length arrays first:
    SELECT CASE (ME%VAR_TYPE)
    CASE (JSON_ARRAY)
        IF (JSON%COUNT(ME)==0) THEN
            ALLOCATE(VEC(0))
            RETURN
        END IF
    END SELECT

    INITIALIZED = .FALSE.

!the callback function is called for each element of the array:
    CALL JSON%GET(ME, ARRAY_CALLBACK=GET_INT_FROM_ARRAY)

    IF (JSON%EXCEPTION_THROWN .AND. ALLOCATED(VEC)) DEALLOCATE(VEC)

    CONTAINS

        SUBROUTINE GET_INT_FROM_ARRAY(JSON, ELEMENT, I, COUNT)

!! callback function for integer

        IMPLICIT NONE

        CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
        TYPE(JSON_VALUE),POINTER,INTENT(IN) :: ELEMENT
        INTEGER(IK),INTENT(IN)              :: I        !! index
        INTEGER(IK),INTENT(IN)              :: COUNT    !! size of array

!size the output array:
        IF (.NOT. INITIALIZED) THEN
            ALLOCATE(VEC(COUNT))
            INITIALIZED = .TRUE.
        END IF

!populate the elements:
        CALL JSON%GET(ELEMENT, VALUE=VEC(I))

        END SUBROUTINE GET_INT_FROM_ARRAY

    END SUBROUTINE JSON_GET_INTEGER_VEC
!*****************************************************************************************

!*****************************************************************************************
!>
!  If `found` is present, set it it false.

    SUBROUTINE FLAG_NOT_FOUND(FOUND)

    IMPLICIT NONE

    LOGICAL(LK),INTENT(OUT),OPTIONAL :: FOUND

    IF (PRESENT(FOUND)) FOUND = .FALSE.

    END SUBROUTINE FLAG_NOT_FOUND
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get an integer vector from a [[json_value]], given the path string.

    SUBROUTINE JSON_GET_INTEGER_VEC_BY_PATH(JSON, ME, PATH, VEC, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                   :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)              :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)              :: PATH
    INTEGER(IK),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC
    LOGICAL(LK),INTENT(OUT),OPTIONAL                 :: FOUND
    INTEGER(IK),DIMENSION(:),INTENT(IN),OPTIONAL     :: DEFAULT !! default value if not found

    CHARACTER(KIND=CK,LEN=*),PARAMETER :: ROUTINE = CK_'json_get_integer_vec_by_path'

!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_get_vec_by_path.inc" 1
    TYPE(JSON_VALUE),POINTER :: P

    IF ( JSON%EXCEPTION_THROWN ) THEN
        IF (PRESENT(DEFAULT)) VEC = DEFAULT
        CALL FLAG_NOT_FOUND(FOUND)
        RETURN
    END IF

    NULLIFY(P)
    CALL JSON%GET(ME=ME, PATH=PATH, P=P)

    IF (.NOT. ASSOCIATED(P)) THEN
        CALL JSON%THROW_EXCEPTION('Error in '//ROUTINE//':'//&
                                  ' Unable to resolve path: '// TRIM(PATH),FOUND)
    ELSE
        CALL JSON%GET(P,VEC)
    END IF

    IF ( JSON%EXCEPTION_THROWN ) THEN
        IF ( PRESENT(FOUND) .OR. PRESENT(DEFAULT)) THEN
            CALL FLAG_NOT_FOUND(FOUND)
            IF (PRESENT(DEFAULT)) VEC = DEFAULT
            CALL JSON%CLEAR_EXCEPTIONS()
        END IF
    ELSE
        IF ( PRESENT(FOUND) ) FOUND = .TRUE.
    END IF
!- 8328 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_value_module.F90" 2

    END SUBROUTINE JSON_GET_INTEGER_VEC_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_integer_vec_by_path]], where "path" is kind=CDK

    SUBROUTINE WRAP_JSON_GET_INTEGER_VEC_BY_PATH(JSON, ME, PATH, VEC, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                   :: JSON
    TYPE(JSON_VALUE),POINTER                         :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN)             :: PATH
    INTEGER(IK),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC
    LOGICAL(LK),INTENT(OUT),OPTIONAL                 :: FOUND
    INTEGER(IK),DIMENSION(:),INTENT(IN),OPTIONAL     :: DEFAULT !! default value if not found

    CALL JSON%GET(ME,PATH=TO_UNICODE(PATH),VEC=VEC,FOUND=FOUND,DEFAULT=DEFAULT)

    END SUBROUTINE WRAP_JSON_GET_INTEGER_VEC_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a real value from a [[json_value]].

    SUBROUTINE JSON_GET_REAL(JSON, ME, VALUE)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT) :: JSON
    TYPE(JSON_VALUE),POINTER       :: ME
    REAL(RK),INTENT(OUT)           :: VALUE

    LOGICAL(LK) :: STATUS_OK !! for [[string_to_real]]

    VALUE = 0.0_RK
    IF ( JSON%EXCEPTION_THROWN ) RETURN

    IF (ME%VAR_TYPE == JSON_REAL) THEN
        VALUE = ME%DBL_VALUE
    ELSE
        IF (JSON%STRICT_TYPE_CHECKING) THEN
            IF (ALLOCATED(ME%NAME)) THEN
                CALL JSON%THROW_EXCEPTION('Error in json_get_real:'//&
                                          ' Unable to resolve value to real: '//ME%NAME)
            ELSE
                CALL JSON%THROW_EXCEPTION('Error in json_get_real:'//&
                                          ' Unable to resolve value to real')
            END IF
        ELSE
!type conversions
            SELECT CASE (ME%VAR_TYPE)
            CASE (JSON_INTEGER)
                VALUE = REAL(ME%INT_VALUE, RK)
            CASE (JSON_LOGICAL)
                IF (ME%LOG_VALUE) THEN
                    VALUE = 1.0_RK
                ELSE
                    VALUE = 0.0_RK
                END IF
            CASE (JSON_STRING)
                CALL STRING_TO_REAL(ME%STR_VALUE,JSON%USE_QUIET_NAN,VALUE,STATUS_OK)
                IF (.NOT. STATUS_OK) THEN
                    VALUE = 0.0_RK
                    IF (ALLOCATED(ME%NAME)) THEN
                        CALL JSON%THROW_EXCEPTION('Error in json_get_real:'//&
                            ' Unable to convert string value to real: '//&
                            ME%NAME//' = '//TRIM(ME%STR_VALUE))
                    ELSE
                        CALL JSON%THROW_EXCEPTION('Error in json_get_real:'//&
                            ' Unable to convert string value to real: '//&
                            TRIM(ME%STR_VALUE))
                    END IF
                END IF
            CASE (JSON_NULL)
                IF (IEEE_SUPPORT_NAN(VALUE) .AND. JSON%NULL_TO_REAL_MODE/=1_IK) THEN
                    SELECT CASE (JSON%NULL_TO_REAL_MODE)
                    CASE(2_IK)
                        IF (JSON%USE_QUIET_NAN) THEN
                            VALUE = IEEE_VALUE(VALUE,IEEE_QUIET_NAN)
                        ELSE
                            VALUE = IEEE_VALUE(VALUE,IEEE_SIGNALING_NAN)
                        END IF
                    CASE(3_IK)
                        VALUE = 0.0_RK
                    END SELECT
                ELSE
                    IF (ALLOCATED(ME%NAME)) THEN
                        CALL JSON%THROW_EXCEPTION('Error in json_get_real:'//&
                                                ' Cannot convert null to NaN: '//ME%NAME)
                    ELSE
                        CALL JSON%THROW_EXCEPTION('Error in json_get_real:'//&
                                                ' Cannot convert null to NaN')
                    END IF
                END IF
            CASE DEFAULT
                IF (ALLOCATED(ME%NAME)) THEN
                    CALL JSON%THROW_EXCEPTION('Error in json_get_real:'//&
                                            ' Unable to resolve value to real: '//ME%NAME)
                ELSE
                    CALL JSON%THROW_EXCEPTION('Error in json_get_real:'//&
                                            ' Unable to resolve value to real')
                END IF
            END SELECT
        END IF
    END IF

    END SUBROUTINE JSON_GET_REAL
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a real value from a [[json_value]], given the path.

    SUBROUTINE JSON_GET_REAL_BY_PATH(JSON, ME, PATH, VALUE, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH
    REAL(RK),INTENT(OUT)                :: VALUE
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND
    REAL(RK),INTENT(IN),OPTIONAL        :: DEFAULT !! default value if not found

    REAL(RK),PARAMETER :: DEFAULT_IF_NOT_SPECIFIED = 0.0_RK
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: ROUTINE = CK_'json_get_real_by_path'

!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_get_scalar_by_path.inc" 1
    TYPE(JSON_VALUE),POINTER :: P

    IF (PRESENT(DEFAULT)) THEN
        VALUE = DEFAULT
    ELSE
        VALUE = DEFAULT_IF_NOT_SPECIFIED
    END IF

    IF ( JSON%EXCEPTION_THROWN ) THEN
       CALL FLAG_NOT_FOUND(FOUND)
       RETURN
    END IF

    NULLIFY(P)
    CALL JSON%GET(ME=ME, PATH=PATH, P=P)

    IF (.NOT. ASSOCIATED(P)) THEN
        CALL JSON%THROW_EXCEPTION('Error in '//ROUTINE//':'//&
                                  ' Unable to resolve path: '// TRIM(PATH),FOUND)
    ELSE
        CALL JSON%GET(P,VALUE)
    END IF

    IF ( JSON%EXCEPTION_THROWN ) THEN
        IF ( PRESENT(FOUND) .OR. PRESENT(DEFAULT)) THEN
            CALL FLAG_NOT_FOUND(FOUND)
            IF (PRESENT(DEFAULT)) VALUE = DEFAULT
            CALL JSON%CLEAR_EXCEPTIONS()
        END IF
    ELSE
        IF ( PRESENT(FOUND) ) FOUND = .TRUE.
    END IF
!- 8460 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_value_module.F90" 2

    END SUBROUTINE JSON_GET_REAL_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_real_by_path]], where "path" is kind=CDK

    SUBROUTINE WRAP_JSON_GET_REAL_BY_PATH(JSON, ME, PATH, VALUE, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH
    REAL(RK),INTENT(OUT)                 :: VALUE
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND
    REAL(RK),INTENT(IN),OPTIONAL         :: DEFAULT !! default value if not found

    CALL JSON%GET(ME,TO_UNICODE(PATH),VALUE,FOUND,DEFAULT)

    END SUBROUTINE WRAP_JSON_GET_REAL_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 5/14/2014
!
!  Get a real vector from a [[json_value]].

    SUBROUTINE JSON_GET_REAL_VEC(JSON, ME, VEC)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                :: JSON
    TYPE(JSON_VALUE),POINTER                      :: ME
    REAL(RK),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC

    LOGICAL(LK) :: INITIALIZED

    IF ( JSON%EXCEPTION_THROWN ) RETURN

! check for 0-length arrays first:
    SELECT CASE (ME%VAR_TYPE)
    CASE (JSON_ARRAY)
        IF (JSON%COUNT(ME)==0) THEN
            ALLOCATE(VEC(0))
            RETURN
        END IF
    END SELECT

    INITIALIZED = .FALSE.

!the callback function is called for each element of the array:
    CALL JSON%GET(ME, ARRAY_CALLBACK=GET_REAL_FROM_ARRAY)

    IF (JSON%EXCEPTION_THROWN .AND. ALLOCATED(VEC)) DEALLOCATE(VEC)

    CONTAINS

        SUBROUTINE GET_REAL_FROM_ARRAY(JSON, ELEMENT, I, COUNT)

!! callback function for real

        IMPLICIT NONE

        CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
        TYPE(JSON_VALUE),POINTER,INTENT(IN) :: ELEMENT
        INTEGER(IK),INTENT(IN)              :: I        !! index
        INTEGER(IK),INTENT(IN)              :: COUNT    !! size of array

!size the output array:
        IF (.NOT. INITIALIZED) THEN
            ALLOCATE(VEC(COUNT))
            INITIALIZED = .TRUE.
        END IF

!populate the elements:
        CALL JSON%GET(ELEMENT, VALUE=VEC(I))

        END SUBROUTINE GET_REAL_FROM_ARRAY

    END SUBROUTINE JSON_GET_REAL_VEC
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a real vector from a [[json_value]], given the path.

    SUBROUTINE JSON_GET_REAL_VEC_BY_PATH(JSON, ME, PATH, VEC, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)           :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)           :: PATH
    REAL(RK),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC
    LOGICAL(LK),INTENT(OUT),OPTIONAL              :: FOUND
    REAL(RK),DIMENSION(:),INTENT(IN),OPTIONAL     :: DEFAULT !! default value if not found

    CHARACTER(KIND=CK,LEN=*),PARAMETER :: ROUTINE = CK_'json_get_real_vec_by_path'

!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_get_vec_by_path.inc" 1
    TYPE(JSON_VALUE),POINTER :: P

    IF ( JSON%EXCEPTION_THROWN ) THEN
        IF (PRESENT(DEFAULT)) VEC = DEFAULT
        CALL FLAG_NOT_FOUND(FOUND)
        RETURN
    END IF

    NULLIFY(P)
    CALL JSON%GET(ME=ME, PATH=PATH, P=P)

    IF (.NOT. ASSOCIATED(P)) THEN
        CALL JSON%THROW_EXCEPTION('Error in '//ROUTINE//':'//&
                                  ' Unable to resolve path: '// TRIM(PATH),FOUND)
    ELSE
        CALL JSON%GET(P,VEC)
    END IF

    IF ( JSON%EXCEPTION_THROWN ) THEN
        IF ( PRESENT(FOUND) .OR. PRESENT(DEFAULT)) THEN
            CALL FLAG_NOT_FOUND(FOUND)
            IF (PRESENT(DEFAULT)) VEC = DEFAULT
            CALL JSON%CLEAR_EXCEPTIONS()
        END IF
    ELSE
        IF ( PRESENT(FOUND) ) FOUND = .TRUE.
    END IF
!- 8563 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_value_module.F90" 2

    END SUBROUTINE JSON_GET_REAL_VEC_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_real_vec_by_path]], where "path" is kind=CDK

    SUBROUTINE WRAP_JSON_GET_REAL_VEC_BY_PATH(JSON, ME, PATH, VEC, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                :: JSON
    TYPE(JSON_VALUE),POINTER                      :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN)          :: PATH
    REAL(RK),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC
    LOGICAL(LK),INTENT(OUT),OPTIONAL              :: FOUND
    REAL(RK),DIMENSION(:),INTENT(IN),OPTIONAL     :: DEFAULT !! default value if not found

    CALL JSON%GET(ME, TO_UNICODE(PATH), VEC, FOUND, DEFAULT)

    END SUBROUTINE WRAP_JSON_GET_REAL_VEC_BY_PATH
!*****************************************************************************************


!*****************************************************************************************
!>
!  Alternate version of [[json_get_real]] where value=real32.

    SUBROUTINE JSON_GET_REAL32(JSON, ME, VALUE)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT) :: JSON
    TYPE(JSON_VALUE),POINTER       :: ME
    REAL(REAL32),INTENT(OUT)       :: VALUE

    REAL(RK) :: TMP

    CALL JSON%GET(ME, TMP)
    VALUE = REAL(TMP,REAL32)

    END SUBROUTINE JSON_GET_REAL32
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_real_by_path]] where value=real32.

    SUBROUTINE JSON_GET_REAL32_BY_PATH(JSON, ME, PATH, VALUE, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH
    REAL(REAL32),INTENT(OUT)            :: VALUE
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND
    REAL(REAL32),INTENT(IN),OPTIONAL    :: DEFAULT !! default value if not found

    REAL(RK) :: TMP
    REAL(RK) :: TMP_DEFAULT

    IF (PRESENT(DEFAULT)) THEN
        TMP_DEFAULT = REAL(DEFAULT,RK)
        CALL JSON%GET(ME, PATH, TMP, FOUND, TMP_DEFAULT)
    ELSE
        CALL JSON%GET(ME, PATH, TMP, FOUND)
    END IF

    VALUE = REAL(TMP,REAL32)

    END SUBROUTINE JSON_GET_REAL32_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_real32_by_path]], where "path" is kind=CDK

    SUBROUTINE WRAP_JSON_GET_REAL32_BY_PATH(JSON, ME, PATH, VALUE, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH
    REAL(REAL32),INTENT(OUT)             :: VALUE
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND
    REAL(REAL32),INTENT(IN),OPTIONAL     :: DEFAULT !! default value if not found

    CALL JSON%GET(ME,TO_UNICODE(PATH),VALUE,FOUND,DEFAULT)

    END SUBROUTINE WRAP_JSON_GET_REAL32_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_real_vec]] where `vec` is `real32`.

    SUBROUTINE JSON_GET_REAL32_VEC(JSON, ME, VEC)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                    :: JSON
    TYPE(JSON_VALUE),POINTER                          :: ME
    REAL(REAL32),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC

    REAL(RK),DIMENSION(:),ALLOCATABLE :: TMP

    CALL JSON%GET(ME, TMP)
    IF (ALLOCATED(TMP)) VEC = REAL(TMP,REAL32)

    END SUBROUTINE JSON_GET_REAL32_VEC
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_real_vec_by_path]] where `vec` is `real32`.

    SUBROUTINE JSON_GET_REAL32_VEC_BY_PATH(JSON, ME, PATH, VEC, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                    :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)               :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)               :: PATH
    REAL(REAL32),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC
    LOGICAL(LK),INTENT(OUT),OPTIONAL                  :: FOUND
    REAL(REAL32),DIMENSION(:),INTENT(IN),OPTIONAL     :: DEFAULT !! default value if not found

    REAL(RK),DIMENSION(:),ALLOCATABLE :: TMP
    REAL(RK),DIMENSION(:),ALLOCATABLE :: TMP_DEFAULT

    IF (PRESENT(DEFAULT)) THEN
        TMP_DEFAULT = REAL(DEFAULT,RK)
        CALL JSON%GET(ME, PATH, TMP, FOUND, TMP_DEFAULT)
    ELSE
        CALL JSON%GET(ME, PATH, TMP, FOUND)
    END IF

    IF (ALLOCATED(TMP)) VEC = REAL(TMP,REAL32)

    END SUBROUTINE JSON_GET_REAL32_VEC_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_real32_vec_by_path]], where "path" is kind=CDK

    SUBROUTINE WRAP_JSON_GET_REAL32_VEC_BY_PATH(JSON, ME, PATH, VEC, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                    :: JSON
    TYPE(JSON_VALUE),POINTER                          :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN)              :: PATH
    REAL(REAL32),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC
    LOGICAL(LK),INTENT(OUT),OPTIONAL                  :: FOUND
    REAL(REAL32),DIMENSION(:),INTENT(IN),OPTIONAL     :: DEFAULT !! default value if not found

    CALL JSON%GET(ME, TO_UNICODE(PATH), VEC, FOUND, DEFAULT)

    END SUBROUTINE WRAP_JSON_GET_REAL32_VEC_BY_PATH
!*****************************************************************************************


!- 8867


!*****************************************************************************************
!>
!  Get a logical value from a [[json_value]].
!
!--- Note
!  If `strict_type_checking` is False, then the following assumptions are made:
!
!  * For integers: a value > 0 is True
!  * For reals: a value > 0 is True
!  * For strings: 'true' is True, and everything else is false. [case sensitive match]

    SUBROUTINE JSON_GET_LOGICAL(JSON, ME, VALUE)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN) :: ME
    LOGICAL(LK),INTENT(OUT)             :: VALUE

    VALUE = .FALSE.
    IF ( JSON%EXCEPTION_THROWN ) RETURN

    IF (ME%VAR_TYPE == JSON_LOGICAL) THEN
        VALUE = ME%LOG_VALUE
    ELSE
        IF (JSON%STRICT_TYPE_CHECKING) THEN
            IF (ALLOCATED(ME%NAME)) THEN
                CALL JSON%THROW_EXCEPTION('Error in json_get_logical: '//&
                                          'Unable to resolve value to logical: '//&
                                          ME%NAME)
            ELSE
                CALL JSON%THROW_EXCEPTION('Error in json_get_logical: '//&
                                          'Unable to resolve value to logical')
            END IF
        ELSE
!type conversions
            SELECT CASE (ME%VAR_TYPE)
            CASE (JSON_INTEGER)
                VALUE = (ME%INT_VALUE > 0_IK)
            CASE (JSON_REAL)
                VALUE = (ME%DBL_VALUE > 0.0_RK)
            CASE (JSON_STRING)
                VALUE = (ME%STR_VALUE == TRUE_STR)
            CASE DEFAULT
                IF (ALLOCATED(ME%NAME)) THEN
                    CALL JSON%THROW_EXCEPTION('Error in json_get_logical: '//&
                                              'Unable to resolve value to logical: '//&
                                              ME%NAME)
                ELSE
                    CALL JSON%THROW_EXCEPTION('Error in json_get_logical: '//&
                                              'Unable to resolve value to logical')
                END IF
            END SELECT
        END IF
    END IF

    END SUBROUTINE JSON_GET_LOGICAL
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a logical value from a [[json_value]], given the path.

    SUBROUTINE JSON_GET_LOGICAL_BY_PATH(JSON, ME, PATH, VALUE, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN) :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH
    LOGICAL(LK),INTENT(OUT)             :: VALUE
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND
    LOGICAL(LK),INTENT(IN),OPTIONAL     :: DEFAULT !! default value if not found

    LOGICAL(LK),PARAMETER :: DEFAULT_IF_NOT_SPECIFIED = .FALSE.
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: ROUTINE = CK_'json_get_logical_by_path'

!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_get_scalar_by_path.inc" 1
    TYPE(JSON_VALUE),POINTER :: P

    IF (PRESENT(DEFAULT)) THEN
        VALUE = DEFAULT
    ELSE
        VALUE = DEFAULT_IF_NOT_SPECIFIED
    END IF

    IF ( JSON%EXCEPTION_THROWN ) THEN
       CALL FLAG_NOT_FOUND(FOUND)
       RETURN
    END IF

    NULLIFY(P)
    CALL JSON%GET(ME=ME, PATH=PATH, P=P)

    IF (.NOT. ASSOCIATED(P)) THEN
        CALL JSON%THROW_EXCEPTION('Error in '//ROUTINE//':'//&
                                  ' Unable to resolve path: '// TRIM(PATH),FOUND)
    ELSE
        CALL JSON%GET(P,VALUE)
    END IF

    IF ( JSON%EXCEPTION_THROWN ) THEN
        IF ( PRESENT(FOUND) .OR. PRESENT(DEFAULT)) THEN
            CALL FLAG_NOT_FOUND(FOUND)
            IF (PRESENT(DEFAULT)) VALUE = DEFAULT
            CALL JSON%CLEAR_EXCEPTIONS()
        END IF
    ELSE
        IF ( PRESENT(FOUND) ) FOUND = .TRUE.
    END IF
!- 8947 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_value_module.F90" 2

    END SUBROUTINE JSON_GET_LOGICAL_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_logical_by_path]], where "path" is kind=CDK

    SUBROUTINE WRAP_JSON_GET_LOGICAL_BY_PATH(JSON, ME, PATH, VALUE, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH
    LOGICAL(LK),INTENT(OUT)              :: VALUE
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: DEFAULT !! default value if not found

    CALL JSON%GET(ME,TO_UNICODE(PATH),VALUE,FOUND,DEFAULT)

    END SUBROUTINE WRAP_JSON_GET_LOGICAL_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 5/14/2014
!
!  Get a logical vector from [[json_value]].

    SUBROUTINE JSON_GET_LOGICAL_VEC(JSON, ME, VEC)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                   :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)              :: ME
    LOGICAL(LK),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC

    LOGICAL(LK) :: INITIALIZED

    IF ( JSON%EXCEPTION_THROWN ) RETURN

! check for 0-length arrays first:
    SELECT CASE (ME%VAR_TYPE)
    CASE (JSON_ARRAY)
        IF (JSON%COUNT(ME)==0) THEN
            ALLOCATE(VEC(0))
            RETURN
        END IF
    END SELECT

    INITIALIZED = .FALSE.

!the callback function is called for each element of the array:
    CALL JSON%GET(ME, ARRAY_CALLBACK=GET_LOGICAL_FROM_ARRAY)

    IF (JSON%EXCEPTION_THROWN .AND. ALLOCATED(VEC)) DEALLOCATE(VEC)

    CONTAINS

        SUBROUTINE GET_LOGICAL_FROM_ARRAY(JSON, ELEMENT, I, COUNT)

!! callback function for logical

        IMPLICIT NONE

        CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
        TYPE(JSON_VALUE),POINTER,INTENT(IN) :: ELEMENT
        INTEGER(IK),INTENT(IN)              :: I        !! index
        INTEGER(IK),INTENT(IN)              :: COUNT    !! size of array

!size the output array:
        IF (.NOT. INITIALIZED) THEN
            ALLOCATE(VEC(COUNT))
            INITIALIZED = .TRUE.
        END IF

!populate the elements:
        CALL JSON%GET(ELEMENT, VALUE=VEC(I))

        END SUBROUTINE GET_LOGICAL_FROM_ARRAY

    END SUBROUTINE JSON_GET_LOGICAL_VEC
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a logical vector from a [[json_value]], given the path.

    SUBROUTINE JSON_GET_LOGICAL_VEC_BY_PATH(JSON, ME, PATH, VEC, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                   :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)              :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)              :: PATH
    LOGICAL(LK),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC
    LOGICAL(LK),INTENT(OUT),OPTIONAL                 :: FOUND
    LOGICAL(LK),DIMENSION(:),INTENT(IN),OPTIONAL     :: DEFAULT

    CHARACTER(KIND=CK,LEN=*),PARAMETER :: ROUTINE = CK_'json_get_logical_vec_by_path'

!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_get_vec_by_path.inc" 1
    TYPE(JSON_VALUE),POINTER :: P

    IF ( JSON%EXCEPTION_THROWN ) THEN
        IF (PRESENT(DEFAULT)) VEC = DEFAULT
        CALL FLAG_NOT_FOUND(FOUND)
        RETURN
    END IF

    NULLIFY(P)
    CALL JSON%GET(ME=ME, PATH=PATH, P=P)

    IF (.NOT. ASSOCIATED(P)) THEN
        CALL JSON%THROW_EXCEPTION('Error in '//ROUTINE//':'//&
                                  ' Unable to resolve path: '// TRIM(PATH),FOUND)
    ELSE
        CALL JSON%GET(P,VEC)
    END IF

    IF ( JSON%EXCEPTION_THROWN ) THEN
        IF ( PRESENT(FOUND) .OR. PRESENT(DEFAULT)) THEN
            CALL FLAG_NOT_FOUND(FOUND)
            IF (PRESENT(DEFAULT)) VEC = DEFAULT
            CALL JSON%CLEAR_EXCEPTIONS()
        END IF
    ELSE
        IF ( PRESENT(FOUND) ) FOUND = .TRUE.
    END IF
!- 9050 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_value_module.F90" 2

    END SUBROUTINE JSON_GET_LOGICAL_VEC_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_logical_vec_by_path]], where "path" is kind=CDK

    SUBROUTINE WRAP_JSON_GET_LOGICAL_VEC_BY_PATH(JSON, ME, PATH, VEC, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                   :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)              :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN)             :: PATH
    LOGICAL(LK),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC
    LOGICAL(LK),INTENT(OUT),OPTIONAL                 :: FOUND
    LOGICAL(LK),DIMENSION(:),INTENT(IN),OPTIONAL     :: DEFAULT

    CALL JSON%GET(ME,TO_UNICODE(PATH),VEC,FOUND,DEFAULT)

    END SUBROUTINE WRAP_JSON_GET_LOGICAL_VEC_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a character string from a [[json_value]].

    SUBROUTINE JSON_GET_STRING(JSON, ME, VALUE)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                   :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)              :: ME
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT) :: VALUE

    VALUE = CK_''
    IF (.NOT. JSON%EXCEPTION_THROWN) THEN

        IF (ME%VAR_TYPE == JSON_STRING) THEN

            IF (ALLOCATED(ME%STR_VALUE)) THEN
                IF (JSON%UNESCAPED_STRINGS) THEN
! default: it is stored already unescaped:
                    VALUE = ME%STR_VALUE
                ELSE
! return the escaped version:
                    CALL ESCAPE_STRING(ME%STR_VALUE, VALUE, JSON%ESCAPE_SOLIDUS)
                END IF
            ELSE
               CALL JSON%THROW_EXCEPTION('Error in json_get_string: '//&
                                         'me%str_value not allocated')
            END IF

        ELSE

            IF (JSON%STRICT_TYPE_CHECKING) THEN
                IF (ALLOCATED(ME%NAME)) THEN
                    CALL JSON%THROW_EXCEPTION('Error in json_get_string:'//&
                                              ' Unable to resolve value to string: '//ME%NAME)
                ELSE
                    CALL JSON%THROW_EXCEPTION('Error in json_get_string:'//&
                                              ' Unable to resolve value to string')
                END IF
            ELSE

                SELECT CASE (ME%VAR_TYPE)

                CASE (JSON_INTEGER)

                    IF (ALLOCATED(ME%INT_VALUE)) THEN
                        VALUE = REPEAT(SPACE, MAX_INTEGER_STR_LEN)
                        CALL INTEGER_TO_STRING(ME%INT_VALUE,INT_FMT,VALUE)
                        VALUE = TRIM(VALUE)
                    ELSE
                        CALL JSON%THROW_EXCEPTION('Error in json_get_string: '//&
                                                  'me%int_value not allocated')
                    END IF

                CASE (JSON_REAL)

                    IF (ALLOCATED(ME%DBL_VALUE)) THEN
                        VALUE = REPEAT(SPACE, MAX_NUMERIC_STR_LEN)
                        CALL REAL_TO_STRING(ME%DBL_VALUE,JSON%REAL_FMT,&
                                            JSON%NON_NORMALS_TO_NULL,&
                                            JSON%COMPACT_REAL,VALUE)
                        VALUE = TRIM(VALUE)
                    ELSE
                        CALL JSON%THROW_EXCEPTION('Error in json_get_string: '//&
                                                  'me%int_value not allocated')
                    END IF

                CASE (JSON_LOGICAL)

                    IF (ALLOCATED(ME%LOG_VALUE)) THEN
                        IF (ME%LOG_VALUE) THEN
                            VALUE = TRUE_STR
                        ELSE
                            VALUE = FALSE_STR
                        END IF
                    ELSE
                        CALL JSON%THROW_EXCEPTION('Error in json_get_string: '//&
                                                  'me%log_value not allocated')
                    END IF

                CASE (JSON_NULL)

                    VALUE = NULL_STR

                CASE DEFAULT
                    IF (ALLOCATED(ME%NAME)) THEN
                        CALL JSON%THROW_EXCEPTION('Error in json_get_string: '//&
                                                  'Unable to resolve value to characters: '//&
                                                  ME%NAME)
                    ELSE
                        CALL JSON%THROW_EXCEPTION('Error in json_get_string: '//&
                                                  'Unable to resolve value to characters')
                    END IF
                END SELECT

            END IF
        END IF

    END IF

    END SUBROUTINE JSON_GET_STRING
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a character string from a [[json_value]], given the path.

    SUBROUTINE JSON_GET_STRING_BY_PATH(JSON, ME, PATH, VALUE, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                   :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)              :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)              :: PATH
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT) :: VALUE
    LOGICAL(LK),INTENT(OUT),OPTIONAL                 :: FOUND
    CHARACTER(KIND=CK,LEN=*),INTENT(IN),OPTIONAL     :: DEFAULT

    CHARACTER(KIND=CK,LEN=*),PARAMETER :: DEFAULT_IF_NOT_SPECIFIED = CK_''
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: ROUTINE = CK_'json_get_string_by_path'

!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_get_scalar_by_path.inc" 1
    TYPE(JSON_VALUE),POINTER :: P

    IF (PRESENT(DEFAULT)) THEN
        VALUE = DEFAULT
    ELSE
        VALUE = DEFAULT_IF_NOT_SPECIFIED
    END IF

    IF ( JSON%EXCEPTION_THROWN ) THEN
       CALL FLAG_NOT_FOUND(FOUND)
       RETURN
    END IF

    NULLIFY(P)
    CALL JSON%GET(ME=ME, PATH=PATH, P=P)

    IF (.NOT. ASSOCIATED(P)) THEN
        CALL JSON%THROW_EXCEPTION('Error in '//ROUTINE//':'//&
                                  ' Unable to resolve path: '// TRIM(PATH),FOUND)
    ELSE
        CALL JSON%GET(P,VALUE)
    END IF

    IF ( JSON%EXCEPTION_THROWN ) THEN
        IF ( PRESENT(FOUND) .OR. PRESENT(DEFAULT)) THEN
            CALL FLAG_NOT_FOUND(FOUND)
            IF (PRESENT(DEFAULT)) VALUE = DEFAULT
            CALL JSON%CLEAR_EXCEPTIONS()
        END IF
    ELSE
        IF ( PRESENT(FOUND) ) FOUND = .TRUE.
    END IF
!- 9197 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_value_module.F90" 2

    END SUBROUTINE JSON_GET_STRING_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_string_by_path]], where "path" is kind=CDK

    SUBROUTINE WRAP_JSON_GET_STRING_BY_PATH(JSON, ME, PATH, VALUE, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                   :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)              :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN)             :: PATH
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT) :: VALUE
    LOGICAL(LK),INTENT(OUT),OPTIONAL                 :: FOUND
    CHARACTER(KIND=CK,LEN=*),INTENT(IN),OPTIONAL     :: DEFAULT

    CALL JSON%GET(ME,TO_UNICODE(PATH),VALUE,FOUND,DEFAULT)

    END SUBROUTINE WRAP_JSON_GET_STRING_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 5/14/2014
!
!  Get a string vector from a [[json_value(type)]].

    SUBROUTINE JSON_GET_STRING_VEC(JSON, ME, VEC)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                                :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)                           :: ME
    CHARACTER(KIND=CK,LEN=*),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC

    LOGICAL(LK) :: INITIALIZED

    IF ( JSON%EXCEPTION_THROWN ) RETURN

! check for 0-length arrays first:
    SELECT CASE (ME%VAR_TYPE)
    CASE (JSON_ARRAY)
        IF (JSON%COUNT(ME)==0) THEN
            ALLOCATE(VEC(0))
            RETURN
        END IF
    END SELECT

    INITIALIZED = .FALSE.

!the callback function is called for each element of the array:
    CALL JSON%GET(ME, ARRAY_CALLBACK=GET_CHARS_FROM_ARRAY)

    IF (JSON%EXCEPTION_THROWN .AND. ALLOCATED(VEC)) DEALLOCATE(VEC)

    CONTAINS

        SUBROUTINE GET_CHARS_FROM_ARRAY(JSON, ELEMENT, I, COUNT)

!! callback function for chars

        IMPLICIT NONE

        CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
        TYPE(JSON_VALUE),POINTER,INTENT(IN) :: ELEMENT
        INTEGER(IK),INTENT(IN)              :: I        !! index
        INTEGER(IK),INTENT(IN)              :: COUNT    !! size of array

        CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: CVAL

!size the output array:
        IF (.NOT. INITIALIZED) THEN
            ALLOCATE(VEC(COUNT))
            INITIALIZED = .TRUE.
        END IF

!populate the elements:
        CALL JSON%GET(ELEMENT, VALUE=CVAL)
        IF (ALLOCATED(CVAL)) THEN
            VEC(I) = CVAL
            DEALLOCATE(CVAL)
        ELSE
            VEC(I) = CK_''
        END IF

        END SUBROUTINE GET_CHARS_FROM_ARRAY

    END SUBROUTINE JSON_GET_STRING_VEC
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get a string vector from a [[json_value(type)]], given the path.

    SUBROUTINE JSON_GET_STRING_VEC_BY_PATH(JSON, ME, PATH, VEC, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                                :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)                           :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)                           :: PATH
    CHARACTER(KIND=CK,LEN=*),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC
    LOGICAL(LK),INTENT(OUT),OPTIONAL                              :: FOUND
    CHARACTER(KIND=CK,LEN=*),DIMENSION(:),INTENT(IN),OPTIONAL     :: DEFAULT

    CHARACTER(KIND=CK,LEN=*),PARAMETER :: ROUTINE = CK_'json_get_string_vec_by_path'

!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_get_vec_by_path.inc" 1
    TYPE(JSON_VALUE),POINTER :: P

    IF ( JSON%EXCEPTION_THROWN ) THEN
        IF (PRESENT(DEFAULT)) VEC = DEFAULT
        CALL FLAG_NOT_FOUND(FOUND)
        RETURN
    END IF

    NULLIFY(P)
    CALL JSON%GET(ME=ME, PATH=PATH, P=P)

    IF (.NOT. ASSOCIATED(P)) THEN
        CALL JSON%THROW_EXCEPTION('Error in '//ROUTINE//':'//&
                                  ' Unable to resolve path: '// TRIM(PATH),FOUND)
    ELSE
        CALL JSON%GET(P,VEC)
    END IF

    IF ( JSON%EXCEPTION_THROWN ) THEN
        IF ( PRESENT(FOUND) .OR. PRESENT(DEFAULT)) THEN
            CALL FLAG_NOT_FOUND(FOUND)
            IF (PRESENT(DEFAULT)) VEC = DEFAULT
            CALL JSON%CLEAR_EXCEPTIONS()
        END IF
    ELSE
        IF ( PRESENT(FOUND) ) FOUND = .TRUE.
    END IF
!- 9308 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_value_module.F90" 2

    END SUBROUTINE JSON_GET_STRING_VEC_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_string_vec_by_path]], where "path" is kind=CDK

    SUBROUTINE WRAP_JSON_GET_STRING_VEC_BY_PATH(JSON, ME, PATH, VEC, FOUND, DEFAULT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                                :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)                           :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN)                          :: PATH
    CHARACTER(KIND=CK,LEN=*),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC
    LOGICAL(LK),INTENT(OUT),OPTIONAL                              :: FOUND
    CHARACTER(KIND=CK,LEN=*),DIMENSION(:),INTENT(IN),OPTIONAL     :: DEFAULT

    CALL JSON%GET(ME,TO_UNICODE(PATH),VEC,FOUND,DEFAULT)

    END SUBROUTINE WRAP_JSON_GET_STRING_VEC_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/16/2016
!
!  Get a string vector from a [[json_value(type)]]. This is an alternate
!  version of [[json_get_string_vec]]. This one returns an allocatable
!  length character (where the string length is the maximum length of
!  any element in the array). It also returns an integer array of the
!  actual sizes of the strings in the JSON structure.
!
!@note This is somewhat inefficient since it does
!      cycle through the array twice.
!
!@warning The allocation of `vec` doesn't work with
!         gfortran 4.9 or 5 due to compiler bugs

    SUBROUTINE JSON_GET_ALLOC_STRING_VEC(JSON, ME, VEC, ILEN)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: ME
    CHARACTER(KIND=CK,LEN=:),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC
    INTEGER(IK),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: ILEN !! the actual length
!! of each character
!! string in the array

    LOGICAL(LK) :: INITIALIZED !! if the output array has been sized
    INTEGER(IK) :: MAX_LEN     !! the length of the longest string in the array

    IF ( JSON%EXCEPTION_THROWN ) RETURN

! check for 0-length arrays first:
    SELECT CASE (ME%VAR_TYPE)
    CASE (JSON_ARRAY)
        IF (JSON%COUNT(ME)==0) THEN
            ALLOCATE(CHARACTER(KIND=CK,LEN=0) :: VEC(0))
            ALLOCATE(ILEN(0))
            RETURN
        END IF
    END SELECT

    INITIALIZED = .FALSE.

    CALL JSON%STRING_INFO(ME,ILEN=ILEN,MAX_STR_LEN=MAX_LEN)
    IF (.NOT. JSON%EXCEPTION_THROWN) THEN
! now get each string using the callback function:
        CALL JSON%GET(ME, ARRAY_CALLBACK=GET_CHARS_FROM_ARRAY)
    END IF

    IF (JSON%EXCEPTION_THROWN) THEN
        IF (ALLOCATED(VEC))  DEALLOCATE(VEC)
        IF (ALLOCATED(ILEN)) DEALLOCATE(ILEN)
    END IF

    CONTAINS

        SUBROUTINE GET_CHARS_FROM_ARRAY(JSON, ELEMENT, I, COUNT)

!! callback function for chars

        IMPLICIT NONE

        CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
        TYPE(JSON_VALUE),POINTER,INTENT(IN) :: ELEMENT
        INTEGER(IK),INTENT(IN)              :: I        !! index
        INTEGER(IK),INTENT(IN)              :: COUNT    !! size of array

        CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: CVAL  !! for getting string

!size the output array:
        IF (.NOT. INITIALIZED) THEN
! string length long enough to hold the longest one
! Note that this doesn't work with gfortran 4.9 or 5.
            ALLOCATE( CHARACTER(KIND=CK,LEN=MAX_LEN) :: VEC(COUNT) )
            INITIALIZED = .TRUE.
        END IF

!populate the elements:
        CALL JSON%GET(ELEMENT, VALUE=CVAL)
        IF (ALLOCATED(CVAL)) THEN
            VEC(I)  = CVAL
            ILEN(I) = LEN(CVAL)  ! return the actual length
            DEALLOCATE(CVAL)
        ELSE
            VEC(I)  = CK_''
            ILEN(I) = 0
        END IF

        END SUBROUTINE GET_CHARS_FROM_ARRAY

    END SUBROUTINE JSON_GET_ALLOC_STRING_VEC
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_alloc_string_vec]] where input is the path.
!
!  This is an alternate version of [[json_get_string_vec_by_path]].
!  This one returns an allocatable length character (where the string
!  length is the maximum length of any element in the array). It also
!  returns an integer array of the actual sizes of the strings in the
!  JSON structure.
!
!@note An alternative to using this routine is to use [[json_get_array]] with
!      a callback function that gets the string from each element and populates
!      a user-defined string type.
!
!@note If the `default` argument is used, and `default_ilen` is not present,
!      then `ilen` will just be returned as the length of the `default` dummy
!      argument (all elements with the same length).

    SUBROUTINE JSON_GET_ALLOC_STRING_VEC_BY_PATH(JSON,ME,PATH,VEC,ILEN,FOUND,DEFAULT,DEFAULT_ILEN)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN) :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH
    CHARACTER(KIND=CK,LEN=:),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC
    INTEGER(IK),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: ILEN !! the actual length
!! of each character
!! string in the array
    LOGICAL(LK),INTENT(OUT),OPTIONAL :: FOUND
    CHARACTER(KIND=CK,LEN=*),DIMENSION(:),INTENT(IN),OPTIONAL :: DEFAULT
    INTEGER(IK),DIMENSION(:),INTENT(IN),OPTIONAL :: DEFAULT_ILEN !! the actual
!! length of `default`

    CHARACTER(KIND=CK,LEN=*),PARAMETER :: ROUTINE = CK_'json_get_alloc_string_vec_by_path'

!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_get_vec_by_path_alloc.inc" 1
    TYPE(JSON_VALUE),POINTER :: P

    IF ( JSON%EXCEPTION_THROWN ) THEN
        IF (PRESENT(DEFAULT)) THEN
            VEC = DEFAULT
            IF (PRESENT(DEFAULT_ILEN)) THEN
                ILEN = DEFAULT_ILEN
            ELSE
                ALLOCATE(ILEN(SIZE(DEFAULT)))
                ILEN = LEN(DEFAULT)
            END IF
        END IF
        CALL FLAG_NOT_FOUND(FOUND)
        RETURN
    END IF

    NULLIFY(P)
    CALL JSON%GET(ME=ME, PATH=PATH, P=P)

    IF (.NOT. ASSOCIATED(P)) THEN
        CALL JSON%THROW_EXCEPTION('Error in '//ROUTINE//':'//&
                                  ' Unable to resolve path: '// TRIM(PATH),FOUND)
    ELSE
        CALL JSON%GET(P,VEC,ILEN)
    END IF

    IF ( JSON%EXCEPTION_THROWN ) THEN
        IF ( PRESENT(FOUND) .OR. PRESENT(DEFAULT)) THEN
            CALL FLAG_NOT_FOUND(FOUND)
            IF (PRESENT(DEFAULT)) THEN
                VEC = DEFAULT
                IF (PRESENT(DEFAULT_ILEN)) THEN
                    ILEN = DEFAULT_ILEN
                ELSE
                    ALLOCATE(ILEN(SIZE(DEFAULT)))
                    ILEN = LEN(DEFAULT)
                END IF
            END IF
            CALL JSON%CLEAR_EXCEPTIONS()
        END IF
    ELSE
        IF ( PRESENT(FOUND) ) FOUND = .TRUE.
    END IF
!- 9463 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_value_module.F90" 2

    END SUBROUTINE JSON_GET_ALLOC_STRING_VEC_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_alloc_string_vec_by_path]], where "path" is kind=CDK

    SUBROUTINE WRAP_JSON_GET_ALLOC_STRING_VEC_BY_PATH(JSON,ME,PATH,VEC,ILEN,FOUND,DEFAULT,DEFAULT_ILEN)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)        :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)   :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN)  :: PATH
    CHARACTER(KIND=CK,LEN=:),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: VEC
    INTEGER(IK),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: ILEN !! the actual length
!! of each character
!! string in the array
    LOGICAL(LK),INTENT(OUT),OPTIONAL :: FOUND
    CHARACTER(KIND=CK,LEN=*),DIMENSION(:),INTENT(IN),OPTIONAL :: DEFAULT
    INTEGER(IK),DIMENSION(:),INTENT(IN),OPTIONAL :: DEFAULT_ILEN !! the actual
!! length of `default`

    CALL JSON%GET(ME,TO_UNICODE(PATH),VEC,ILEN,FOUND,DEFAULT,DEFAULT_ILEN)

    END SUBROUTINE WRAP_JSON_GET_ALLOC_STRING_VEC_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  This routine calls the user-supplied [[json_array_callback_func]]
!  subroutine for each element in the array.
!
!@note For integer, real, logical, and character arrays,
!      higher-level routines are provided (see `get` methods), so
!      this routine does not have to be used for those cases.

    RECURSIVE SUBROUTINE JSON_GET_ARRAY(JSON, ME, ARRAY_CALLBACK)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN) :: ME
    PROCEDURE(JSON_ARRAY_CALLBACK_FUNC) :: ARRAY_CALLBACK

    TYPE(JSON_VALUE),POINTER :: ELEMENT !! temp variable for getting elements
    INTEGER(IK) :: I      !! counter
    INTEGER(IK) :: COUNT  !! number of elements in the array

    IF ( JSON%EXCEPTION_THROWN ) RETURN

    SELECT CASE (ME%VAR_TYPE)
    CASE (JSON_ARRAY)
        COUNT = JSON%COUNT(ME)
        ELEMENT => ME%CHILDREN
        DO I = 1, COUNT ! callback for each child
            IF (.NOT. ASSOCIATED(ELEMENT)) THEN
                CALL JSON%THROW_EXCEPTION('Error in json_get_array: '//&
                                          'Malformed JSON linked list')
                RETURN
            END IF
            CALL ARRAY_CALLBACK(JSON, ELEMENT, I, COUNT)
            IF (JSON%EXCEPTION_THROWN) EXIT
            ELEMENT => ELEMENT%NEXT
        END DO
    CASE DEFAULT
        CALL JSON%THROW_EXCEPTION('Error in json_get_array:'//&
                                  ' Resolved value is not an array ')
    END SELECT

    END SUBROUTINE JSON_GET_ARRAY
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/28/2016
!
!  Traverse a JSON structure.
!  This routine calls the user-specified [[json_traverse_callback_func]]
!  for each element of the structure.

    SUBROUTINE JSON_TRAVERSE(JSON,P,TRAVERSE_CALLBACK)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)         :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)    :: P
    PROCEDURE(JSON_TRAVERSE_CALLBACK_FUNC) :: TRAVERSE_CALLBACK

    LOGICAL(LK) :: FINISHED !! can be used to stop the process

    IF (.NOT. JSON%EXCEPTION_THROWN) CALL TRAVERSE(P)

    CONTAINS

        RECURSIVE SUBROUTINE TRAVERSE(P)

!! recursive [[json_value]] traversal.

        IMPLICIT NONE

        TYPE(JSON_VALUE),POINTER,INTENT(IN) :: P

        TYPE(JSON_VALUE),POINTER :: ELEMENT  !! a child element
        INTEGER(IK) :: I        !! counter
        INTEGER(IK) :: ICOUNT   !! number of children

        IF (JSON%EXCEPTION_THROWN) RETURN
        CALL TRAVERSE_CALLBACK(JSON,P,FINISHED) ! first call for this object
        IF (FINISHED) RETURN

!for arrays and objects, have to also call for all children:
        IF (P%VAR_TYPE==JSON_ARRAY .OR. P%VAR_TYPE==JSON_OBJECT) THEN

            ICOUNT = JSON%COUNT(P) ! number of children
            IF (ICOUNT>0) THEN
                ELEMENT => P%CHILDREN   ! first one
                DO I = 1, ICOUNT        ! call for each child
                    IF (.NOT. ASSOCIATED(ELEMENT)) THEN
                        CALL JSON%THROW_EXCEPTION('Error in json_traverse: '//&
                                                  'Malformed JSON linked list')
                        RETURN
                    END IF
                    CALL TRAVERSE(ELEMENT)
                    IF (FINISHED .OR. JSON%EXCEPTION_THROWN) EXIT
                    ELEMENT => ELEMENT%NEXT
                END DO
            END IF
            NULLIFY(ELEMENT)

        END IF

        END SUBROUTINE TRAVERSE

    END SUBROUTINE JSON_TRAVERSE
!*****************************************************************************************

!*****************************************************************************************
!>
!  This routine calls the user-supplied array_callback subroutine
!  for each element in the array (specified by the path).

    RECURSIVE SUBROUTINE JSON_GET_ARRAY_BY_PATH(JSON, ME, PATH, ARRAY_CALLBACK, FOUND)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN) :: ME
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: PATH
    PROCEDURE(JSON_ARRAY_CALLBACK_FUNC) :: ARRAY_CALLBACK
    LOGICAL(LK),INTENT(OUT),OPTIONAL    :: FOUND

    TYPE(JSON_VALUE),POINTER :: P

    IF ( JSON%EXCEPTION_THROWN ) THEN
        IF ( PRESENT(FOUND) ) FOUND = .FALSE.
        RETURN
    END IF

    NULLIFY(P)

! resolve the path to the value
    CALL JSON%GET(ME=ME, PATH=PATH, P=P)

    IF (.NOT. ASSOCIATED(P)) THEN
        CALL JSON%THROW_EXCEPTION('Error in json_get_array:'//&
                                  ' Unable to resolve path: '//TRIM(PATH),FOUND)
    ELSE
       CALL JSON%GET(ME=P,ARRAY_CALLBACK=ARRAY_CALLBACK)
       NULLIFY(P)
    END IF
    IF ( JSON%EXCEPTION_THROWN ) THEN
        IF ( PRESENT(FOUND) ) THEN
            FOUND = .FALSE.
            CALL JSON%CLEAR_EXCEPTIONS()
        END IF
    ELSE
        IF ( PRESENT(FOUND) ) FOUND = .TRUE.
    END IF

    END SUBROUTINE JSON_GET_ARRAY_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_get_array_by_path]], where "path" is kind=CDK

    RECURSIVE SUBROUTINE WRAP_JSON_GET_ARRAY_BY_PATH(JSON, ME, PATH, ARRAY_CALLBACK, FOUND)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER,INTENT(IN)  :: ME
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: PATH
    PROCEDURE(JSON_ARRAY_CALLBACK_FUNC)  :: ARRAY_CALLBACK
    LOGICAL(LK),INTENT(OUT),OPTIONAL     :: FOUND

    CALL JSON%GET(ME, TO_UNICODE(PATH), ARRAY_CALLBACK, FOUND)

    END SUBROUTINE WRAP_JSON_GET_ARRAY_BY_PATH
!*****************************************************************************************

!*****************************************************************************************
!>
!  Internal routine to be called before parsing JSON.
!  Currently, all this does it allocate the `comment_char` if none was specified.

    SUBROUTINE JSON_PREPARE_PARSER(JSON)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT) :: JSON

    IF (JSON%ALLOW_COMMENTS .AND. .NOT. ALLOCATED(JSON%COMMENT_CHAR)) THEN
! comments are enabled, but user hasn't set the comment char,
! so in this case use the default:
        JSON%COMMENT_CHAR = CK_'/!#'
    END IF

    END SUBROUTINE JSON_PREPARE_PARSER
!*****************************************************************************************

!*****************************************************************************************
!>
!  Parse the JSON file and populate the [[json_value]] tree.
!
!--- Inputs
!
!  The inputs can be:
!
!  * `file` & `unit` : the specified unit is used to read JSON from file.
!                      [note if unit is already open, then the filename is ignored]
!  * `file`          : JSON is read from file using internal unit number
!
!--- Example
!
!````fortran
!    type(json_core) :: json
!    type(json_value),pointer :: p
!    call json%load(file='myfile.json', p=p)
!````
!
!--- History
!  * Jacob Williams : 01/13/2015 : added read from string option.
!  * Izaak Beekman  : 03/08/2015 : moved read from string to separate
!    subroutine, and error annotation to separate subroutine.
!
!@note When calling this routine, any exceptions thrown from previous
!      calls will automatically be cleared.

    SUBROUTINE JSON_PARSE_FILE(JSON, FILE, P, UNIT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: FILE  !! JSON file name
    TYPE(JSON_VALUE),POINTER             :: P     !! output structure
    INTEGER(IK),INTENT(IN),OPTIONAL      :: UNIT  !! file unit number (/= 0)

    INTEGER(IK) :: IUNIT   !! file unit actually used
    INTEGER(IK) :: ISTAT   !! iostat flag
    LOGICAL(LK) :: IS_OPEN !! if the file is already open
    LOGICAL(LK) :: HAS_DUPLICATE  !! if checking for duplicate keys
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: PATH !! path to any duplicate key

! clear any exceptions and initialize:
    CALL JSON%INITIALIZE()
    CALL JSON%PREPARE_PARSER()

    IF ( PRESENT(UNIT) ) THEN

        IF (UNIT==0) THEN
            CALL JSON%THROW_EXCEPTION('Error in json_parse_file: unit number must not be 0.')
            RETURN
        END IF

        IUNIT = UNIT

! check to see if the file is already open
! if it is, then use it, otherwise open the file with the name given.
        INQUIRE(UNIT=IUNIT, OPENED=IS_OPEN, IOSTAT=ISTAT)
        IF (ISTAT==0 .AND. .NOT. IS_OPEN) THEN
! open the file
            OPEN (  UNIT        = IUNIT, &
                    FILE        = FILE, &
                    STATUS      = 'OLD', &
                    ACTION      = 'READ', &
                    FORM        = FORM_SPEC, &
                    ACCESS      = ACCESS_SPEC, &
                    IOSTAT      = ISTAT &
                     )
        ELSE
! if the file is already open, then we need to make sure
! that it is open with the correct form/access/etc...
        END IF

    ELSE

! open the file with a new unit number:
        OPEN (  NEWUNIT     = IUNIT, &
                FILE        = FILE, &
                STATUS      = 'OLD', &
                ACTION      = 'READ', &
                FORM        = FORM_SPEC, &
                ACCESS      = ACCESS_SPEC, &
                IOSTAT      = ISTAT &
                 )

    END IF

    IF (ISTAT==0) THEN

        IF (USE_UNFORMATTED_STREAM) THEN
! save the file size to be read:
            INQUIRE(UNIT=IUNIT, SIZE=JSON%FILESIZE, IOSTAT=ISTAT)
        END IF

! create the value and associate the pointer
        CALL JSON_VALUE_CREATE(P)

! Note: the name of the root json_value doesn't really matter,
!  but we'll allocate something here just in case.
        P%NAME = TRIM(FILE)  !use the file name

! parse as a value
        CALL JSON%PARSE_VALUE(UNIT=IUNIT, STR=CK_'', VALUE=P)
        CALL JSON%PARSE_END(UNIT=IUNIT, STR=CK_'')

! check for errors:
        IF (JSON%EXCEPTION_THROWN) THEN
            CALL JSON%ANNOTATE_INVALID_JSON(IUNIT,CK_'')
        ELSE
            IF (.NOT. JSON%ALLOW_DUPLICATE_KEYS) THEN
                CALL JSON%CHECK_FOR_DUPLICATE_KEYS(P,HAS_DUPLICATE,PATH=PATH)
                IF (.NOT. JSON%EXCEPTION_THROWN) THEN
                    IF (HAS_DUPLICATE) THEN
                        CALL JSON%THROW_EXCEPTION('Error in json_parse_file: '//&
                                                  'Duplicate key found: '//PATH)
                    END IF
                END IF
            END IF
        END IF

! close the file:
        CLOSE(UNIT=IUNIT, IOSTAT=ISTAT)

    ELSE

        CALL JSON%THROW_EXCEPTION('Error in json_parse_file: Error opening file: '//TRIM(FILE))
        NULLIFY(P)

    END IF

    END SUBROUTINE JSON_PARSE_FILE
!*****************************************************************************************

!*****************************************************************************************
!>
!  Parse the JSON string and populate the [[json_value]] tree.
!
!--- See also
!  * [[json_parse_file]]

    SUBROUTINE JSON_PARSE_STRING(JSON, P, STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: P     !! output structure
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: STR   !! string with JSON data

    INTEGER(IK),PARAMETER :: IUNIT = 0 !! indicates that json data will be read from buffer

    LOGICAL(LK) :: HAS_DUPLICATE  !! if checking for duplicate keys
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: PATH !! path to any duplicate key

! clear any exceptions and initialize:
    CALL JSON%INITIALIZE()
    CALL JSON%PREPARE_PARSER()

! create the value and associate the pointer
    CALL JSON_VALUE_CREATE(P)

! Note: the name of the root json_value doesn't really matter,
!  but we'll allocate something here just in case.
    P%NAME = CK_''

! parse as a value
    CALL JSON%PARSE_VALUE(UNIT=IUNIT, STR=STR, VALUE=P)
    CALL JSON%PARSE_END(UNIT=IUNIT, STR=STR)

    IF (JSON%EXCEPTION_THROWN) THEN
        CALL JSON%ANNOTATE_INVALID_JSON(IUNIT,STR)
    ELSE
        IF (.NOT. JSON%ALLOW_DUPLICATE_KEYS) THEN
            CALL JSON%CHECK_FOR_DUPLICATE_KEYS(P,HAS_DUPLICATE,PATH=PATH)
            IF (.NOT. JSON%EXCEPTION_THROWN) THEN
                IF (HAS_DUPLICATE) THEN
                    CALL JSON%THROW_EXCEPTION('Error in json_parse_string: '//&
                                              'Duplicate key found: '//PATH)
                END IF
            END IF
        END IF
    END IF

    END SUBROUTINE JSON_PARSE_STRING
!*****************************************************************************************

!*****************************************************************************************
!>
!  An error checking routine to call after a file (or string) has been parsed.
!  It will throw an exception if there are any other non-whitespace characters
!  in the file.

    SUBROUTINE JSON_PARSE_END(JSON, UNIT, STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    INTEGER(IK),INTENT(IN)              :: UNIT   !! file unit number
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: STR    !! string containing JSON
!! data (only used if `unit=0`)

    LOGICAL(LK)              :: EOF !! end-of-file flag
    CHARACTER(KIND=CK,LEN=1) :: C   !! character read from file
!! (or string) by [[pop_char]]

! first check for exceptions:
    IF (JSON%EXCEPTION_THROWN) RETURN

! pop the next non whitespace character off the file
    CALL JSON%POP_CHAR(UNIT, STR=STR, EOF=EOF, SKIP_WS=.TRUE., &
                        SKIP_COMMENTS=JSON%ALLOW_COMMENTS, POPPED=C)

    IF (.NOT. EOF) THEN
        CALL JSON%THROW_EXCEPTION('Error in json_parse_end:'//&
                                  ' Unexpected character found after parsing value. "'//&
                                  C//'"')
    END IF

    END SUBROUTINE JSON_PARSE_END
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_parse_string]], where `str` is kind=CDK.

    SUBROUTINE WRAP_JSON_PARSE_STRING(JSON, P, STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P     !! output structure
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: STR   !! string with JSON data

    CALL JSON%DESERIALIZE(P,TO_UNICODE(STR))

    END SUBROUTINE WRAP_JSON_PARSE_STRING
!*****************************************************************************************

!*****************************************************************************************
!>
!  Generate a warning message if there was an error parsing a JSON
!  file or string.

    SUBROUTINE ANNOTATE_INVALID_JSON(JSON,IUNIT,STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    INTEGER(IK),INTENT(IN)              :: IUNIT !! file unit number
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: STR   !! string with JSON data

    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: LINE      !! line containing the error
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: ARROW_STR !! arrow string that points
!! to the current character
    CHARACTER(KIND=CK,LEN=MAX_INTEGER_STR_LEN) :: LINE_STR !! current line number string
    CHARACTER(KIND=CK,LEN=MAX_INTEGER_STR_LEN) :: CHAR_STR !! current character count string
    INTEGER(IK) :: I          !! line number counter
    INTEGER(IK) :: I_NL_PREV  !! index of previous newline character
    INTEGER(IK) :: I_NL       !! index of current newline character

! If there was an error reading the file, then
! print the line where the error occurred:
    IF (JSON%EXCEPTION_THROWN) THEN

!the counters for the current line and the last character read:
        CALL INTEGER_TO_STRING(JSON%LINE_COUNT, INT_FMT, LINE_STR)
        CALL INTEGER_TO_STRING(JSON%CHAR_COUNT, INT_FMT, CHAR_STR)

!draw the arrow string that points to the current character:
        ARROW_STR = REPEAT('-',MAX( 0_IK, JSON%CHAR_COUNT - 1_IK) )//'^'

        IF (JSON%LINE_COUNT>0 .AND. JSON%CHAR_COUNT>0) THEN

            IF (IUNIT/=0) THEN

                IF (USE_UNFORMATTED_STREAM) THEN
                    CALL JSON%GET_CURRENT_LINE_FROM_FILE_STREAM(IUNIT,LINE)
                ELSE
                    CALL JSON%GET_CURRENT_LINE_FROM_FILE_SEQUENTIAL(IUNIT,LINE)
                END IF

            ELSE

!get the current line from the string:
! [this is done by counting the newline characters]
                I_NL_PREV = 0  !index of previous newline character
                I_NL = 2  !just in case line_count = 0
                DO I=1,JSON%LINE_COUNT
                    I_NL = INDEX(STR(I_NL_PREV+1:),NEWLINE)
                    IF (I_NL==0) THEN   !last line - no newline character
                        I_NL = LEN(STR)+1
                        EXIT
                    END IF
                    I_NL = I_NL + I_NL_PREV   !index of current newline character
                    I_NL_PREV = I_NL          !update for next iteration
                END DO
                LINE = STR(I_NL_PREV+1 : I_NL-1)  !extract current line

            END IF

        ELSE
!in this case, it was an empty line or file
            LINE = CK_''
        END IF

! add a newline for the error display if necessary:
        LINE = TRIM(LINE)
        IF (LEN(LINE)>0) THEN
            I = LEN(LINE)
            IF (LINE(I:I)/=NEWLINE) LINE = LINE//NEWLINE
        ELSE
            LINE = LINE//NEWLINE
        END IF

!create the error message:
        IF (ALLOCATED(JSON%ERR_MESSAGE)) THEN
            JSON%ERR_MESSAGE = JSON%ERR_MESSAGE//NEWLINE
        ELSE
            JSON%ERR_MESSAGE = ''
        END IF
        JSON%ERR_MESSAGE = JSON%ERR_MESSAGE//&
                           'line: '//TRIM(ADJUSTL(LINE_STR))//', '//&
                           'character: '//TRIM(ADJUSTL(CHAR_STR))//NEWLINE//&
                           LINE//ARROW_STR

        IF (ALLOCATED(LINE)) DEALLOCATE(LINE)

    END IF

    END SUBROUTINE ANNOTATE_INVALID_JSON
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Rewind the file to the beginning of the current line, and return this line.
!  The file is assumed to be opened.
!  This is the SEQUENTIAL version (see also [[get_current_line_from_file_stream]]).

    SUBROUTINE GET_CURRENT_LINE_FROM_FILE_SEQUENTIAL(IUNIT,LINE)

    IMPLICIT NONE

    INTEGER(IK),INTENT(IN)                           :: IUNIT  !! file unit number
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT) :: LINE   !! current line

    CHARACTER(KIND=CK,LEN=SEQ_CHUNK_SIZE) :: CHUNK !! for reading line in chunks
    INTEGER(IK) :: ISTAT  !! iostat flag
    INTEGER(IK) :: ISIZE  !! number of characters read in read statement

!initialize:
    LINE = CK_''

!rewind to beginning of the current record:
    BACKSPACE(IUNIT, IOSTAT=ISTAT)

!loop to read in all the characters in the current record.
![the line is read in chunks until the end of the line is reached]
    IF (ISTAT==0) THEN
        DO
            ISIZE = 0
            READ(IUNIT,FMT='(A)',ADVANCE='NO',SIZE=ISIZE,IOSTAT=ISTAT) CHUNK
            IF (ISTAT==0) THEN
                LINE = LINE//CHUNK
            ELSE
                IF (ISIZE>0 .AND. ISIZE<=SEQ_CHUNK_SIZE) LINE = LINE//CHUNK(1:ISIZE)
                EXIT
            END IF
        END DO
    END IF

    END SUBROUTINE GET_CURRENT_LINE_FROM_FILE_SEQUENTIAL
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Rewind the file to the beginning of the current line, and return this line.
!  The file is assumed to be opened.
!  This is the STREAM version (see also [[get_current_line_from_file_sequential]]).

    SUBROUTINE GET_CURRENT_LINE_FROM_FILE_STREAM(JSON,IUNIT,LINE)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                   :: JSON
    INTEGER(IK),INTENT(IN)                           :: IUNIT  !! file unit number
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT) :: LINE   !! current line

    INTEGER(IK)              :: ISTART  !! start position of current line
    INTEGER(IK)              :: IEND    !! end position of current line
    INTEGER(IK)              :: IOS     !! file read `iostat` code
    CHARACTER(KIND=CK,LEN=1) :: C       !! a character read from the file
    LOGICAL                  :: DONE    !! flag to exit the loop

    ISTART = JSON%IPOS
    DO
        IF (ISTART<=1) THEN
            ISTART = 1
            EXIT
        END IF
        READ(IUNIT,POS=ISTART,IOSTAT=IOS) C
        DONE = IOS /= 0_IK
        IF (.NOT. DONE) DONE = C==NEWLINE
        IF (DONE) THEN
            IF (ISTART/=1) ISTART = ISTART - 1
            EXIT
        END IF
        ISTART = ISTART-1  !rewind until the beginning of the line
    END DO
    IEND = JSON%IPOS
    DO
        READ(IUNIT,POS=IEND,IOSTAT=IOS) C
        IF (IS_IOSTAT_END(IOS)) THEN
! account for end of file without linebreak
            IEND=IEND-1
            EXIT
        END IF
        IF (C==NEWLINE .OR. IOS/=0) EXIT
        IEND=IEND+1
    END DO
    ALLOCATE( CHARACTER(KIND=CK,LEN=IEND-ISTART+1) :: LINE )
    READ(IUNIT,POS=ISTART,IOSTAT=IOS) LINE

    END SUBROUTINE GET_CURRENT_LINE_FROM_FILE_STREAM
!*****************************************************************************************

!*****************************************************************************************
!>
!  Core parsing routine.

    RECURSIVE SUBROUTINE PARSE_VALUE(JSON, UNIT, STR, VALUE)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    INTEGER(IK),INTENT(IN)              :: UNIT   !! file unit number
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: STR    !! string containing JSON
!! data (only used if `unit=0`)
    TYPE(JSON_VALUE),POINTER            :: VALUE  !! JSON data that is extracted

    LOGICAL(LK)              :: EOF !! end-of-file flag
    CHARACTER(KIND=CK,LEN=1) :: C   !! character read from file
!! (or string) by [[pop_char]]
!- 10133


    IF (.NOT. JSON%EXCEPTION_THROWN) THEN

!the routine is being called incorrectly.
        IF (.NOT. ASSOCIATED(VALUE)) THEN
            CALL JSON%THROW_EXCEPTION('Error in parse_value: value pointer not associated.')
            RETURN
        END IF

! pop the next non whitespace character off the file
        CALL JSON%POP_CHAR(UNIT, STR=STR, EOF=EOF, SKIP_WS=.TRUE., &
                           SKIP_COMMENTS=JSON%ALLOW_COMMENTS, POPPED=C)

        IF (EOF) THEN
            RETURN
        ELSE

            SELECT CASE (C)

            CASE (START_OBJECT)

! start object
                CALL JSON%TO_OBJECT(VALUE)    !allocate class
                CALL JSON%PARSE_OBJECT(UNIT, STR, VALUE)

            CASE (START_ARRAY)

! start array
                CALL JSON%TO_ARRAY(VALUE)    !allocate class
                CALL JSON%PARSE_ARRAY(UNIT, STR, VALUE)

            CASE (END_ARRAY)

! end an empty array
                CALL JSON%PUSH_CHAR(C)
                IF (ASSOCIATED(VALUE)) THEN
                    DEALLOCATE(VALUE)
                    NULLIFY(VALUE)
                END IF

            CASE (QUOTATION_MARK)

! string
                CALL JSON%TO_STRING(VALUE)    !allocate class

                SELECT CASE (VALUE%VAR_TYPE)
                CASE (JSON_STRING)
!- 10187

                    CALL JSON%PARSE_STRING(UNIT,STR,VALUE%STR_VALUE)

                END SELECT

            CASE (CK_'t') !true_str(1:1) gfortran bug work around

!true
                CALL JSON%PARSE_FOR_CHARS(UNIT, STR, TRUE_STR(2:))
!allocate class and set value:
                IF (.NOT. JSON%EXCEPTION_THROWN) CALL JSON%TO_LOGICAL(VALUE,.TRUE.)

            CASE (CK_'f') !false_str(1:1) gfortran bug work around

!false
                CALL JSON%PARSE_FOR_CHARS(UNIT, STR, FALSE_STR(2:))
!allocate class and set value:
                IF (.NOT. JSON%EXCEPTION_THROWN) CALL JSON%TO_LOGICAL(VALUE,.FALSE.)

            CASE (CK_'n') !null_str(1:1) gfortran bug work around

!null
                CALL JSON%PARSE_FOR_CHARS(UNIT, STR, NULL_STR(2:))
                IF (.NOT. JSON%EXCEPTION_THROWN) CALL JSON%TO_NULL(VALUE) ! allocate class

            CASE(CK_'-', CK_'0': CK_'9', CK_'.', CK_'+')

                CALL JSON%PUSH_CHAR(C)
                CALL JSON%PARSE_NUMBER(UNIT, STR, VALUE)

            CASE DEFAULT

                CALL JSON%THROW_EXCEPTION('Error in parse_value:'//&
                                          ' Unexpected character while parsing value. "'//&
                                          C//'"')

            END SELECT
        END IF

    END IF

    END SUBROUTINE PARSE_VALUE
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Allocate a [[json_value]] pointer and make it a logical(LK) variable.
!  The pointer should not already be allocated.
!
!--- Example
!````fortran
!     type(json_value),pointer :: p
!     type(json_core) :: json
!     call json%create_logical(p,'value',.true.)
!````

    SUBROUTINE JSON_VALUE_CREATE_LOGICAL(JSON,P,VAL,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: P
    LOGICAL(LK),INTENT(IN)              :: VAL   !! variable value
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: NAME  !! variable name

    CALL JSON_VALUE_CREATE(P)
    CALL JSON%TO_LOGICAL(P,VAL,NAME)

    END SUBROUTINE JSON_VALUE_CREATE_LOGICAL
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  Wrapper for [[json_value_create_logical]] so `create_logical` method can
!  be called with name of character kind 'DEFAULT' or 'ISO_10646'

    SUBROUTINE WRAP_JSON_VALUE_CREATE_LOGICAL(JSON,P,VAL,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    LOGICAL(LK),INTENT(IN)               :: VAL
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: NAME

    CALL JSON%CREATE_LOGICAL(P,VAL,TO_UNICODE(NAME))

    END SUBROUTINE WRAP_JSON_VALUE_CREATE_LOGICAL
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Allocate a [[json_value]] pointer and make it an integer(IK) variable.
!  The pointer should not already be allocated.
!
!--- Example
!````fortran
!     type(json_value),pointer :: p
!     type(json_core) :: json
!     call json%create_integer(p,'value',1)
!````

    SUBROUTINE JSON_VALUE_CREATE_INTEGER(JSON,P,VAL,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: P
    INTEGER(IK),INTENT(IN)              :: VAL
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: NAME

    CALL JSON_VALUE_CREATE(P)
    CALL JSON%TO_INTEGER(P,VAL,NAME)

    END SUBROUTINE JSON_VALUE_CREATE_INTEGER
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  A wrapper procedure for [[json_value_create_integer]] so that `create_integer`
!  method may be called with either a 'DEFAULT' or 'ISO_10646' character kind
!  `name` actual argument.

    SUBROUTINE WRAP_JSON_VALUE_CREATE_INTEGER(JSON,P,VAL,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    INTEGER(IK),INTENT(IN)               :: VAL
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: NAME

    CALL JSON%CREATE_INTEGER(P,VAL,TO_UNICODE(NAME))

    END SUBROUTINE WRAP_JSON_VALUE_CREATE_INTEGER
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Allocate a [[json_value]] pointer and make it a real(RK) variable.
!  The pointer should not already be allocated.
!
!--- Example
!````fortran
!     type(json_value),pointer :: p
!     type(json_core) :: json
!     call json%create_real(p,'value',1.0_RK)
!````

    SUBROUTINE JSON_VALUE_CREATE_REAL(JSON,P,VAL,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: P
    REAL(RK),INTENT(IN)                 :: VAL
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: NAME

    CALL JSON_VALUE_CREATE(P)
    CALL JSON%TO_REAL(P,VAL,NAME)

    END SUBROUTINE JSON_VALUE_CREATE_REAL
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  A wrapper for [[json_value_create_real]] so that `create_real` method
!  may be called with an actual argument corresponding to the dummy argument,
!  `name` that may be of 'DEFAULT' or 'ISO_10646' character kind.

    SUBROUTINE WRAP_JSON_VALUE_CREATE_REAL(JSON,P,VAL,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    REAL(RK),INTENT(IN)                  :: VAL
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: NAME

    CALL JSON%CREATE_REAL(P,VAL,TO_UNICODE(NAME))

    END SUBROUTINE WRAP_JSON_VALUE_CREATE_REAL
!*****************************************************************************************


!*****************************************************************************************
!>
!  Alternate version of [[json_value_create_real]] where val=real32.
!
!@note The value is converted into a `real(RK)` variable internally.

    SUBROUTINE JSON_VALUE_CREATE_REAL32(JSON,P,VAL,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: P
    REAL(REAL32),INTENT(IN)             :: VAL
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: NAME

    CALL JSON%CREATE_REAL(P,REAL(VAL,RK),NAME)

    END SUBROUTINE JSON_VALUE_CREATE_REAL32
!*****************************************************************************************

!*****************************************************************************************
!>
!  Alternate version of [[json_value_create_real32]] where "name" is kind(CDK).

    SUBROUTINE WRAP_JSON_VALUE_CREATE_REAL32(JSON,P,VAL,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    REAL(REAL32),INTENT(IN)              :: VAL
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: NAME

    CALL JSON%CREATE_REAL(P,VAL,TO_UNICODE(NAME))

    END SUBROUTINE WRAP_JSON_VALUE_CREATE_REAL32
!*****************************************************************************************


!- 10455


!*****************************************************************************************
!> author: Jacob Williams
!
!  Allocate a json_value pointer and make it a string variable.
!  The pointer should not already be allocated.
!
!--- Example
!````fortran
!     type(json_value),pointer :: p
!     type(json_core) :: json
!     call json%create_string(p,'value','hello')
!````

    SUBROUTINE JSON_VALUE_CREATE_STRING(JSON,P,VAL,NAME,TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: VAL
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: NAME
    LOGICAL(LK),INTENT(IN),OPTIONAL     :: TRIM_STR      !! if TRIM() should be called for the `val`
    LOGICAL(LK),INTENT(IN),OPTIONAL     :: ADJUSTL_STR   !! if ADJUSTL() should be called for the `val`

    CALL JSON_VALUE_CREATE(P)
    CALL JSON%TO_STRING(P,VAL,NAME,TRIM_STR,ADJUSTL_STR)

    END SUBROUTINE JSON_VALUE_CREATE_STRING
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  Wrap [[json_value_create_string]] so that `create_string` method may be called
!  with actual character string arguments for `name` and `val` that are BOTH of
!  'DEFAULT' or 'ISO_10646' character kind.

    SUBROUTINE WRAP_JSON_VALUE_CREATE_STRING(JSON,P,VAL,NAME,TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: VAL
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: NAME
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: TRIM_STR      !! if TRIM() should be called for the `val`
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: ADJUSTL_STR   !! if ADJUSTL() should be called for the `val`

    CALL JSON%CREATE_STRING(P,TO_UNICODE(VAL),TO_UNICODE(NAME),TRIM_STR,ADJUSTL_STR)

    END SUBROUTINE WRAP_JSON_VALUE_CREATE_STRING
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Allocate a json_value pointer and make it a null variable.
!  The pointer should not already be allocated.
!
!--- Example
!````fortran
!     type(json_value),pointer :: p
!     type(json_core) :: json
!     call json%create_null(p,'value')
!````

    SUBROUTINE JSON_VALUE_CREATE_NULL(JSON,P,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: NAME

    CALL JSON_VALUE_CREATE(P)
    CALL JSON%TO_NULL(P,NAME)

    END SUBROUTINE JSON_VALUE_CREATE_NULL
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  Wrap [[json_value_create_null]] so that `create_null` method may be called with
!  an actual argument corresponding to the dummy argument `name` that is either
!  of 'DEFAULT' or 'ISO_10646' character kind.

    SUBROUTINE WRAP_JSON_VALUE_CREATE_NULL(JSON,P,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: NAME

    CALL JSON%CREATE_NULL(P,TO_UNICODE(NAME))

    END SUBROUTINE WRAP_JSON_VALUE_CREATE_NULL
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Allocate a [[json_value]] pointer and make it an object variable.
!  The pointer should not already be allocated.
!
!--- Example
!````fortran
!     type(json_value),pointer :: p
!     type(json_core) :: json
!     call json%create_object(p,'objectname')
!````
!
!@note The name is not significant for the root structure or an array element.
!      In those cases, an empty string can be used.

    SUBROUTINE JSON_VALUE_CREATE_OBJECT(JSON,P,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: NAME

    CALL JSON_VALUE_CREATE(P)
    CALL JSON%TO_OBJECT(P,NAME)

    END SUBROUTINE JSON_VALUE_CREATE_OBJECT
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  Wrap [[json_value_create_object]] so that `create_object` method may be called
!  with an actual argument corresponding to the dummy argument `name` that is of
!  either 'DEFAULT' or 'ISO_10646' character kind.

    SUBROUTINE WRAP_JSON_VALUE_CREATE_OBJECT(JSON,P,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: NAME

    CALL JSON%CREATE_OBJECT(P,TO_UNICODE(NAME))

    END SUBROUTINE WRAP_JSON_VALUE_CREATE_OBJECT
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Allocate a [[json_value]] pointer and make it an array variable.
!  The pointer should not already be allocated.
!
!--- Example
!````fortran
!     type(json_value),pointer :: p
!     type(json_core) :: json
!     call json%create_array(p,'arrayname')
!````

    SUBROUTINE JSON_VALUE_CREATE_ARRAY(JSON,P,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    TYPE(JSON_VALUE),POINTER            :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: NAME

    CALL JSON_VALUE_CREATE(P)
    CALL JSON%TO_ARRAY(P,NAME)

    END SUBROUTINE JSON_VALUE_CREATE_ARRAY
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  A wrapper for [[json_value_create_array]] so that `create_array` method may be
!  called with an actual argument, corresponding to the dummy argument `name`,
!  that is either of 'DEFAULT' or 'ISO_10646' character kind.

    SUBROUTINE WRAP_JSON_VALUE_CREATE_ARRAY(JSON,P,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    TYPE(JSON_VALUE),POINTER             :: P
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: NAME

    CALL JSON%CREATE_ARRAY(P,TO_UNICODE(NAME))

    END SUBROUTINE WRAP_JSON_VALUE_CREATE_ARRAY
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Change the [[json_value]] variable to a logical.

    SUBROUTINE TO_LOGICAL(JSON,P,VAL,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)               :: JSON
    TYPE(JSON_VALUE),POINTER                     :: P
    LOGICAL(LK),INTENT(IN),OPTIONAL              :: VAL   !! if the value is also to be set
!! (if not present, then .false. is used).
    CHARACTER(KIND=CK,LEN=*),INTENT(IN),OPTIONAL :: NAME  !! if the name is also to be changed.

!set type and value:
    CALL DESTROY_JSON_DATA(P)
    P%VAR_TYPE = JSON_LOGICAL
    ALLOCATE(P%LOG_VALUE)
    IF (PRESENT(VAL)) THEN
        P%LOG_VALUE = VAL
    ELSE
        P%LOG_VALUE = .FALSE.    !default value
    END IF

!name:
    IF (PRESENT(NAME)) CALL JSON%RENAME(P,NAME)

    END SUBROUTINE TO_LOGICAL
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Change the [[json_value]] variable to an integer.

    SUBROUTINE TO_INTEGER(JSON,P,VAL,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)               :: JSON
    TYPE(JSON_VALUE),POINTER                     :: P
    INTEGER(IK),INTENT(IN),OPTIONAL              :: VAL   !! if the value is also to be set
!! (if not present, then 0 is used).
    CHARACTER(KIND=CK,LEN=*),INTENT(IN),OPTIONAL :: NAME  !! if the name is also to be changed.

!set type and value:
    CALL DESTROY_JSON_DATA(P)
    P%VAR_TYPE = JSON_INTEGER
    ALLOCATE(P%INT_VALUE)
    IF (PRESENT(VAL)) THEN
        P%INT_VALUE = VAL
    ELSE
        P%INT_VALUE = 0_IK    !default value
    END IF

!name:
    IF (PRESENT(NAME)) CALL JSON%RENAME(P,NAME)

    END SUBROUTINE TO_INTEGER
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Change the [[json_value]] variable to a real.

    SUBROUTINE TO_REAL(JSON,P,VAL,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)               :: JSON
    TYPE(JSON_VALUE),POINTER                     :: P
    REAL(RK),INTENT(IN),OPTIONAL                 :: VAL   !! if the value is also to be set
!! (if not present, then 0.0_rk is used).
    CHARACTER(KIND=CK,LEN=*),INTENT(IN),OPTIONAL :: NAME  !! if the name is also to be changed.

!set type and value:
    CALL DESTROY_JSON_DATA(P)
    P%VAR_TYPE = JSON_REAL
    ALLOCATE(P%DBL_VALUE)
    IF (PRESENT(VAL)) THEN
        P%DBL_VALUE = VAL
    ELSE
        P%DBL_VALUE = 0.0_RK    !default value
    END IF

!name:
    IF (PRESENT(NAME)) CALL JSON%RENAME(P,NAME)

    END SUBROUTINE TO_REAL
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Change the [[json_value]] variable to a string.
!
!--- Modified
!  * Izaak Beekman : 02/24/2015

    SUBROUTINE TO_STRING(JSON,P,VAL,NAME,TRIM_STR,ADJUSTL_STR)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)  :: JSON
    TYPE(JSON_VALUE),POINTER        :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN),OPTIONAL :: VAL   !! if the value is also to be set
!! (if not present, then '' is used).
    CHARACTER(KIND=CK,LEN=*),INTENT(IN),OPTIONAL :: NAME  !! if the name is also to be changed.
    LOGICAL(LK),INTENT(IN),OPTIONAL     :: TRIM_STR       !! if TRIM() should be called for the `val`
!! (only used if `val` is present)
    LOGICAL(LK),INTENT(IN),OPTIONAL     :: ADJUSTL_STR    !! if ADJUSTL() should be called for the `val`
!! (only used if `val` is present)
!! (note that ADJUSTL is done before TRIM)

    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: STR !! temp string for `trim()` and/or `adjustl()`
    LOGICAL :: TRIM_STRING    !! if the string is to be trimmed
    LOGICAL :: ADJUSTL_STRING !! if the string is to be adjusted left

!set type and value:
    CALL DESTROY_JSON_DATA(P)
    P%VAR_TYPE = JSON_STRING
    IF (PRESENT(VAL)) THEN

        IF (PRESENT(TRIM_STR)) THEN
            TRIM_STRING = TRIM_STR
        ELSE
            TRIM_STRING = .FALSE.
        END IF
        IF (PRESENT(ADJUSTL_STR)) THEN
            ADJUSTL_STRING = ADJUSTL_STR
        ELSE
            ADJUSTL_STRING = .FALSE.
        END IF

        IF (TRIM_STRING .OR. ADJUSTL_STRING) THEN
            STR = VAL
            IF (ADJUSTL_STRING) STR = ADJUSTL(STR)
            IF (TRIM_STRING)    STR = TRIM(STR)
            P%STR_VALUE = STR
        ELSE
            P%STR_VALUE = VAL
        END IF

    ELSE
        P%STR_VALUE = CK_''  ! default value
    END IF

!name:
    IF (PRESENT(NAME)) CALL JSON%RENAME(P,NAME)

    END SUBROUTINE TO_STRING
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Change the [[json_value]] variable to a null.

    SUBROUTINE TO_NULL(JSON,P,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)               :: JSON
    TYPE(JSON_VALUE),POINTER                     :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN),OPTIONAL :: NAME  !! if the name is also to be changed.

!set type and value:
    CALL DESTROY_JSON_DATA(P)
    P%VAR_TYPE = JSON_NULL

!name:
    IF (PRESENT(NAME)) CALL JSON%RENAME(P,NAME)

    END SUBROUTINE TO_NULL
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Change the [[json_value]] variable to an object.

    SUBROUTINE TO_OBJECT(JSON,P,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)               :: JSON
    TYPE(JSON_VALUE),POINTER                     :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN),OPTIONAL :: NAME  !! if the name is also to be changed.

!set type and value:
    CALL DESTROY_JSON_DATA(P)
    P%VAR_TYPE = JSON_OBJECT

!name:
    IF (PRESENT(NAME)) CALL JSON%RENAME(P,NAME)

    END SUBROUTINE TO_OBJECT
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Change the [[json_value]] variable to an array.

    SUBROUTINE TO_ARRAY(JSON,P,NAME)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)               :: JSON
    TYPE(JSON_VALUE),POINTER                     :: P
    CHARACTER(KIND=CK,LEN=*),INTENT(IN),OPTIONAL :: NAME  !! if the name is also to be changed.

!set type and value:
    CALL DESTROY_JSON_DATA(P)
    P%VAR_TYPE = JSON_ARRAY

!name:
    IF (PRESENT(NAME)) CALL JSON%RENAME(P,NAME)

    END SUBROUTINE TO_ARRAY
!*****************************************************************************************

!*****************************************************************************************
!>
!  Core parsing routine.

    RECURSIVE SUBROUTINE PARSE_OBJECT(JSON, UNIT, STR, PARENT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    INTEGER(IK),INTENT(IN)              :: UNIT    !! file unit number (if parsing from a file)
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: STR     !! JSON string (if parsing from a string)
    TYPE(JSON_VALUE),POINTER            :: PARENT  !! the parsed object will be added as a child of this

    TYPE(JSON_VALUE),POINTER :: PAIR  !! temp variable
    LOGICAL(LK)              :: EOF   !! end of file flag
    CHARACTER(KIND=CK,LEN=1) :: C     !! character returned by [[pop_char]]
!- 10897


    IF (.NOT. JSON%EXCEPTION_THROWN) THEN

!the routine is being called incorrectly.
        IF (.NOT. ASSOCIATED(PARENT)) THEN
            CALL JSON%THROW_EXCEPTION('Error in parse_object: parent pointer not associated.')
        END IF

        NULLIFY(PAIR)    !probably not necessary

! pair name
        CALL JSON%POP_CHAR(UNIT, STR=STR, EOF=EOF, SKIP_WS=.TRUE., &
                            SKIP_COMMENTS=JSON%ALLOW_COMMENTS, POPPED=C)
        IF (EOF) THEN
            CALL JSON%THROW_EXCEPTION('Error in parse_object:'//&
                                      ' Unexpected end of file while parsing start of object.')
            RETURN
        ELSE IF (END_OBJECT == C) THEN
! end of an empty object
            RETURN
        ELSE IF (QUOTATION_MARK == C) THEN
            CALL JSON_VALUE_CREATE(PAIR)
!- 10924

            CALL JSON%PARSE_STRING(UNIT,STR,PAIR%NAME)

            IF (JSON%EXCEPTION_THROWN) THEN
                CALL JSON%DESTROY(PAIR)
                RETURN
            END IF
        ELSE
            CALL JSON%THROW_EXCEPTION('Error in parse_object: Expecting string: "'//C//'"')
            RETURN
        END IF

! pair value
        CALL JSON%POP_CHAR(UNIT, STR=STR, EOF=EOF, SKIP_WS=.TRUE., &
                            SKIP_COMMENTS=JSON%ALLOW_COMMENTS, POPPED=C)
        IF (EOF) THEN
            CALL JSON%DESTROY(PAIR)
            CALL JSON%THROW_EXCEPTION('Error in parse_object:'//&
                                      ' Unexpected end of file while parsing object member.')
            RETURN
        ELSE IF (COLON_CHAR == C) THEN
! parse the value
            CALL JSON%PARSE_VALUE(UNIT, STR, PAIR)
            IF (JSON%EXCEPTION_THROWN) THEN
                CALL JSON%DESTROY(PAIR)
                RETURN
            ELSE
                CALL JSON%ADD(PARENT, PAIR)
            END IF
        ELSE
            CALL JSON%DESTROY(PAIR)
            CALL JSON%THROW_EXCEPTION('Error in parse_object:'//&
                                      ' Expecting : and then a value: '//C)
            RETURN
        END IF

! another possible pair
        CALL JSON%POP_CHAR(UNIT, STR=STR, EOF=EOF, SKIP_WS=.TRUE., &
                            SKIP_COMMENTS=JSON%ALLOW_COMMENTS, POPPED=C)
        IF (EOF) THEN
            CALL JSON%THROW_EXCEPTION('Error in parse_object: '//&
                                      'End of file encountered when parsing an object')
            RETURN
        ELSE IF (DELIMITER == C) THEN
! read the next member
            CALL JSON%PARSE_OBJECT(UNIT = UNIT, STR=STR, PARENT = PARENT)
        ELSE IF (END_OBJECT == C) THEN
! end of object
            RETURN
        ELSE
            CALL JSON%THROW_EXCEPTION('Error in parse_object: Expecting end of object: '//C)
            RETURN
        END IF

    END IF

    END SUBROUTINE PARSE_OBJECT
!*****************************************************************************************

!*****************************************************************************************
!>
!  Core parsing routine.

    RECURSIVE SUBROUTINE PARSE_ARRAY(JSON, UNIT, STR, ARRAY)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    INTEGER(IK),INTENT(IN)              :: UNIT   !! file unit number (if parsing from a file)
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: STR    !! JSON string (if parsing from a string)
    TYPE(JSON_VALUE),POINTER            :: ARRAY

    TYPE(JSON_VALUE),POINTER :: ELEMENT !! temp variable for array element
    LOGICAL(LK)              :: EOF     !! end of file flag
    CHARACTER(KIND=CK,LEN=1) :: C       !! character returned by [[pop_char]]

    DO

        IF (JSON%EXCEPTION_THROWN) EXIT

! try to parse an element value
        NULLIFY(ELEMENT)
        CALL JSON_VALUE_CREATE(ELEMENT)
        CALL JSON%PARSE_VALUE(UNIT, STR, ELEMENT)
        IF (JSON%EXCEPTION_THROWN) THEN
            IF (ASSOCIATED(ELEMENT)) CALL JSON%DESTROY(ELEMENT)
            EXIT
        END IF

! parse value will deallocate an empty array value
        IF (ASSOCIATED(ELEMENT)) CALL JSON%ADD(ARRAY, ELEMENT)

! popped the next character
        CALL JSON%POP_CHAR(UNIT, STR=STR, EOF=EOF, SKIP_WS=.TRUE., &
                           SKIP_COMMENTS=JSON%ALLOW_COMMENTS, POPPED=C)

        IF (EOF) THEN
! The file ended before array was finished:
            CALL JSON%THROW_EXCEPTION('Error in parse_array: '//&
                                      'End of file encountered when parsing an array.')
            EXIT
        ELSE IF (DELIMITER == C) THEN
! parse the next element
            CYCLE
        ELSE IF (END_ARRAY == C) THEN
! end of array
            EXIT
        ELSE
            CALL JSON%THROW_EXCEPTION('Error in parse_array: '//&
                                      'Unexpected character encountered when parsing array.')
            EXIT
        END IF

    END DO

    END SUBROUTINE PARSE_ARRAY
!*****************************************************************************************

!*****************************************************************************************
!>
!  Parses a string while reading a JSON file.
!
!--- History
!  * Jacob Williams : 6/16/2014 : Added hex validation.
!  * Jacob Williams : 12/3/2015 : Fixed some bugs.
!  * Jacob Williams : 8/23/2015 : `string` is now returned unescaped.
!  * Jacob Williams : 7/21/2018 : moved hex validate to [[unescape_string]].

    SUBROUTINE PARSE_STRING(JSON, UNIT, STR, STRING)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)                   :: JSON
    INTEGER(IK),INTENT(IN)                           :: UNIT   !! file unit number (if
!! parsing from a file)
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)              :: STR    !! JSON string (if parsing
!! from a string)
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT) :: STRING !! the string (unescaped
!! if necessary)

    LOGICAL(LK)              :: EOF      !! end of file flag
    LOGICAL(LK)              :: ESCAPE   !! for escape string parsing
    CHARACTER(KIND=CK,LEN=1) :: C        !! character returned by [[pop_char]]
    INTEGER(IK)              :: IP       !! index to put next character,
!! to speed up by reducing the number
!! of character string reallocations.
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: ERROR_MESSAGE !! for string unescaping

!at least return a blank string if there is a problem:
    STRING = BLANK_CHUNK

    IF (.NOT. JSON%EXCEPTION_THROWN) THEN

!initialize:
        ESCAPE = .FALSE.
        IP     = 1

        DO

!get the next character from the file:
            CALL JSON%POP_CHAR(UNIT, STR=STR, EOF=EOF, SKIP_WS=.FALSE., POPPED=C)

            IF (EOF) THEN

                CALL JSON%THROW_EXCEPTION('Error in parse_string: Expecting end of string')
                RETURN

            ELSE IF (C==QUOTATION_MARK .AND. .NOT. ESCAPE) THEN  !end of string

                EXIT

            ELSE

!if the string is not big enough, then add another chunk:
                IF (IP>LEN(STRING)) STRING = STRING // BLANK_CHUNK

!append to string:
                STRING(IP:IP) = C
                IP = IP + 1

! check for escape character, so we don't
! exit prematurely if escaping a quotation
! character:
                IF (ESCAPE) THEN
                    ESCAPE = .FALSE.
                ELSE
                    ESCAPE = (C==BACKSLASH)
                END IF

            END IF

        END DO

!trim the string if necessary:
        IF (IP<LEN(STRING)+1) THEN
            IF (IP==1) THEN
                STRING = CK_''
            ELSE
                STRING = STRING(1:IP-1)
            END IF
        END IF

! string is returned unescaped:
! (this will also validate any hex strings present)
        CALL UNESCAPE_STRING(STRING,ERROR_MESSAGE)
        IF (ALLOCATED(ERROR_MESSAGE)) THEN
            CALL JSON%THROW_EXCEPTION(ERROR_MESSAGE)
            DEALLOCATE(ERROR_MESSAGE)  !cleanup
        END IF

    END IF

    END SUBROUTINE PARSE_STRING
!*****************************************************************************************

!*****************************************************************************************
!>
!  Core parsing routine.
!
!  This is used to verify the strings `true`, `false`, and `null` during parsing.

    SUBROUTINE PARSE_FOR_CHARS(JSON, UNIT, STR, CHARS)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    INTEGER(IK),INTENT(IN)              :: UNIT   !! file unit number (if parsing from a file)
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: STR    !! JSON string (if parsing from a string)
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: CHARS  !! the string to check for.

    INTEGER(IK) :: I               !! counter
    INTEGER(IK) :: LENGTH          !! trimmed length of `chars`
    LOGICAL(LK) :: EOF             !! end of file flag
    CHARACTER(KIND=CK,LEN=1) :: C  !! character returned by [[pop_char]]

    IF (.NOT. JSON%EXCEPTION_THROWN) THEN

        LENGTH = LEN_TRIM(CHARS)

        DO I = 1, LENGTH
            CALL JSON%POP_CHAR(UNIT, STR=STR, EOF=EOF, SKIP_WS=.FALSE., POPPED=C)
            IF (EOF) THEN
                CALL JSON%THROW_EXCEPTION('Error in parse_for_chars:'//&
                                     ' Unexpected end of file while parsing.')
                RETURN
            ELSE IF (C /= CHARS(I:I)) THEN
                CALL JSON%THROW_EXCEPTION('Error in parse_for_chars:'//&
                                     ' Unexpected character: "'//C//'" (expecting "'//&
                                     CHARS(I:I)//'")')
                RETURN
            END IF
        END DO

    END IF

    END SUBROUTINE PARSE_FOR_CHARS
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/20/2014
!
!  Read a numerical value from the file (or string).
!  The routine will determine if it is an integer or a real, and
!  allocate the type accordingly.
!
!@note Complete rewrite of the original FSON routine, which had some problems.

    SUBROUTINE PARSE_NUMBER(JSON, UNIT, STR, VALUE)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    INTEGER(IK),INTENT(IN)              :: UNIT   !! file unit number (if parsing from a file)
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: STR    !! JSON string (if parsing from a string)
    TYPE(JSON_VALUE),POINTER            :: VALUE

    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: TMP !! temp string
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: SAVED_ERR_MESSAGE !! temp error message for
!! string to int conversion
    CHARACTER(KIND=CK,LEN=1) :: C           !! character returned by [[pop_char]]
    LOGICAL(LK)              :: EOF         !! end of file flag
    REAL(RK)                 :: RVAL        !! real value
    INTEGER(IK)              :: IVAL        !! integer value
    LOGICAL(LK)              :: FIRST       !! first character
    LOGICAL(LK)              :: IS_INTEGER  !! it is an integer
    INTEGER(IK)              :: IP          !! index to put next character
!! [to speed up by reducing the number
!! of character string reallocations]

    IF (.NOT. JSON%EXCEPTION_THROWN) THEN

        TMP = BLANK_CHUNK
        IP = 1
        FIRST = .TRUE.
        IS_INTEGER = .TRUE.  !assume it may be an integer, unless otherwise determined

!read one character at a time and accumulate the string:
        DO

!get the next character:
            CALL JSON%POP_CHAR(UNIT, STR=STR, EOF=EOF, SKIP_WS=.TRUE., POPPED=C)

            SELECT CASE (C)
            CASE(CK_'-',CK_'+')    !note: allowing a '+' as the first character here.

                IF (IS_INTEGER .AND. (.NOT. FIRST)) IS_INTEGER = .FALSE.

!add it to the string:
!tmp = tmp // c   !...original
                IF (IP>LEN(TMP)) TMP = TMP // BLANK_CHUNK
                TMP(IP:IP) = C
                IP = IP + 1

            CASE(CK_'.',CK_'E',CK_'e',CK_'D',CK_'d')    !can be present in real numbers

                IF (IS_INTEGER) IS_INTEGER = .FALSE.

!add it to the string:
!tmp = tmp // c   !...original
                IF (IP>LEN(TMP)) TMP = TMP // BLANK_CHUNK
                TMP(IP:IP) = C
                IP = IP + 1

            CASE(CK_'0':CK_'9')    !valid characters for numbers

!add it to the string:
!tmp = tmp // c   !...original
                IF (IP>LEN(TMP)) TMP = TMP // BLANK_CHUNK
                TMP(IP:IP) = C
                IP = IP + 1

            CASE DEFAULT

!push back the last character read:
                CALL JSON%PUSH_CHAR(C)

!string to value:
                IF (IS_INTEGER) THEN
! it is an integer:
                    IVAL = JSON%STRING_TO_INT(TMP)

                    IF (JSON%EXCEPTION_THROWN .AND. .NOT. JSON%STRICT_INTEGER_TYPE_CHECKING) THEN
! if it couldn't be converted to an integer,
! then try to convert it to a real value and see if that works

                        SAVED_ERR_MESSAGE = JSON%ERR_MESSAGE  ! keep the original error message
                        CALL JSON%CLEAR_EXCEPTIONS()          ! clear exceptions
                        RVAL = JSON%STRING_TO_DBLE(TMP)
                        IF (JSON%EXCEPTION_THROWN) THEN
! restore original error message and continue
                            JSON%ERR_MESSAGE = SAVED_ERR_MESSAGE
                            CALL JSON%TO_INTEGER(VALUE,IVAL) ! just so we have something
                        ELSE
! in this case, we return a real
                            CALL JSON%TO_REAL(VALUE,RVAL)
                        END IF

                    ELSE
                        CALL JSON%TO_INTEGER(VALUE,IVAL)
                    END IF

                ELSE
! it is a real:
                    RVAL = JSON%STRING_TO_DBLE(TMP)
                    CALL JSON%TO_REAL(VALUE,RVAL)
                END IF

                EXIT    !finished

            END SELECT

            IF (FIRST) FIRST = .FALSE.

        END DO

!cleanup:
        IF (ALLOCATED(TMP)) DEALLOCATE(TMP)

    END IF

    END SUBROUTINE PARSE_NUMBER
!*****************************************************************************************

!*****************************************************************************************
!>
!  Get the next character from the file (or string).
!
!--- See also
!  * [[push_char]]
!
!@note This routine ignores non-printing ASCII characters
!      (`iachar<=31`) that are in strings.

    SUBROUTINE POP_CHAR(JSON,UNIT,STR,SKIP_WS,SKIP_COMMENTS,EOF,POPPED)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)       :: JSON
    INTEGER(IK),INTENT(IN)               :: UNIT          !! file unit number (if parsing
!! from a file)
    CHARACTER(KIND=CK,LEN=*),INTENT(IN)  :: STR           !! JSON string (if parsing from a
!! string) -- only used if `unit=0`
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: SKIP_WS       !! to ignore whitespace [default False]
    LOGICAL(LK),INTENT(IN),OPTIONAL      :: SKIP_COMMENTS !! to ignore comment lines [default False]
    LOGICAL(LK),INTENT(OUT)              :: EOF           !! true if the end of the file has
!! been reached.
    CHARACTER(KIND=CK,LEN=1),INTENT(OUT) :: POPPED        !! the popped character returned

    INTEGER(IK)              :: IOS             !! `iostat` flag
    INTEGER(IK)              :: STR_LEN         !! length of `str`
    CHARACTER(KIND=CK,LEN=1) :: C               !! a character read from the file (or string)
    LOGICAL(LK)              :: IGNORE          !! if whitespace is to be ignored
    LOGICAL(LK)              :: IGNORE_COMMENTS !! if comment lines are to be ignored
    LOGICAL(LK)              :: PARSING_COMMENT !! if we are in the process
!! of parsing a comment line

    IF (.NOT. JSON%EXCEPTION_THROWN) THEN

        EOF = .FALSE.
        IF (.NOT. PRESENT(SKIP_WS)) THEN
            IGNORE = .FALSE.
        ELSE
            IGNORE = SKIP_WS
        END IF
        PARSING_COMMENT = .FALSE.
        IF (.NOT. PRESENT(SKIP_COMMENTS)) THEN
            IGNORE_COMMENTS = .FALSE.
        ELSE
            IGNORE_COMMENTS = SKIP_COMMENTS
        END IF

        DO

            IF (JSON%PUSHED_INDEX > 0) THEN

! there is a character pushed back on, most likely
! from the number parsing. Note: this can only occur if
! reading from a file when use_unformatted_stream=.false.
                C = JSON%PUSHED_CHAR(JSON%PUSHED_INDEX:JSON%PUSHED_INDEX)
                JSON%PUSHED_INDEX = JSON%PUSHED_INDEX - 1

            ELSE

                IF (UNIT/=0) THEN    !read from the file

!read the next character:
                    IF (USE_UNFORMATTED_STREAM) THEN

! in this case, we read the file in chunks.
! if we already have the character we need,
! then get it from the chunk. Otherwise,
! read in another chunk.
                        IF (JSON%ICHUNK<1) THEN
! read in a chunk:
                            JSON%ICHUNK = 0
                            IF (JSON%FILESIZE<JSON%IPOS+LEN(JSON%CHUNK)-1) THEN
! for the last chunk, we resize
! it to the correct size:
                                JSON%CHUNK = REPEAT(SPACE, JSON%FILESIZE-JSON%IPOS+1)
                            END IF
                            READ(UNIT=UNIT,POS=JSON%IPOS,IOSTAT=IOS) JSON%CHUNK
                        ELSE
                            IOS = 0
                        END IF
                        JSON%ICHUNK = JSON%ICHUNK + 1
                        IF (JSON%ICHUNK>LEN(JSON%CHUNK)) THEN
! check this just in case
                            IOS = IOSTAT_END
                        ELSE
! get the next character from the chunk:
                            C = JSON%CHUNK(JSON%ICHUNK:JSON%ICHUNK)
                            IF (JSON%ICHUNK==LEN(JSON%CHUNK)) THEN
                                JSON%ICHUNK = 0 ! reset for next chunk
                            END IF
                        END IF

                    ELSE
! a formatted read:
                        READ(UNIT=UNIT,FMT='(A1)',ADVANCE='NO',IOSTAT=IOS) C
                    END IF
                    JSON%IPOS = JSON%IPOS + 1

                ELSE    !read from the string

                    STR_LEN = LEN(STR)   !length of the string
                    IF (JSON%IPOS<=STR_LEN) THEN
                        C = STR(JSON%IPOS:JSON%IPOS)
                        IOS = 0
                    ELSE
                        IOS = IOSTAT_END  !end of the string
                    END IF
                    JSON%IPOS = JSON%IPOS + 1

                END IF

                JSON%CHAR_COUNT = JSON%CHAR_COUNT + 1    !character count in the current line

                IF (IS_IOSTAT_END(IOS)) THEN  !end of file

! no character to return
                    JSON%CHAR_COUNT = 0
                    EOF = .TRUE.
                    POPPED = SPACE ! just to set a value
                    EXIT

                ELSE IF (IS_IOSTAT_EOR(IOS) .OR. C==NEWLINE) THEN    !end of record

                    JSON%CHAR_COUNT = 0
                    JSON%LINE_COUNT = JSON%LINE_COUNT + 1
                    IF (IGNORE_COMMENTS) PARSING_COMMENT = .FALSE. ! done parsing this comment line
                    CYCLE

                END IF

            END IF

            IF (IGNORE_COMMENTS .AND. (PARSING_COMMENT .OR. SCAN(C,JSON%COMMENT_CHAR,KIND=IK)>0_IK) ) THEN

! skipping the comment
                PARSING_COMMENT = .TRUE.
                CYCLE

            ELSE IF (ANY(C == CONTROL_CHARS)) THEN

! non printing ascii characters
                CYCLE

            ELSE IF (IGNORE .AND. C == SPACE) THEN

! ignoring whitespace
                CYCLE

            ELSE

! return the character
                POPPED = C
                EXIT

            END IF

        END DO

    END IF

    END SUBROUTINE POP_CHAR
!*****************************************************************************************

!*****************************************************************************************
!>
!  Core routine.
!
!--- See also
!  * [[pop_char]]
!
!--- History
!  * Jacob Williams : 5/3/2015 : replaced original version of this routine.

    SUBROUTINE PUSH_CHAR(JSON,C)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT)      :: JSON
    CHARACTER(KIND=CK,LEN=1),INTENT(IN) :: C     !! to character to push

    CHARACTER(KIND=CK,LEN=MAX_NUMERIC_STR_LEN) :: ISTR  !! for error printing

    IF (.NOT. JSON%EXCEPTION_THROWN) THEN

        IF (USE_UNFORMATTED_STREAM) THEN

!in this case, c is ignored, and we just
!decrement the stream position counter:
            JSON%IPOS = JSON%IPOS - 1
            JSON%ICHUNK = JSON%ICHUNK - 1

        ELSE

            JSON%PUSHED_INDEX = JSON%PUSHED_INDEX + 1

            IF (JSON%PUSHED_INDEX>0 .AND. JSON%PUSHED_INDEX<=LEN(JSON%PUSHED_CHAR)) THEN
                JSON%PUSHED_CHAR(JSON%PUSHED_INDEX:JSON%PUSHED_INDEX) = C
            ELSE
                CALL INTEGER_TO_STRING(JSON%PUSHED_INDEX,INT_FMT,ISTR)
                CALL JSON%THROW_EXCEPTION('Error in push_char: '//&
                                          'invalid valid of pushed_index: '//TRIM(ISTR))
            END IF

        END IF

!character count in the current line
        JSON%CHAR_COUNT = JSON%CHAR_COUNT - 1

    END IF

    END SUBROUTINE PUSH_CHAR
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Print any error message, and then clear the exceptions.
!
!@note This routine is used by the unit tests.
!      It was originally in json_example.f90, and was
!      moved here 2/26/2015 by Izaak Beekman.

    SUBROUTINE JSON_PRINT_ERROR_MESSAGE(JSON,IO_UNIT)

    IMPLICIT NONE

    CLASS(JSON_CORE),INTENT(INOUT) :: JSON
    INTEGER, INTENT(IN), OPTIONAL  :: IO_UNIT  !! unit number for
!! printing error message

    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: ERROR_MSG  !! error message
    LOGICAL :: STATUS_OK !! false if there were any errors thrown

!get error message:
    CALL JSON%CHECK_FOR_ERRORS(STATUS_OK, ERROR_MSG)

!print it if there is one:
    IF (.NOT. STATUS_OK) THEN
        IF (PRESENT(IO_UNIT)) THEN
            WRITE(IO_UNIT,'(A)') ERROR_MSG
        ELSE
            WRITE(OUTPUT_UNIT,'(A)') ERROR_MSG
        END IF
        DEALLOCATE(ERROR_MSG)
        CALL JSON%CLEAR_EXCEPTIONS()
    END IF

    END SUBROUTINE JSON_PRINT_ERROR_MESSAGE
!*****************************************************************************************

!*****************************************************************************************
    END MODULE ModLib_JSONValue
!*****************************************************************************************

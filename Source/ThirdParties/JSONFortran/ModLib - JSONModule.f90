!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_module.F90"
!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  A Modern Fortran JSON (JavaScript Object Notation) API.
!
!  This module provides access to [[json_value_module]] and
!  [[json_file_module]]. For normal JSON-Fortran use, using this module
!  is all that is necessary.
!
!  Note that this module renames the kind definition variables from [[ModLib_JSONKinds]]
!  from [`RK`, `IK`, `LK`, `CK`, and `CDK`] to [`json_RK`, `json_IK`, `json_LK`,
!  `json_CK`, and `json_CDK`] so as to avoid namespace pollution with short
!  variable names.
!
!- 23

!
!--- License
!  * JSON-Fortran is released under a BSD-style license.
!    See the [LICENSE](https://github.com/jacobwilliams/json-fortran/blob/master/LICENSE)
!    file for details.
!
!--- History
!  * Joseph A. Levin : March 2012 : Original [FSON](https://github.com/josephalevin/fson)
!    code [retrieved on 12/2/2013].
!  * Jacob Williams : 2/8/2014 : Extensive modifications to the original FSON code.
!    The original F95 code was split into four files:
!    fson_path_m.f95, fson_string_m.f95, fson_value_m.f95, and fson.f95.
!    The new code has been extensively updated, refactored and combined into this
!    one module (json_module.f90).
!    Various Fortran 2003/2008 features are now used
!    (e.g., allocatable strings, newunit, generic, class, and abstract interface).
!  * Development continues at: [Github](https://github.com/jacobwilliams/json-fortran)
!
!--- See also
!  * [json-fortran development site](https://github.com/jacobwilliams/json-fortran)
!  * [json-fortran online documentation](https://jacobwilliams.github.io/json-fortran)
!  * [JSON website](http://www.json.org/)
!  * [JSON validator](http://jsonlint.com/)
!
!@note Originally JSON-Fortran was entirely contained within this module.

    MODULE ModLib_JSONModule

    USE ModLib_JSONKinds, ONLY: JSON_RK  => RK, &
                          JSON_IK  => IK, &
                          JSON_LK  => LK, &
                          JSON_CK  => CK, &
                          JSON_CDK => CDK
!- 61

    USE ModLib_JSONParameters, ONLY: JSON_UNKNOWN,&
                               JSON_NULL,   &
                               JSON_OBJECT, &
                               JSON_ARRAY,  &
                               JSON_LOGICAL,&
                               JSON_INTEGER,&
                               JSON_REAL,   &
                               JSON_DOUBLE, &
                               JSON_STRING
    USE ModLib_JSONValue
    USE ModLib_JSONFile

    IMPLICIT NONE

    CHARACTER(KIND=JSON_CK,LEN=*),PARAMETER,PRIVATE :: VERSION = '8.5.0'
!! JSON-Fortran version.
!!
!!@note This string should match the one in the `.VERSION` file (which is used
!!      for the documentation generation.)

    PUBLIC

    CONTAINS
!*****************************************************************************************

!*****************************************************************************************
!>
!  Returns the JSON-Fortran version string.

    FUNCTION JSON_FORTRAN_VERSION() RESULT(VER)

    IMPLICIT NONE

    CHARACTER(LEN=:),ALLOCATABLE :: VER  !! JSON-Fortran version string

    VER = VERSION

    END FUNCTION JSON_FORTRAN_VERSION
!*****************************************************************************************

!*****************************************************************************************
    END MODULE ModLib_JSONModule
!*****************************************************************************************

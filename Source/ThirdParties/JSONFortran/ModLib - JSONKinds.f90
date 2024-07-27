!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\ModLib_JSONKinds.F90"
!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  JSON-Fortran kind definitions.
!
!--- License
!  * JSON-Fortran is released under a BSD-style license.
!    See the [LICENSE](https://github.com/jacobwilliams/json-fortran/blob/master/LICENSE)
!    file for details.
!
!@note ```-DUSE_UCS4``` is an optional preprocessor flag.
!      When present, Unicode support is enabled. Note that this
!      is currently only supported with the gfortran compiler.
!      Example: ```gfortran -DUSE_UCS4 ... ```
!- 21

!      The documentation given here assumes ```USE_UCS4``` **is not** defined.

!
!@warning ```CK``` and ```CDK``` are the JSON-Fortran character kind and JSON-Fortran default
!         character kind respectively. Client code **MUST** ensure characters of ```kind=CK```
!         are used for all character variables and strings passed to the JSON-Fortran
!         library *EXCEPT* for file names which must be of ```'DEFAULT'``` character kind,
!         provided here as ```CDK```. In particular, any variable that is a: json path, string
!         value or object name passed to the JSON-Fortran library **MUST** be of type ```CK```.
!
!@note Most string literal constants of default kind are fine to pass as arguments to
!      JSON-Fortran procedures since they have been overloaded to accept ```intent(in)```
!      character arguments of the default (```CDK```) kind. If you find a procedure which does
!      not accept an ```intent(in)``` literal string argument of default kind, please
!      [file an issue](https://github.com/jacobwilliams/json-fortran/issues/new) on GitHub.
!
!@note The default real kind (`RK`) and the default integer kind (`IK`) can be
!      changed using optional preprocessor flags. This library was built with kinds:
!- 46

!      real(kind=real64) [8 bytes]

!      and
!- 58

!      integer(kind=int32) [4 bytes]

!      .
!
!@note In addition to the real kind specified by `RK`, interfaces for
!      the real kinds with less precision are also provided in the library,
!      but all are converted to `real(RK)` variables internally.

    MODULE ModLib_JSONKinds

    USE,INTRINSIC :: ISO_FORTRAN_ENV

    IMPLICIT NONE

    PRIVATE

! used for the reals with less precision
! than the default precision:

    PUBLIC :: REAL32

!- 82


!- 90

    INTEGER,PARAMETER,PUBLIC :: RK = REAL64   !! Default real kind if not specified [8 bytes]


!- 102

    INTEGER,PARAMETER,PUBLIC :: IK = INT32    !! Default integer kind if not specified [4 bytes]


!*********************************************************
!>
!  Processor dependent 'DEFAULT' character kind.
!  This is 1 byte for the Intel and Gfortran compilers.
    INTEGER,PARAMETER,PUBLIC :: CDK = SELECTED_CHAR_KIND('DEFAULT')
!*********************************************************

!*********************************************************
!>
!  Default logical kind.
!  This is 4 bytes for the Intel and Gfortran compilers
!  (and perhaps others).
!  The declaration ensures a valid kind
!  if the compiler doesn't have a logical_kinds(3).
    INTEGER,PARAMETER,PUBLIC :: LK = LOGICAL_KINDS(MIN(3,SIZE(LOGICAL_KINDS)))
!*********************************************************

!*********************************************************
!>
!  String kind preprocessor macro.
!- 129

! this is the string kind to use unless compiling with GFortran AND
! UCS4/ISO 10646 support is requested
    CHARACTER(KIND=CDK,LEN=*),PARAMETER :: JSON_FORTRAN_STRING_KIND = 'DEFAULT'

!*********************************************************

!*********************************************************
!>
!  Default character kind used by JSON-Fortran.
!  If ISO 10646 (UCS4) support is available, use that,
!  otherwise, gracefully fall back on 'DEFAULT' characters.
!  Currently only gfortran >= 4.9.2 will correctly support
!  UCS4 which is stored in 4 bytes.
!  (and perhaps others).
    INTEGER,PARAMETER,PUBLIC :: CK = SELECTED_CHAR_KIND(JSON_FORTRAN_STRING_KIND)
!*********************************************************

    END MODULE ModLib_JSONKinds
!*****************************************************************************************

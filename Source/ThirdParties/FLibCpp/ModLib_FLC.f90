! This file was automatically generated by SWIG (http://www.swig.org).
! Version 4.0.2+fortran
!
! Do not make changes to this file unless you know what you are doing--modify
! the SWIG interface file instead.

! Flibcpp project, https://github.com/swig-fortran/flibcpp
! Copyright (c) 2019-2020 Oak Ridge National Laboratory, UT-Battelle, LLC.
! Distributed under an MIT open source license: see LICENSE for details.

MODULE ModLib_FLC
 USE, INTRINSIC :: ISO_C_BINDING
 IMPLICIT NONE
 PRIVATE

 ! DECLARATION CONSTRUCTS
 INTEGER(C_INT), PUBLIC, &
   BIND(C, name="flc_ierr") :: ierr
 TYPE, BIND(C) :: SwigArrayWrapper
  TYPE(C_PTR), PUBLIC :: DATA = C_NULL_PTR
  INTEGER(C_SIZE_T), PUBLIC :: SIZE = 0
 END TYPE
 PUBLIC :: get_serr
 INTEGER(C_INT), PARAMETER, PUBLIC :: SWIG_UnknownError = -1_C_INT
 INTEGER(C_INT), PARAMETER, PUBLIC :: SWIG_IOError = -2_C_INT
 INTEGER(C_INT), PARAMETER, PUBLIC :: SWIG_RuntimeError = -3_C_INT
 INTEGER(C_INT), PARAMETER, PUBLIC :: SWIG_IndexError = -4_C_INT
 INTEGER(C_INT), PARAMETER, PUBLIC :: SWIG_TypeError = -5_C_INT
 INTEGER(C_INT), PARAMETER, PUBLIC :: SWIG_DivisionByZero = -6_C_INT
 INTEGER(C_INT), PARAMETER, PUBLIC :: SWIG_OverflowError = -7_C_INT
 INTEGER(C_INT), PARAMETER, PUBLIC :: SWIG_SyntaxError = -8_C_INT
 INTEGER(C_INT), PARAMETER, PUBLIC :: SWIG_ValueError = -9_C_INT
 INTEGER(C_INT), PARAMETER, PUBLIC :: SWIG_SystemError = -10_C_INT
 INTEGER(C_INT), PARAMETER, PUBLIC :: SWIG_AttributeError = -11_C_INT
 INTEGER(C_INT), PARAMETER, PUBLIC :: SWIG_MemoryError = -12_C_INT
 INTEGER(C_INT), PARAMETER, PUBLIC :: SWIG_NullReferenceError = -13_C_INT
 PUBLIC :: get_flibcpp_version
 INTEGER(C_INT), PUBLIC, PROTECTED, &
   BIND(C, name="flibcpp_version_major") :: flibcpp_version_major
 INTEGER(C_INT), PUBLIC, PROTECTED, &
   BIND(C, name="flibcpp_version_minor") :: flibcpp_version_minor
 INTEGER(C_INT), PUBLIC, PROTECTED, &
   BIND(C, name="flibcpp_version_patch") :: flibcpp_version_patch

! WRAPPER DECLARATIONS
INTERFACE
 SUBROUTINE SWIG_free(cptr) &
  BIND(C, name="free")
 USE, INTRINSIC :: ISO_C_BINDING
 TYPE(C_PTR), VALUE :: cptr
END SUBROUTINE
FUNCTION swigc_get_serr() &
BIND(C, name="_wrap_get_serr") &
RESULT(fresult)
USE, INTRINSIC :: ISO_C_BINDING
IMPORT :: swigarraywrapper
TYPE(SwigArrayWrapper) :: fresult
END FUNCTION

FUNCTION swigc_flibcpp_version_get() &
BIND(C, name="_wrap_flibcpp_version_get") &
RESULT(fresult)
USE, INTRINSIC :: ISO_C_BINDING
IMPORT :: swigarraywrapper
TYPE(SwigArrayWrapper) :: fresult
END FUNCTION

END INTERFACE


CONTAINS
 ! MODULE SUBPROGRAMS

SUBROUTINE SWIGTM_fout_char_Sm_(imout, fout)
  USE, INTRINSIC :: ISO_C_BINDING
  TYPE(SwigArrayWrapper), INTENT(IN) :: imout
  CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: fout
  CHARACTER(KIND=C_CHAR), DIMENSION(:), POINTER :: chars
  INTEGER(KIND=C_SIZE_T) :: i
  CALL c_f_pointer(imout%DATA, chars, [imout%SIZE])
  ALLOCATE(CHARACTER(LEN=imout%SIZE) :: fout)
  DO i=1, imout%SIZE
    fout(i:i) = CHAR(ICHAR(chars(i)))
  END DO
END SUBROUTINE

FUNCTION get_serr() &
RESULT(swig_result)
USE, INTRINSIC :: ISO_C_BINDING
CHARACTER(LEN=:), ALLOCATABLE :: swig_result
TYPE(SwigArrayWrapper) :: fresult

fresult = swigc_get_serr()
CALL SWIGTM_fout_char_Sm_(fresult, swig_result)
IF (.FALSE.) CALL SWIG_free(fresult%DATA)
END FUNCTION

FUNCTION get_flibcpp_version() &
RESULT(swig_result)
USE, INTRINSIC :: ISO_C_BINDING
CHARACTER(LEN=:), ALLOCATABLE :: swig_result
TYPE(SwigArrayWrapper) :: fresult

fresult = swigc_flibcpp_version_get()
CALL SWIGTM_fout_char_Sm_(fresult, swig_result)
IF (.FALSE.) CALL SWIG_free(fresult%DATA)
END FUNCTION


END MODULE
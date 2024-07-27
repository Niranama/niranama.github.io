!< VTK_Fortran parameters.
MODULE ModLib_VTKParameters
!< VTK_Fortran parameters.
USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : OUTPUT_UNIT, ERROR_UNIT
USE ModLib_Penf

IMPLICIT NONE
PRIVATE
SAVE
PUBLIC :: STDERR
PUBLIC :: STDOUT
PUBLIC :: END_REC

INTEGER(I4P), PARAMETER :: STDERR = ERROR_UNIT  !< Standard error unit.
INTEGER(I4P), PARAMETER :: STDOUT = OUTPUT_UNIT !< Standard output unit.
CHARACTER(1), PARAMETER :: END_REC = CHAR(10)   !< End-character for binary-record finalize.
ENDMODULE ModLib_VTKParameters

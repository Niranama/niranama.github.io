!< VTK_Fortran, pure Fortran (2003+) library to parse and emitt VTK files.
MODULE ModLib_VTKFortran
!< VTK_Fortran, pure Fortran (2003+) library to parse and emitt VTK files.
USE ModLib_Penf
USE ModLib_PVTKFile, ONLY : PVTK_FILE
USE ModLib_VTKFile, ONLY : VTK_FILE
USE ModLib_VTMFile, ONLY : VTM_FILE

IMPLICIT NONE
PRIVATE
PUBLIC :: PVTK_FILE
PUBLIC :: VTK_FILE
PUBLIC :: VTM_FILE
PUBLIC :: WRITE_XML_VOLATILE

CONTAINS
   FUNCTION WRITE_XML_VOLATILE(XML_VOLATILE, FILENAME) RESULT(ERROR)
   !< Write the volatile file into a real file.
   !< This is what a master process should do into a parallel scenario where it being the only process allowed TO ACCESS TO
   !< filesystem: slave processes create XML volatile file econded into a characters string and master process COLLECTS AND WRITES
   !< them by means of `write_xml_volatile`.
   CHARACTER(*), INTENT(IN) :: XML_VOLATILE !< XML volatile file.
   CHARACTER(*), INTENT(IN) :: FILENAME     !< XML file name.
   INTEGER(I4P)             :: ERROR        !< Status error.
   INTEGER(I4P)             :: XML_UNIT     !< XML file unit.

   OPEN(NEWUNIT=XML_UNIT,             &
        FILE=TRIM(ADJUSTL(FILENAME)), &
        FORM='UNFORMATTED',           &
        ACCESS='STREAM',              &
        ACTION='WRITE',               &
        STATUS='REPLACE',             &
        IOSTAT=ERROR)
   WRITE(UNIT=XML_UNIT, IOSTAT=ERROR) XML_VOLATILE
   ENDFUNCTION WRITE_XML_VOLATILE
ENDMODULE ModLib_VTKFortran

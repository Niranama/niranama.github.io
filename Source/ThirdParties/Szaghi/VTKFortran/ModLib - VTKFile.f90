!< VTK file class.
MODULE ModLib_VTKFile
!< VTK file class.
USE ModLib_BeFor64
USE ModLib_Penf
USE ModLib_Stringifor
USE ModLib_XMLWriterAbstract
USE ModLib_XMLWriterAppended
USE ModLib_XMLWriterASCII
USE ModLib_XMLWriterBinary

IMPLICIT NONE
PRIVATE
PUBLIC :: VTK_FILE

TYPE :: VTK_FILE
  !< VTK file class.
  PRIVATE
  CLASS(XML_WRITER_ABSTRACT), ALLOCATABLE, PUBLIC :: XML_WRITER !< XML writer.
  CONTAINS
    PROCEDURE, PASS(SELF) :: GET_XML_VOLATILE !< Return the eventual XML volatile string file.
    PROCEDURE, PASS(SELF) :: INITIALIZE       !< Initialize file.
    PROCEDURE, PASS(SELF) :: FINALIZE         !< Finalize file.
    PROCEDURE, PASS(SELF) :: FREE             !< Free allocated memory.
ENDTYPE VTK_FILE
CONTAINS
   PURE SUBROUTINE GET_XML_VOLATILE(SELF, XML_VOLATILE, ERROR)
   !< Return the eventual XML volatile string file.
   CLASS(VTK_FILE),  INTENT(IN)               :: SELF         !< VTK file.
   CHARACTER(LEN=:), INTENT(OUT), ALLOCATABLE :: XML_VOLATILE !< XML volatile file.
   INTEGER(I4P),     INTENT(OUT), OPTIONAL    :: ERROR !< Error status.

   CALL SELF%XML_WRITER%GET_XML_VOLATILE(XML_VOLATILE=XML_VOLATILE, ERROR=ERROR)
   ENDSUBROUTINE GET_XML_VOLATILE

   FUNCTION INITIALIZE(SELF, FORMAT, FILENAME, MESH_TOPOLOGY, IS_VOLATILE, NX1, NX2, NY1, NY2, NZ1, NZ2) RESULT(ERROR)
   !< Initialize file (writer).
   !<
   !< @note This function must be the first to be called.
   !<
   !<### Supported output formats are (the passed specifier value is case insensitive):
   !<
   !<- ASCII: data are saved in ASCII format;
   !<- BINARY: data are saved in base64 encoded format;
   !<- RAW: data are saved in raw-binary format in the appended tag of the XML file;
   !<- BINARY-APPENDED: data are saved in base64 encoded format in the appended tag of the XML file.
   !<
   !<### Supported topologies are:
   !<
   !<- RectilinearGrid;
   !<- StructuredGrid;
   !<- UnstructuredGrid.
   !<
   !<### Example of usage
   !<
   !<```fortran
   !< type(vtk_file) :: vtk
   !< integer(I4P)   :: nx1, nx2, ny1, ny2, nz1, nz2
   !< ...
   !< error = vtk%initialize('BINARY','XML_RECT_BINARY.vtr','RectilinearGrid',nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,NZ1=NZ1,NZ2=NZ2)
   !< ...
   !<```
   !< @note The file extension is necessary in the file name. The XML standard has different extensions for eaCH
   !< different topologies (e.g. *vtr* for rectilinear topology). See the VTK-standard file for more informatiON.
   CLASS(VTK_FILE), INTENT(INOUT)        :: SELF          !< VTK file.
   CHARACTER(*),    INTENT(IN)           :: FORMAT        !< File format: ASCII, BINARY, RAW or BINARY-APPENDED.
   CHARACTER(*),    INTENT(IN)           :: FILENAME      !< File name.
   CHARACTER(*),    INTENT(IN)           :: MESH_TOPOLOGY !< Mesh topology.
   LOGICAL,         INTENT(IN), OPTIONAL :: IS_VOLATILE   !< Flag to check volatile writer.
   INTEGER(I4P),    INTENT(IN), OPTIONAL :: NX1           !< Initial node of x axis.
   INTEGER(I4P),    INTENT(IN), OPTIONAL :: NX2           !< Final node of x axis.
   INTEGER(I4P),    INTENT(IN), OPTIONAL :: NY1           !< Initial node of y axis.
   INTEGER(I4P),    INTENT(IN), OPTIONAL :: NY2           !< Final node of y axis.
   INTEGER(I4P),    INTENT(IN), OPTIONAL :: NZ1           !< Initial node of z axis.
   INTEGER(I4P),    INTENT(IN), OPTIONAL :: NZ2           !< Final node of z axis.
   INTEGER(I4P)                          :: ERROR         !< Error status.
   TYPE(STRING)                          :: FFORMAT       !< File format.

   IF (.NOT.IS_INITIALIZED) CALL PENF_INIT
   IF (.NOT.IS_B64_INITIALIZED) CALL B64_INIT
   FFORMAT = TRIM(ADJUSTL(FORMAT))
   FFORMAT = FFORMAT%UPPER()
   IF (ALLOCATED(SELF%XML_WRITER)) DEALLOCATE(SELF%XML_WRITER)
   SELECT CASE(FFORMAT%CHARS())
   CASE('ASCII')
      ALLOCATE(XML_WRITER_ASCII_LOCAL :: SELF%XML_WRITER)
   CASE('BINARY-APPENDED', 'RAW')
      ALLOCATE(XML_WRITER_APPENDED :: SELF%XML_WRITER)
   CASE('BINARY')
      ALLOCATE(XML_WRITER_BINARY_LOCAL :: SELF%XML_WRITER)
   CASE DEFAULT
      ERROR = 1
   ENDSELECT
   ERROR = SELF%XML_WRITER%INITIALIZE(FORMAT=FORMAT, FILENAME=FILENAME, MESH_TOPOLOGY=MESH_TOPOLOGY, &
                                      IS_VOLATILE=IS_VOLATILE,                                       &
                                      NX1=NX1, NX2=NX2, NY1=NY1, NY2=NY2, NZ1=NZ1, NZ2=NZ2)
   ENDFUNCTION INITIALIZE

   FUNCTION FINALIZE(SELF) RESULT(ERROR)
   !< Finalize file (writer).
   CLASS(VTK_FILE), INTENT(INOUT)  :: SELF  !< VTK file.
   INTEGER(I4P)                    :: ERROR !< Error status.
   CHARACTER(LEN=:),           ALLOCATABLE :: XML_VOLATILE !< XML volatile file.

   ERROR = 1
   IF (ALLOCATED(SELF%XML_WRITER)) ERROR = SELF%XML_WRITER%FINALIZE()
   ENDFUNCTION FINALIZE

   ELEMENTAL SUBROUTINE FREE(SELF, ERROR)
   !< Free allocated memory.
   CLASS(VTK_FILE), INTENT(INOUT)         :: SELF  !< VTK file.
   INTEGER(I4P),    INTENT(OUT), OPTIONAL :: ERROR !< Error status.

   IF (ALLOCATED(SELF%XML_WRITER)) CALL SELF%XML_WRITER%FREE(ERROR=ERROR)
   ENDSUBROUTINE FREE
ENDMODULE ModLib_VTKFile

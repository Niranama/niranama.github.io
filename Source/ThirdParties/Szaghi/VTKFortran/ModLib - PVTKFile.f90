!< Parallel (partioned) VTK file class.
MODULE ModLib_PVTKFile
!< Parallel (partioned) VTK file class.
USE ModLib_BeFor64
USE ModLib_Penf
USE ModLib_XMLWriterAbstract
USE ModLib_XMLWriterASCII

IMPLICIT NONE
PRIVATE
PUBLIC :: PVTK_FILE

TYPE :: PVTK_FILE
  !< VTK parallel (partioned) file class.
  PRIVATE
  CLASS(XML_WRITER_ABSTRACT), ALLOCATABLE, PUBLIC :: XML_WRITER !< XML writer.
  CONTAINS
    PROCEDURE, PASS(SELF) :: INITIALIZE !< Initialize file.
    PROCEDURE, PASS(SELF) :: FINALIZE   !< Finalize file.
ENDTYPE PVTK_FILE
CONTAINS
  FUNCTION INITIALIZE(SELF, FILENAME, MESH_TOPOLOGY, MESH_KIND, NX1, NX2, NY1, NY2, NZ1, NZ2) RESULT(ERROR)
  !< Initialize file (writer).
  !<
  !< @note This function must be the first to be called.
  !<
  !<### Supported topologies are:
  !<
  !<- PRectilinearGrid;
  !<- PStructuredGrid;
  !<- PUnstructuredGrid.
  !<
  !<### Example of usage
  !<
  !<```fortran
  !< type(pvtk_file) :: pvtk
  !< integer(I4P)    :: nx1, nx2, ny1, ny2, nz1, nz2
  !< ...
  !< error = pvtk%initialize('XML_RECT_BINARY.pvtr','PRectilinearGrid',nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,NZ2=NZ2)
  !< ...
  !<```
  !< @note The file extension is necessary in the file name. The XML standard has different extensions for each
  !< different topologies (e.g. *pvtr* for rectilinear topology). See the VTK-standard file for more informatiON.
  CLASS(PVTK_FILE), INTENT(INOUT)         :: SELF          !< VTK file.
  CHARACTER(*),     INTENT(IN)            :: FILENAME      !< File name.
  CHARACTER(*),     INTENT(IN)            :: MESH_TOPOLOGY !< Mesh topology.
  CHARACTER(*),     INTENT(IN)            :: MESH_KIND     !< Kind of mesh data: Float64, Float32, ecc.
  INTEGER(I4P),     INTENT(IN),  OPTIONAL :: NX1           !< Initial node of x axis.
  INTEGER(I4P),     INTENT(IN),  OPTIONAL :: NX2           !< Final node of x axis.
  INTEGER(I4P),     INTENT(IN),  OPTIONAL :: NY1           !< Initial node of y axis.
  INTEGER(I4P),     INTENT(IN),  OPTIONAL :: NY2           !< Final node of y axis.
  INTEGER(I4P),     INTENT(IN),  OPTIONAL :: NZ1           !< Initial node of z axis.
  INTEGER(I4P),     INTENT(IN),  OPTIONAL :: NZ2           !< Final node of z axis.
  INTEGER(I4P)                            :: ERROR         !< Error status.

  IF (.NOT.IS_INITIALIZED) CALL PENF_INIT
  IF (.NOT.IS_B64_INITIALIZED) CALL B64_INIT
  IF (ALLOCATED(SELF%XML_WRITER)) DEALLOCATE(SELF%XML_WRITER)
  ALLOCATE(XML_WRITER_ASCII_LOCAL :: SELF%XML_WRITER)
  ERROR = SELF%XML_WRITER%INITIALIZE(FORMAT='ascii', FILENAME=FILENAME, MESH_TOPOLOGY=MESH_TOPOLOGY, &
                                     NX1=NX1, NX2=NX2, NY1=NY1, NY2=NY2, NZ1=NZ1, NZ2=NZ2, MESH_KIND=MESH_KIND)
  ENDFUNCTION INITIALIZE

  FUNCTION FINALIZE(SELF) RESULT(ERROR)
  !< Finalize file (writer).
  CLASS(PVTK_FILE), INTENT(INOUT) :: SELF  !< VTK file.
  INTEGER(I4P)                    :: ERROR !< Error status.

  ERROR = 1
  IF (ALLOCATED(SELF%XML_WRITER)) ERROR = SELF%XML_WRITER%FINALIZE()
  ENDFUNCTION FINALIZE
ENDMODULE ModLib_PVTKFile

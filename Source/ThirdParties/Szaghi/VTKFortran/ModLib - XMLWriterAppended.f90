!< VTK file XMl writer, appended.
MODULE ModLib_XMLWriterAppended
!< VTK file XMl writer, appended.
USE ModLib_Penf
USE ModLib_Stringifor
USE ModLib_VTKDataEncoder
USE ModLib_VTKParameters
USE ModLib_XMLWriterAbstract

IMPLICIT NONE
PRIVATE
PUBLIC :: XML_WRITER_APPENDED

TYPE, EXTENDS(XML_WRITER_ABSTRACT) :: XML_WRITER_APPENDED
  !< VTK file XML writer, appended.
  TYPE(STRING) :: ENCODING      !< Appended data encoding: "raw" or "base64".
  INTEGER(I4P) :: SCRATCH=0_I4P !< Scratch logical unit.
  CONTAINS
    ! deferred methods
    PROCEDURE, PASS(SELF) :: INITIALIZE                 !< Initialize writer.
    PROCEDURE, PASS(SELF) :: FINALIZE                   !< Finalize writer.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY1_RANK1_R8P !< Write dataarray 1, rank 1, R8P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY1_RANK1_R4P !< Write dataarray 1, rank 1, R4P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY1_RANK1_I8P !< Write dataarray 1, rank 1, I8P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY1_RANK1_I4P !< Write dataarray 1, rank 1, I4P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY1_RANK1_I2P !< Write dataarray 1, rank 1, I2P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY1_RANK1_I1P !< Write dataarray 1, rank 1, I1P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY1_RANK2_R8P !< Write dataarray 1, rank 2, R8P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY1_RANK2_R4P !< Write dataarray 1, rank 2, R4P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY1_RANK2_I8P !< Write dataarray 1, rank 2, I8P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY1_RANK2_I4P !< Write dataarray 1, rank 2, I4P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY1_RANK2_I2P !< Write dataarray 1, rank 2, I2P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY1_RANK2_I1P !< Write dataarray 1, rank 2, I1P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY1_RANK3_R8P !< Write dataarray 1, rank 3, R8P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY1_RANK3_R4P !< Write dataarray 1, rank 3, R4P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY1_RANK3_I8P !< Write dataarray 1, rank 3, I8P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY1_RANK3_I4P !< Write dataarray 1, rank 3, I4P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY1_RANK3_I2P !< Write dataarray 1, rank 3, I2P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY1_RANK3_I1P !< Write dataarray 1, rank 3, I1P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY1_RANK4_R8P !< Write dataarray 1, rank 4, R8P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY1_RANK4_R4P !< Write dataarray 1, rank 4, R4P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY1_RANK4_I8P !< Write dataarray 1, rank 4, I8P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY1_RANK4_I4P !< Write dataarray 1, rank 4, I4P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY1_RANK4_I2P !< Write dataarray 1, rank 4, I2P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY1_RANK4_I1P !< Write dataarray 1, rank 4, I1P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY3_RANK1_R8P !< Write dataarray 3, rank 1, R8P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY3_RANK1_R4P !< Write dataarray 3, rank 1, R4P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY3_RANK1_I8P !< Write dataarray 3, rank 1, I8P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY3_RANK1_I4P !< Write dataarray 3, rank 1, I4P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY3_RANK1_I2P !< Write dataarray 3, rank 1, I2P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY3_RANK1_I1P !< Write dataarray 3, rank 1, I1P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY3_RANK3_R8P !< Write dataarray 3, rank 3, R8P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY3_RANK3_R4P !< Write dataarray 3, rank 3, R4P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY3_RANK3_I8P !< Write dataarray 3, rank 3, I8P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY3_RANK3_I4P !< Write dataarray 3, rank 3, I4P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY3_RANK3_I2P !< Write dataarray 3, rank 3, I2P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY3_RANK3_I1P !< Write dataarray 3, rank 3, I1P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY6_RANK1_R8P !< Write dataarray 6, rank 1, R8P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY6_RANK1_R4P !< Write dataarray 6, rank 1, R4P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY6_RANK1_I8P !< Write dataarray 6, rank 1, I8P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY6_RANK1_I4P !< Write dataarray 6, rank 1, I4P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY6_RANK1_I2P !< Write dataarray 6, rank 1, I2P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY6_RANK1_I1P !< Write dataarray 6, rank 1, I1P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY6_RANK3_R8P !< Write dataarray 6, rank 3, R8P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY6_RANK3_R4P !< Write dataarray 6, rank 3, R4P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY6_RANK3_I8P !< Write dataarray 6, rank 3, I8P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY6_RANK3_I4P !< Write dataarray 6, rank 3, I4P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY6_RANK3_I2P !< Write dataarray 6, rank 3, I2P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY6_RANK3_I1P !< Write dataarray 6, rank 3, I1P.
    PROCEDURE, PASS(SELF) :: WRITE_DATAARRAY_APPENDED   !< Write appended.
    ! private methods
    PROCEDURE, PASS(SELF), PRIVATE :: IOFFSET_UPDATE     !< Update ioffset count.
    PROCEDURE, PASS(SELF), PRIVATE :: OPEN_SCRATCH_FILE  !< Open scratch file.
    PROCEDURE, PASS(SELF), PRIVATE :: CLOSE_SCRATCH_FILE !< Close scratch file.
    GENERIC, PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY =>          &
                        WRITE_ON_SCRATCH_DATAARRAY1_RANK1,     &
                        WRITE_ON_SCRATCH_DATAARRAY1_RANK2,     &
                        WRITE_ON_SCRATCH_DATAARRAY1_RANK3,     &
                        WRITE_ON_SCRATCH_DATAARRAY1_RANK4,     &
                        WRITE_ON_SCRATCH_DATAARRAY3_RANK1_R8P, &
                        WRITE_ON_SCRATCH_DATAARRAY3_RANK1_R4P, &
                        WRITE_ON_SCRATCH_DATAARRAY3_RANK1_I8P, &
                        WRITE_ON_SCRATCH_DATAARRAY3_RANK1_I4P, &
                        WRITE_ON_SCRATCH_DATAARRAY3_RANK1_I2P, &
                        WRITE_ON_SCRATCH_DATAARRAY3_RANK1_I1P, &
                        WRITE_ON_SCRATCH_DATAARRAY3_RANK2_R8P, &
                        WRITE_ON_SCRATCH_DATAARRAY3_RANK2_R4P, &
                        WRITE_ON_SCRATCH_DATAARRAY3_RANK2_I8P, &
                        WRITE_ON_SCRATCH_DATAARRAY3_RANK2_I4P, &
                        WRITE_ON_SCRATCH_DATAARRAY3_RANK2_I2P, &
                        WRITE_ON_SCRATCH_DATAARRAY3_RANK2_I1P, &
                        WRITE_ON_SCRATCH_DATAARRAY3_RANK3_R8P, &
                        WRITE_ON_SCRATCH_DATAARRAY3_RANK3_R4P, &
                        WRITE_ON_SCRATCH_DATAARRAY3_RANK3_I8P, &
                        WRITE_ON_SCRATCH_DATAARRAY3_RANK3_I4P, &
                        WRITE_ON_SCRATCH_DATAARRAY3_RANK3_I2P, &
                        WRITE_ON_SCRATCH_DATAARRAY3_RANK3_I1P, &
                        WRITE_ON_SCRATCH_DATAARRAY6_RANK1_R8P, &
                        WRITE_ON_SCRATCH_DATAARRAY6_RANK1_R4P, &
                        WRITE_ON_SCRATCH_DATAARRAY6_RANK1_I8P, &
                        WRITE_ON_SCRATCH_DATAARRAY6_RANK1_I4P, &
                        WRITE_ON_SCRATCH_DATAARRAY6_RANK1_I2P, &
                        WRITE_ON_SCRATCH_DATAARRAY6_RANK1_I1P, &
                        WRITE_ON_SCRATCH_DATAARRAY6_RANK2_R8P, &
                        WRITE_ON_SCRATCH_DATAARRAY6_RANK2_R4P, &
                        WRITE_ON_SCRATCH_DATAARRAY6_RANK2_I8P, &
                        WRITE_ON_SCRATCH_DATAARRAY6_RANK2_I4P, &
                        WRITE_ON_SCRATCH_DATAARRAY6_RANK2_I2P, &
                        WRITE_ON_SCRATCH_DATAARRAY6_RANK2_I1P, &
                        WRITE_ON_SCRATCH_DATAARRAY6_RANK3_R8P, &
                        WRITE_ON_SCRATCH_DATAARRAY6_RANK3_R4P, &
                        WRITE_ON_SCRATCH_DATAARRAY6_RANK3_I8P, &
                        WRITE_ON_SCRATCH_DATAARRAY6_RANK3_I4P, &
                        WRITE_ON_SCRATCH_DATAARRAY6_RANK3_I2P, &
                        WRITE_ON_SCRATCH_DATAARRAY6_RANK3_I1P !< Write dataarray.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY1_RANK1     !< Write dataarray, data 1 rank 1.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY1_RANK2     !< Write dataarray, data 1 rank 2.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY1_RANK3     !< Write dataarray, data 1 rank 3.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY1_RANK4     !< Write dataarray, data 1 rank 4.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY3_RANK1_R8P !< Write dataarray, comp 3 rank 1, R8P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY3_RANK1_R4P !< Write dataarray, comp 3 rank 1, R4P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY3_RANK1_I8P !< Write dataarray, comp 3 rank 1, I8P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY3_RANK1_I4P !< Write dataarray, comp 3 rank 1, I4P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY3_RANK1_I2P !< Write dataarray, comp 3 rank 1, I2P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY3_RANK1_I1P !< Write dataarray, comp 3 rank 1, I1P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY3_RANK2_R8P !< Write dataarray, comp 3 rank 2, R8P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY3_RANK2_R4P !< Write dataarray, comp 3 rank 2, R4P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY3_RANK2_I8P !< Write dataarray, comp 3 rank 2, I8P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY3_RANK2_I4P !< Write dataarray, comp 3 rank 2, I4P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY3_RANK2_I2P !< Write dataarray, comp 3 rank 2, I2P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY3_RANK2_I1P !< Write dataarray, comp 3 rank 2, I1P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY3_RANK3_R8P !< Write dataarray, comp 3 rank 3, R8P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY3_RANK3_R4P !< Write dataarray, comp 3 rank 3, R4P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY3_RANK3_I8P !< Write dataarray, comp 3 rank 3, I8P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY3_RANK3_I4P !< Write dataarray, comp 3 rank 3, I4P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY3_RANK3_I2P !< Write dataarray, comp 3 rank 3, I2P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY3_RANK3_I1P !< Write dataarray, comp 3 rank 3, I1P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY6_RANK1_R8P !< Write dataarray, comp 6 rank 1, R8P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY6_RANK1_R4P !< Write dataarray, comp 6 rank 1, R4P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY6_RANK1_I8P !< Write dataarray, comp 6 rank 1, I8P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY6_RANK1_I4P !< Write dataarray, comp 6 rank 1, I4P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY6_RANK1_I2P !< Write dataarray, comp 6 rank 1, I2P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY6_RANK1_I1P !< Write dataarray, comp 6 rank 1, I1P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY6_RANK2_R8P !< Write dataarray, comp 6 rank 2, R8P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY6_RANK2_R4P !< Write dataarray, comp 6 rank 2, R4P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY6_RANK2_I8P !< Write dataarray, comp 6 rank 2, I8P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY6_RANK2_I4P !< Write dataarray, comp 6 rank 2, I4P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY6_RANK2_I2P !< Write dataarray, comp 6 rank 2, I2P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY6_RANK2_I1P !< Write dataarray, comp 6 rank 2, I1P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY6_RANK3_R8P !< Write dataarray, comp 6 rank 3, R8P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY6_RANK3_R4P !< Write dataarray, comp 6 rank 3, R4P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY6_RANK3_I8P !< Write dataarray, comp 6 rank 3, I8P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY6_RANK3_I4P !< Write dataarray, comp 6 rank 3, I4P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY6_RANK3_I2P !< Write dataarray, comp 6 rank 3, I2P.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_ON_SCRATCH_DATAARRAY6_RANK3_I1P !< Write dataarray, comp 6 rank 3, I1P.
ENDTYPE XML_WRITER_APPENDED
CONTAINS
  FUNCTION INITIALIZE(SELF, FORMAT, FILENAME, MESH_TOPOLOGY, NX1, NX2, NY1, NY2, NZ1, NZ2, &
                      IS_VOLATILE, MESH_KIND) RESULT(ERROR)
  !< Initialize writer.
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF          !< Writer.
  CHARACTER(*),               INTENT(IN)           :: FORMAT        !< File format: ASCII.
  CHARACTER(*),               INTENT(IN)           :: FILENAME      !< File name.
  CHARACTER(*),               INTENT(IN)           :: MESH_TOPOLOGY !< Mesh topology.
  INTEGER(I4P),               INTENT(IN), OPTIONAL :: NX1           !< Initial node of x axis.
  INTEGER(I4P),               INTENT(IN), OPTIONAL :: NX2           !< Final node of x axis.
  INTEGER(I4P),               INTENT(IN), OPTIONAL :: NY1           !< Initial node of y axis.
  INTEGER(I4P),               INTENT(IN), OPTIONAL :: NY2           !< Final node of y axis.
  INTEGER(I4P),               INTENT(IN), OPTIONAL :: NZ1           !< Initial node of z axis.
  INTEGER(I4P),               INTENT(IN), OPTIONAL :: NZ2           !< Final node of z axis.
  CHARACTER(*),               INTENT(IN), OPTIONAL :: MESH_KIND     !< Kind of mesh data: Float64, Float32, ecC.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_VOLATILE   !< Flag to check volatile writer.
  INTEGER(I4P)                                     :: ERROR         !< Error status.

  SELF%TOPOLOGY = TRIM(ADJUSTL(MESH_TOPOLOGY))
  SELF%FORMAT_CH = 'appended'
  SELF%ENCODING = FORMAT
  SELF%ENCODING = SELF%ENCODING%UPPER()
  SELECT CASE(SELF%ENCODING%CHARS())
  CASE('RAW')
    SELF%ENCODING = 'raw'
  CASE('BINARY-APPENDED')
    SELF%ENCODING = 'base64'
  ENDSELECT
  CALL SELF%OPEN_XML_FILE(FILENAME=FILENAME)
  CALL SELF%WRITE_HEADER_TAG
  CALL SELF%WRITE_TOPOLOGY_TAG(NX1=NX1, NX2=NX2, NY1=NY1, NY2=NY2, NZ1=NZ1, NZ2=NZ2, MESH_KIND=MESH_KIND)
  SELF%IOFFSET = 0
  CALL SELF%OPEN_SCRATCH_FILE
  ERROR = SELF%ERROR
  ENDFUNCTION INITIALIZE

   FUNCTION FINALIZE(SELF) RESULT(ERROR)
   !< Finalize writer.
   CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF  !< Writer.
   INTEGER(I4P)                              :: ERROR !< Error status.

   CALL SELF%WRITE_END_TAG(NAME=SELF%TOPOLOGY%CHARS())
   CALL SELF%WRITE_DATAARRAY_APPENDED
   CALL SELF%WRITE_END_TAG(NAME='VTKFile')
   CALL SELF%CLOSE_XML_FILE
   CALL SELF%CLOSE_SCRATCH_FILE
   ERROR = SELF%ERROR
   ENDFUNCTION FINALIZE

  ELEMENTAL SUBROUTINE IOFFSET_UPDATE(SELF, N_BYTE)
  !< Update ioffset count.
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF  !< Writer.
  INTEGER(I4P),               INTENT(IN)    :: N_BYTE !< Number of bytes saved.

  IF (SELF%ENCODING=='raw') THEN
    SELF%IOFFSET = SELF%IOFFSET + BYI4P + N_BYTE
  ELSE
    SELF%IOFFSET = SELF%IOFFSET + ((N_BYTE + BYI4P + 2_I4P)/3_I4P)*4_I4P
  ENDIF
  ENDSUBROUTINE IOFFSET_UPDATE

  SUBROUTINE OPEN_SCRATCH_FILE(SELF)
  !< Open scratch file.
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF  !< Writer.

  OPEN(NEWUNIT=SELF%SCRATCH, &
       FORM='UNFORMATTED',   &
       ACCESS='STREAM',      &
       ACTION='READWRITE',   &
       STATUS='SCRATCH',     &
       IOSTAT=SELF%ERROR)
  ENDSUBROUTINE OPEN_SCRATCH_FILE

  SUBROUTINE CLOSE_SCRATCH_FILE(SELF)
  !< Close scratch file.
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF  !< Writer.

  CLOSE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)
  ENDSUBROUTINE CLOSE_SCRATCH_FILE

  ! write_dataarray methods
  FUNCTION WRITE_DATAARRAY1_RANK1_R8P(SELF, DATA_NAME, X, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (R8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  REAL(R8P),                  INTENT(IN)           :: X(1:)        !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Float64'
  N_COMPONENTS = 1
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY1_RANK1_R8P

  FUNCTION WRITE_DATAARRAY1_RANK1_R4P(SELF, DATA_NAME, X, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (R4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  REAL(R4P),                  INTENT(IN)           :: X(1:)        !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Float32'
  N_COMPONENTS = 1
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY1_RANK1_R4P

  FUNCTION WRITE_DATAARRAY1_RANK1_I8P(SELF, DATA_NAME, X, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I8P),               INTENT(IN)           :: X(1:)        !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Int64'
  N_COMPONENTS = 1
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY1_RANK1_I8P

  FUNCTION WRITE_DATAARRAY1_RANK1_I4P(SELF, DATA_NAME, X, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I4P),               INTENT(IN)           :: X(1:)        !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Int32'
  N_COMPONENTS = 1
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY1_RANK1_I4P

  FUNCTION WRITE_DATAARRAY1_RANK1_I2P(SELF, DATA_NAME, X, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I2P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I2P),               INTENT(IN)           :: X(1:)        !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Int16'
  N_COMPONENTS = 1
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY1_RANK1_I2P

  FUNCTION WRITE_DATAARRAY1_RANK1_I1P(SELF, DATA_NAME, X, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I1P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I1P),               INTENT(IN)           :: X(1:)        !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Int8'
  N_COMPONENTS = 1
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY1_RANK1_I1P

  FUNCTION WRITE_DATAARRAY1_RANK2_R8P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF          !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME     !< Data name.
  REAL(R8P),                  INTENT(IN)           :: X(1:,1:)      !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR         !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE     !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS  !< Number of components.

  DATA_TYPE = 'Float64'
  N_COMPONENTS = SIZE(X, DIM=1)
  IF (PRESENT(ONE_COMPONENT)) THEN
    IF (ONE_COMPONENT) N_COMPONENTS = 1
  ENDIF
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY1_RANK2_R8P

  FUNCTION WRITE_DATAARRAY1_RANK2_R4P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF          !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME     !< Data name.
  REAL(R4P),                  INTENT(IN)           :: X(1:,1:)      !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR         !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE     !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS  !< Number of components.

  DATA_TYPE = 'Float32'
  N_COMPONENTS = SIZE(X, DIM=1)
  IF (PRESENT(ONE_COMPONENT)) THEN
    IF (ONE_COMPONENT) N_COMPONENTS = 1
  ENDIF
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY1_RANK2_R4P

  FUNCTION WRITE_DATAARRAY1_RANK2_I8P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF          !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME     !< Data name.
  INTEGER(I8P),               INTENT(IN)           :: X(1:,1:)      !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR         !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE     !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS  !< Number of components.

  DATA_TYPE = 'Int64'
  N_COMPONENTS = SIZE(X, DIM=1)
  IF (PRESENT(ONE_COMPONENT)) THEN
    IF (ONE_COMPONENT) N_COMPONENTS = 1
  ENDIF
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY1_RANK2_I8P

  FUNCTION WRITE_DATAARRAY1_RANK2_I4P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF          !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME     !< Data name.
  INTEGER(I4P),               INTENT(IN)           :: X(1:,1:)      !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR         !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE     !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS  !< Number of components.

  DATA_TYPE = 'Int32'
  N_COMPONENTS = SIZE(X, DIM=1)
  IF (PRESENT(ONE_COMPONENT)) THEN
    IF (ONE_COMPONENT) N_COMPONENTS = 1
  ENDIF
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY1_RANK2_I4P

  FUNCTION WRITE_DATAARRAY1_RANK2_I2P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF          !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME     !< Data name.
  INTEGER(I2P),               INTENT(IN)           :: X(1:,1:)      !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR         !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE     !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS  !< Number of components.

  DATA_TYPE = 'Int16'
  N_COMPONENTS = SIZE(X, DIM=1)
  IF (PRESENT(ONE_COMPONENT)) THEN
    IF (ONE_COMPONENT) N_COMPONENTS = 1
  ENDIF
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY1_RANK2_I2P

  FUNCTION WRITE_DATAARRAY1_RANK2_I1P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF          !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME     !< Data name.
  INTEGER(I1P),               INTENT(IN)           :: X(1:,1:)      !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR         !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE     !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS  !< Number of components.

  DATA_TYPE = 'Int8'
  N_COMPONENTS = SIZE(X, DIM=1)
  IF (PRESENT(ONE_COMPONENT)) THEN
    IF (ONE_COMPONENT) N_COMPONENTS = 1
  ENDIF
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY1_RANK2_I1P

  FUNCTION WRITE_DATAARRAY1_RANK3_R8P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF          !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME     !< Data name.
  REAL(R8P),                  INTENT(IN)           :: X(1:,1:,1:)   !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR         !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE     !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS  !< Number of components.

  DATA_TYPE = 'Float64'
  N_COMPONENTS = SIZE(X, DIM=1)
  IF (PRESENT(ONE_COMPONENT)) THEN
    IF (ONE_COMPONENT) N_COMPONENTS = 1
  ENDIF
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY1_RANK3_R8P

  FUNCTION WRITE_DATAARRAY1_RANK3_R4P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF          !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME     !< Data name.
  REAL(R4P),                  INTENT(IN)           :: X(1:,1:,1:)   !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR         !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE     !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS  !< Number of components.

  DATA_TYPE = 'Float32'
  N_COMPONENTS = SIZE(X, DIM=1)
  IF (PRESENT(ONE_COMPONENT)) THEN
    IF (ONE_COMPONENT) N_COMPONENTS = 1
  ENDIF
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY1_RANK3_R4P

  FUNCTION WRITE_DATAARRAY1_RANK3_I8P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF          !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME     !< Data name.
  INTEGER(I8P),               INTENT(IN)           :: X(1:,1:,1:)   !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR         !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE     !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS  !< Number of components.

  DATA_TYPE = 'Int64'
  N_COMPONENTS = SIZE(X, DIM=1)
  IF (PRESENT(ONE_COMPONENT)) THEN
    IF (ONE_COMPONENT) N_COMPONENTS = 1
  ENDIF
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY1_RANK3_I8P

  FUNCTION WRITE_DATAARRAY1_RANK3_I4P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF          !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME     !< Data name.
  INTEGER(I4P),               INTENT(IN)           :: X(1:,1:,1:)   !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR         !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE     !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS  !< Number of components.

  DATA_TYPE = 'Int32'
  N_COMPONENTS = SIZE(X, DIM=1)
  IF (PRESENT(ONE_COMPONENT)) THEN
    IF (ONE_COMPONENT) N_COMPONENTS = 1
  ENDIF
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY1_RANK3_I4P

  FUNCTION WRITE_DATAARRAY1_RANK3_I2P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF          !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME     !< Data name.
  INTEGER(I2P),               INTENT(IN)           :: X(1:,1:,1:)   !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR         !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE     !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS  !< Number of components.

  DATA_TYPE = 'Int16'
  N_COMPONENTS = SIZE(X, DIM=1)
  IF (PRESENT(ONE_COMPONENT)) THEN
    IF (ONE_COMPONENT) N_COMPONENTS = 1
  ENDIF
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY1_RANK3_I2P

  FUNCTION WRITE_DATAARRAY1_RANK3_I1P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF          !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME     !< Data name.
  INTEGER(I1P),               INTENT(IN)           :: X(1:,1:,1:)   !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR         !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE     !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS  !< Number of components.

  DATA_TYPE = 'Int8'
  N_COMPONENTS = SIZE(X, DIM=1)
  IF (PRESENT(ONE_COMPONENT)) THEN
    IF (ONE_COMPONENT) N_COMPONENTS = 1
  ENDIF
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY1_RANK3_I1P

  FUNCTION WRITE_DATAARRAY1_RANK4_R8P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF           !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME      !< Data name.
  REAL(R8P),                  INTENT(IN)           :: X(1:,1:,1:,1:) !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT  !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES      !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR          !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE      !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS   !< Number of components.

  DATA_TYPE = 'Float64'
  N_COMPONENTS = SIZE(X, DIM=1)
  IF (PRESENT(ONE_COMPONENT)) THEN
    IF (ONE_COMPONENT) N_COMPONENTS = 1
  ENDIF
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY1_RANK4_R8P

  FUNCTION WRITE_DATAARRAY1_RANK4_R4P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF           !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME      !< Data name.
  REAL(R4P),                  INTENT(IN)           :: X(1:,1:,1:,1:) !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT  !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES      !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR          !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE      !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS   !< Number of components.

  DATA_TYPE = 'Float32'
  N_COMPONENTS = SIZE(X, DIM=1)
  IF (PRESENT(ONE_COMPONENT)) THEN
    IF (ONE_COMPONENT) N_COMPONENTS = 1
  ENDIF
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY1_RANK4_R4P

  FUNCTION WRITE_DATAARRAY1_RANK4_I8P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF           !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME      !< Data name.
  INTEGER(I8P),               INTENT(IN)           :: X(1:,1:,1:,1:) !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT  !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES      !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR          !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE      !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS   !< Number of components.

  DATA_TYPE = 'Int64'
  N_COMPONENTS = SIZE(X, DIM=1)
  IF (PRESENT(ONE_COMPONENT)) THEN
    IF (ONE_COMPONENT) N_COMPONENTS = 1
  ENDIF
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY1_RANK4_I8P

  FUNCTION WRITE_DATAARRAY1_RANK4_I4P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF           !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME      !< Data name.
  INTEGER(I4P),               INTENT(IN)           :: X(1:,1:,1:,1:) !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT  !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES      !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR          !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE      !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS   !< Number of components.

  DATA_TYPE = 'Int32'
  N_COMPONENTS = SIZE(X, DIM=1)
  IF (PRESENT(ONE_COMPONENT)) THEN
    IF (ONE_COMPONENT) N_COMPONENTS = 1
  ENDIF
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY1_RANK4_I4P

  FUNCTION WRITE_DATAARRAY1_RANK4_I2P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF           !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME      !< Data name.
  INTEGER(I2P),               INTENT(IN)           :: X(1:,1:,1:,1:) !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT  !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES      !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR          !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE      !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS   !< Number of components.

  DATA_TYPE = 'Int16'
  N_COMPONENTS = SIZE(X, DIM=1)
  IF (PRESENT(ONE_COMPONENT)) THEN
    IF (ONE_COMPONENT) N_COMPONENTS = 1
  ENDIF
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY1_RANK4_I2P

  FUNCTION WRITE_DATAARRAY1_RANK4_I1P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF           !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME      !< Data name.
  INTEGER(I1P),               INTENT(IN)           :: X(1:,1:,1:,1:) !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT  !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES      !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR          !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE      !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS   !< Number of components.

  DATA_TYPE = 'Int8'
  N_COMPONENTS = SIZE(X, DIM=1)
  IF (PRESENT(ONE_COMPONENT)) THEN
    IF (ONE_COMPONENT) N_COMPONENTS = 1
  ENDIF
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY1_RANK4_I1P

  FUNCTION WRITE_DATAARRAY3_RANK1_R8P(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  REAL(R8P),                  INTENT(IN)           :: X(1:)        !< X component of data variable.
  REAL(R8P),                  INTENT(IN)           :: Y(1:)        !< Y component of data variable.
  REAL(R8P),                  INTENT(IN)           :: Z(1:)        !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead "NumberOfCoMPONENTS" ATTRIBUTE.
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Float64'
  N_COMPONENTS = 3
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X, Y=Y, Z=Z))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY3_RANK1_R8P

  FUNCTION WRITE_DATAARRAY3_RANK1_R4P(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  REAL(R4P),                  INTENT(IN)           :: X(1:)        !< X component of data variable.
  REAL(R4P),                  INTENT(IN)           :: Y(1:)        !< Y component of data variable.
  REAL(R4P),                  INTENT(IN)           :: Z(1:)        !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Float32'
  N_COMPONENTS = 3
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X, Y=Y, Z=Z))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY3_RANK1_R4P

  FUNCTION WRITE_DATAARRAY3_RANK1_I8P(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I8P),               INTENT(IN)           :: X(1:)        !< X component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: Y(1:)        !< Y component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: Z(1:)        !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Int64'
  N_COMPONENTS = 3
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X, Y=Y, Z=Z))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY3_RANK1_I8P

  FUNCTION WRITE_DATAARRAY3_RANK1_I4P(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I4P),               INTENT(IN)           :: X(1:)        !< X component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: Y(1:)        !< Y component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: Z(1:)        !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Int32'
  N_COMPONENTS = 3
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X, Y=Y, Z=Z))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY3_RANK1_I4P

  FUNCTION WRITE_DATAARRAY3_RANK1_I2P(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I2P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I2P),               INTENT(IN)           :: X(1:)        !< X component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: Y(1:)        !< Y component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: Z(1:)        !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Int16'
  N_COMPONENTS = 3
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X, Y=Y, Z=Z))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY3_RANK1_I2P

  FUNCTION WRITE_DATAARRAY3_RANK1_I1P(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I1P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I1P),               INTENT(IN)           :: X(1:)        !< X component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: Y(1:)        !< Y component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: Z(1:)        !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Int8'
  N_COMPONENTS = 3
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X, Y=Y, Z=Z))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY3_RANK1_I1P

  FUNCTION WRITE_DATAARRAY3_RANK3_R8P(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  REAL(R8P),                  INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
  REAL(R8P),                  INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
  REAL(R8P),                  INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Float64'
  N_COMPONENTS = 3
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X, Y=Y, Z=Z))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY3_RANK3_R8P

  FUNCTION WRITE_DATAARRAY3_RANK3_R4P(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  REAL(R4P),                  INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
  REAL(R4P),                  INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
  REAL(R4P),                  INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Float32'
  N_COMPONENTS = 3
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X, Y=Y, Z=Z))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY3_RANK3_R4P

  FUNCTION WRITE_DATAARRAY3_RANK3_I8P(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I8P),               INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Int64'
  N_COMPONENTS = 3
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X, Y=Y, Z=Z))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY3_RANK3_I8P

  FUNCTION WRITE_DATAARRAY3_RANK3_I4P(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I4P),               INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Int32'
  N_COMPONENTS = 3
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X, Y=Y, Z=Z))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY3_RANK3_I4P

  FUNCTION WRITE_DATAARRAY3_RANK3_I2P(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I2P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I2P),               INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Int16'
  N_COMPONENTS = 3
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X, Y=Y, Z=Z))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY3_RANK3_I2P

  FUNCTION WRITE_DATAARRAY3_RANK3_I1P(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I1P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I1P),               INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Int8'
  N_COMPONENTS = 3
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(X=X, Y=Y, Z=Z))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY3_RANK3_I1P

  FUNCTION WRITE_DATAARRAY6_RANK1_R8P(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  REAL(R8P),                  INTENT(IN)           :: U(1:)        !< U component of data variable.
  REAL(R8P),                  INTENT(IN)           :: V(1:)        !< V component of data variable.
  REAL(R8P),                  INTENT(IN)           :: W(1:)        !< W component of data variable.
  REAL(R8P),                  INTENT(IN)           :: X(1:)        !< X component of data variable.
  REAL(R8P),                  INTENT(IN)           :: Y(1:)        !< Y component of data variable.
  REAL(R8P),                  INTENT(IN)           :: Z(1:)        !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead "NumberOfCoMPONENTS" ATTRIBUTE.
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Float64'
  N_COMPONENTS = 6
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(U=U, V=V, W=W, X=X, Y=Y, Z=Z))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY6_RANK1_R8P

  FUNCTION WRITE_DATAARRAY6_RANK1_R4P(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  REAL(R4P),                  INTENT(IN)           :: U(1:)        !< U component of data variable.
  REAL(R4P),                  INTENT(IN)           :: V(1:)        !< V component of data variable.
  REAL(R4P),                  INTENT(IN)           :: W(1:)        !< W component of data variable.
  REAL(R4P),                  INTENT(IN)           :: X(1:)        !< X component of data variable.
  REAL(R4P),                  INTENT(IN)           :: Y(1:)        !< Y component of data variable.
  REAL(R4P),                  INTENT(IN)           :: Z(1:)        !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Float32'
  N_COMPONENTS = 6
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(U=U, V=V, W=W, X=X, Y=Y, Z=Z))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY6_RANK1_R4P

  FUNCTION WRITE_DATAARRAY6_RANK1_I8P(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I8P),               INTENT(IN)           :: U(1:)        !< U component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: V(1:)        !< V component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: W(1:)        !< W component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: X(1:)        !< X component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: Y(1:)        !< Y component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: Z(1:)        !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Int64'
  N_COMPONENTS = 6
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(U=U, V=V, W=W, X=X, Y=Y, Z=Z))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY6_RANK1_I8P

  FUNCTION WRITE_DATAARRAY6_RANK1_I4P(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I4P),               INTENT(IN)           :: U(1:)        !< U component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: V(1:)        !< V component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: W(1:)        !< W component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: X(1:)        !< X component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: Y(1:)        !< Y component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: Z(1:)        !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Int32'
  N_COMPONENTS = 6
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(U=U, V=V, W=W, X=X, Y=Y, Z=Z))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY6_RANK1_I4P

  FUNCTION WRITE_DATAARRAY6_RANK1_I2P(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I2P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I2P),               INTENT(IN)           :: U(1:)        !< U component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: V(1:)        !< V component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: W(1:)        !< W component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: X(1:)        !< X component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: Y(1:)        !< Y component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: Z(1:)        !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Int16'
  N_COMPONENTS = 6
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(U=U, V=V, W=W, X=X, Y=Y, Z=Z))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY6_RANK1_I2P

  FUNCTION WRITE_DATAARRAY6_RANK1_I1P(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I1P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I1P),               INTENT(IN)           :: U(1:)        !< U component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: V(1:)        !< V component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: W(1:)        !< W component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: X(1:)        !< X component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: Y(1:)        !< Y component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: Z(1:)        !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Int8'
  N_COMPONENTS = 6
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(U=U, V=V, W=W, X=X, Y=Y, Z=Z))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY6_RANK1_I1P

  FUNCTION WRITE_DATAARRAY6_RANK3_R8P(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (R8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  REAL(R8P),                  INTENT(IN)           :: U(1:,1:,1:)  !< U component of data variable.
  REAL(R8P),                  INTENT(IN)           :: V(1:,1:,1:)  !< V component of data variable.
  REAL(R8P),                  INTENT(IN)           :: W(1:,1:,1:)  !< W component of data variable.
  REAL(R8P),                  INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
  REAL(R8P),                  INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
  REAL(R8P),                  INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Float64'
  N_COMPONENTS = 6
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(U=U, V=V, W=W, X=X, Y=Y, Z=Z))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY6_RANK3_R8P

  FUNCTION WRITE_DATAARRAY6_RANK3_R4P(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (R4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  REAL(R4P),                  INTENT(IN)           :: U(1:,1:,1:)  !< U component of data variable.
  REAL(R4P),                  INTENT(IN)           :: V(1:,1:,1:)  !< V component of data variable.
  REAL(R4P),                  INTENT(IN)           :: W(1:,1:,1:)  !< W component of data variable.
  REAL(R4P),                  INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
  REAL(R4P),                  INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
  REAL(R4P),                  INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Float32'
  N_COMPONENTS = 6
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(U=U, V=V, W=W, X=X, Y=Y, Z=Z))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY6_RANK3_R4P

  FUNCTION WRITE_DATAARRAY6_RANK3_I8P(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I8P),               INTENT(IN)           :: U(1:,1:,1:)  !< U component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: V(1:,1:,1:)  !< V component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: W(1:,1:,1:)  !< W component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Int64'
  N_COMPONENTS = 6
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(U=U, V=V, W=W, X=X, Y=Y, Z=Z))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY6_RANK3_I8P

  FUNCTION WRITE_DATAARRAY6_RANK3_I4P(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I4P),               INTENT(IN)           :: U(1:,1:,1:)  !< U component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: V(1:,1:,1:)  !< V component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: W(1:,1:,1:)  !< W component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Int32'
  N_COMPONENTS = 6
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(U=U, V=V, W=W, X=X, Y=Y, Z=Z))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY6_RANK3_I4P

  FUNCTION WRITE_DATAARRAY6_RANK3_I2P(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I2P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I2P),               INTENT(IN)           :: U(1:,1:,1:)  !< U component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: V(1:,1:,1:)  !< V component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: W(1:,1:,1:)  !< W component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Int16'
  N_COMPONENTS = 6
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(U=U, V=V, W=W, X=X, Y=Y, Z=Z))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY6_RANK3_I2P

  FUNCTION WRITE_DATAARRAY6_RANK3_I1P(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I1P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I1P),               INTENT(IN)           :: U(1:,1:,1:)  !< U component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: V(1:,1:,1:)  !< V component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: W(1:,1:,1:)  !< W component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  CHARACTER(LEN=:), ALLOCATABLE                    :: DATA_TYPE    !< Data type.
  INTEGER(I4P)                                     :: N_COMPONENTS !< Number of components.

  DATA_TYPE = 'Int8'
  N_COMPONENTS = 6
  CALL SELF%WRITE_DATAARRAY_TAG_APPENDED(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, &
                                         IS_TUPLES=IS_TUPLES)
  CALL SELF%IOFFSET_UPDATE(N_BYTE=SELF%WRITE_ON_SCRATCH_DATAARRAY(U=U, V=V, W=W, X=X, Y=Y, Z=Z))
  ERROR = SELF%ERROR
  ENDFUNCTION WRITE_DATAARRAY6_RANK3_I1P

  SUBROUTINE WRITE_DATAARRAY_APPENDED(SELF)
  !< Do nothing, ascii data cannot be appended.
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF              !< Writer.
  TYPE(STRING)                              :: TAG_ATTRIBUTES    !< Tag attributes.
  INTEGER(I4P)                              :: N_BYTE            !< Bytes count.
  CHARACTER(LEN=2)                          :: DATAARRAY_TYPE    !< Dataarray type = R8,R4,I8,I4,I2,I1.
  INTEGER(I4P)                              :: DATAARRAY_DIM     !< Dataarray dimension.
  REAL(R8P),    ALLOCATABLE                 :: DATAARRAY_R8P(:)  !< Dataarray buffer of R8P.
  REAL(R4P),    ALLOCATABLE                 :: DATAARRAY_R4P(:)  !< Dataarray buffer of R4P.
  INTEGER(I8P), ALLOCATABLE                 :: DATAARRAY_I8P(:)  !< Dataarray buffer of I8P.
  INTEGER(I4P), ALLOCATABLE                 :: DATAARRAY_I4P(:)  !< Dataarray buffer of I4P.
  INTEGER(I2P), ALLOCATABLE                 :: DATAARRAY_I2P(:)  !< Dataarray buffer of I2P.
  INTEGER(I1P), ALLOCATABLE                 :: DATAARRAY_I1P(:)  !< Dataarray buffer of I1P.

  CALL SELF%WRITE_START_TAG(NAME='AppendedData', ATTRIBUTES='encoding="'//SELF%ENCODING%CHARS()//'"')
  WRITE(UNIT=SELF%XML, IOSTAT=SELF%ERROR)'_'
  ENDFILE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)
  REWIND(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)
  DO
    CALL READ_ARRAY_FROM_SCRATCH
    IF (SELF%ERROR==0) CALL WRITE_DATAARRAY_ON_XML
    IF (IS_IOSTAT_END(SELF%ERROR)) EXIT
  ENDDO
  CLOSE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)
  WRITE(UNIT=SELF%XML, IOSTAT=SELF%ERROR)END_REC
  CALL SELF%WRITE_END_TAG(NAME='AppendedData')
  CONTAINS
    SUBROUTINE READ_ARRAY_FROM_SCRATCH
    !< Read the current dataaray from scratch file.

    READ(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR, END=10)N_BYTE, DATAARRAY_TYPE, DATAARRAY_DIM
    SELECT CASE(DATAARRAY_TYPE)
    CASE('R8')
      IF (ALLOCATED(DATAARRAY_R8P)) DEALLOCATE(DATAARRAY_R8P) ; ALLOCATE(DATAARRAY_R8P(1:DATAARRAY_DIM))
      READ(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)DATAARRAY_R8P
    CASE('R4')
      IF (ALLOCATED(DATAARRAY_R4P)) DEALLOCATE(DATAARRAY_R4P) ; ALLOCATE(DATAARRAY_R4P(1:DATAARRAY_DIM))
      READ(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)DATAARRAY_R4P
    CASE('I8')
      IF (ALLOCATED(DATAARRAY_I8P)) DEALLOCATE(DATAARRAY_I8P) ; ALLOCATE(DATAARRAY_I8P(1:DATAARRAY_DIM))
      READ(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)DATAARRAY_I8P
    CASE('I4')
      IF (ALLOCATED(DATAARRAY_I4P)) DEALLOCATE(DATAARRAY_I4P) ; ALLOCATE(DATAARRAY_I4P(1:DATAARRAY_DIM))
      READ(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)DATAARRAY_I4P
    CASE('I2')
      IF (ALLOCATED(DATAARRAY_I2P)) DEALLOCATE(DATAARRAY_I2P) ; ALLOCATE(DATAARRAY_I2P(1:DATAARRAY_DIM))
      READ(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)DATAARRAY_I2P
    CASE('I1')
      IF (ALLOCATED(DATAARRAY_I1P)) DEALLOCATE(DATAARRAY_I1P) ; ALLOCATE(DATAARRAY_I1P(1:DATAARRAY_DIM))
      READ(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)DATAARRAY_I1P
    CASE DEFAULT
      SELF%ERROR = 1
      WRITE (STDERR,'(A)')' error: bad dataarray_type = '//DATAARRAY_TYPE
      WRITE (STDERR,'(A)')' bytes = '//TRIM(STR(N=N_BYTE))
      WRITE (STDERR,'(A)')' dataarray dimension = '//TRIM(STR(N=DATAARRAY_DIM))
    ENDSELECT
    10 RETURN
    ENDSUBROUTINE READ_ARRAY_FROM_SCRATCH

    SUBROUTINE WRITE_DATAARRAY_ON_XML
    !< Write the current dataaray on xml file.
    CHARACTER(LEN=:), ALLOCATABLE  :: CODE !< Dataarray encoded with Base64 codec.

    IF (SELF%ENCODING=='raw') THEN
      SELECT CASE(DATAARRAY_TYPE)
      CASE('R8')
        WRITE(UNIT=SELF%XML, IOSTAT=SELF%ERROR)N_BYTE, DATAARRAY_R8P
        DEALLOCATE(DATAARRAY_R8P)
      CASE('R4')
        WRITE(UNIT=SELF%XML, IOSTAT=SELF%ERROR)N_BYTE, DATAARRAY_R4P
        DEALLOCATE(DATAARRAY_R4P)
      CASE('I8')
        WRITE(UNIT=SELF%XML, IOSTAT=SELF%ERROR)N_BYTE, DATAARRAY_I8P
        DEALLOCATE(DATAARRAY_I8P)
      CASE('I4')
        WRITE(UNIT=SELF%XML, IOSTAT=SELF%ERROR)N_BYTE, DATAARRAY_I4P
        DEALLOCATE(DATAARRAY_I4P)
      CASE('I2')
        WRITE(UNIT=SELF%XML, IOSTAT=SELF%ERROR)N_BYTE, DATAARRAY_I2P
        DEALLOCATE(DATAARRAY_I2P)
      CASE('I1')
        WRITE(UNIT=SELF%XML, IOSTAT=SELF%ERROR)N_BYTE, DATAARRAY_I1P
        DEALLOCATE(DATAARRAY_I1P)
      ENDSELECT
    ELSE
      SELECT CASE(DATAARRAY_TYPE)
      CASE('R8')
        CODE = ENCODE_BINARY_DATAARRAY(X=DATAARRAY_R8P)
        WRITE(UNIT=SELF%XML, IOSTAT=SELF%ERROR)CODE
      CASE('R4')
        CODE = ENCODE_BINARY_DATAARRAY(X=DATAARRAY_R4P)
        WRITE(UNIT=SELF%XML, IOSTAT=SELF%ERROR)CODE
      CASE('I8')
        CODE = ENCODE_BINARY_DATAARRAY(X=DATAARRAY_I8P)
        WRITE(UNIT=SELF%XML, IOSTAT=SELF%ERROR)CODE
      CASE('I4')
        CODE = ENCODE_BINARY_DATAARRAY(X=DATAARRAY_I4P)
        WRITE(UNIT=SELF%XML, IOSTAT=SELF%ERROR)CODE
      CASE('I2')
        CODE = ENCODE_BINARY_DATAARRAY(X=DATAARRAY_I2P)
        WRITE(UNIT=SELF%XML, IOSTAT=SELF%ERROR)CODE
      CASE('I1')
        CODE = ENCODE_BINARY_DATAARRAY(X=DATAARRAY_I1P)
        WRITE(UNIT=SELF%XML, IOSTAT=SELF%ERROR)CODE
      ENDSELECT
    ENDIF
    ENDSUBROUTINE WRITE_DATAARRAY_ON_XML
  ENDSUBROUTINE WRITE_DATAARRAY_APPENDED

  ! write_on_scratch_dataarray methods
  FUNCTION WRITE_ON_SCRATCH_DATAARRAY1_RANK1(SELF, X) RESULT(N_BYTE)
  !< Write a dataarray with 1 components of rank 1.
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF   !< Writer.
  CLASS(*),                   INTENT(IN)    :: X(1:)  !< Data variable.
  INTEGER(I4P)                              :: N_BYTE !< Number of bytes
  INTEGER(I4P)                              :: NN     !< Number of elements.

  NN = SIZE(X, DIM=1)
  SELECT TYPE(X)
  TYPE IS(REAL(R8P))
    N_BYTE = NN*BYR8P
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)N_BYTE, 'R8', NN
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)X
  TYPE IS(REAL(R4P))
    N_BYTE = NN*BYR4P
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)N_BYTE, 'R4', NN
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)X
  TYPE IS(INTEGER(I8P))
    N_BYTE = NN*BYI8P
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)N_BYTE, 'I8', NN
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)X
  TYPE IS(INTEGER(I4P))
    N_BYTE = NN*BYI4P
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)N_BYTE, 'I4', NN
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)X
  TYPE IS(INTEGER(I2P))
    N_BYTE = NN*BYI2P
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)N_BYTE, 'I2', NN
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)X
  TYPE IS(INTEGER(I1P))
    N_BYTE = NN*BYI1P
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)N_BYTE, 'I1', NN
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)X
  ENDSELECT
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY1_RANK1

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY1_RANK2(SELF, X) RESULT(N_BYTE)
  !< Write a dataarray with 1 components of rank 2.
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF     !< Writer.
  CLASS(*),                   INTENT(IN)    :: X(1:,1:) !< Data variable.
  INTEGER(I4P)                              :: N_BYTE   !< Number of bytes
  INTEGER(I4P)                              :: NN       !< Number of elements.

  NN = SIZE(X, DIM=1)*SIZE(X, DIM=2)
  SELECT TYPE(X)
  TYPE IS(REAL(R8P))
    N_BYTE = NN*BYR8P
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)N_BYTE, 'R8', NN
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)X
  TYPE IS(REAL(R4P))
    N_BYTE = NN*BYR4P
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)N_BYTE, 'R4', NN
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)X
  TYPE IS(INTEGER(I8P))
    N_BYTE = NN*BYI8P
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)N_BYTE, 'I8', NN
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)X
  TYPE IS(INTEGER(I4P))
    N_BYTE = NN*BYI4P
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)N_BYTE, 'I4', NN
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)X
  TYPE IS(INTEGER(I2P))
    N_BYTE = NN*BYI2P
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)N_BYTE, 'I2', NN
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)X
  TYPE IS(INTEGER(I1P))
    N_BYTE = NN*BYI1P
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)N_BYTE, 'I1', NN
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)X
  ENDSELECT
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY1_RANK2

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY1_RANK3(SELF, X) RESULT(N_BYTE)
  !< Write a dataarray with 1 components of rank 3.
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF        !< Writer.
  CLASS(*),                   INTENT(IN)    :: X(1:,1:,1:) !< Data variable.
  INTEGER(I4P)                              :: N_BYTE      !< Number of bytes
  INTEGER(I4P)                              :: NN          !< Number of elements.

  NN = SIZE(X, DIM=1)*SIZE(X, DIM=2)*SIZE(X, DIM=3)
  SELECT TYPE(X)
  TYPE IS(REAL(R8P))
    N_BYTE = NN*BYR8P
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)N_BYTE, 'R8', NN
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)X
  TYPE IS(REAL(R4P))
    N_BYTE = NN*BYR4P
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)N_BYTE, 'R4', NN
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)X
  TYPE IS(INTEGER(I8P))
    N_BYTE = NN*BYI8P
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)N_BYTE, 'I8', NN
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)X
  TYPE IS(INTEGER(I4P))
    N_BYTE = NN*BYI4P
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)N_BYTE, 'I4', NN
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)X
  TYPE IS(INTEGER(I2P))
    N_BYTE = NN*BYI2P
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)N_BYTE, 'I2', NN
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)X
  TYPE IS(INTEGER(I1P))
    N_BYTE = NN*BYI1P
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)N_BYTE, 'I1', NN
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)X
  ENDSELECT
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY1_RANK3

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY1_RANK4(SELF, X) RESULT(N_BYTE)
  !< Write a dataarray with 1 components of rank 4.
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF           !< Writer.
  CLASS(*),                   INTENT(IN)    :: X(1:,1:,1:,1:) !< Data variable.
  INTEGER(I4P)                              :: N_BYTE         !< Number of bytes
  INTEGER(I4P)                              :: NN             !< Number of elements.

  NN = SIZE(X, DIM=1)*SIZE(X, DIM=2)*SIZE(X, DIM=3)*SIZE(X, DIM=4)
  SELECT TYPE(X)
  TYPE IS(REAL(R8P))
    N_BYTE = NN*BYR8P
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)N_BYTE, 'R8', NN
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)X
  TYPE IS(REAL(R4P))
    N_BYTE = NN*BYR4P
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)N_BYTE, 'R4', NN
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)X
  TYPE IS(INTEGER(I8P))
    N_BYTE = NN*BYI8P
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)N_BYTE, 'I8', NN
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)X
  TYPE IS(INTEGER(I4P))
    N_BYTE = NN*BYI4P
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)N_BYTE, 'I4', NN
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)X
  TYPE IS(INTEGER(I2P))
    N_BYTE = NN*BYI2P
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)N_BYTE, 'I2', NN
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)X
  TYPE IS(INTEGER(I1P))
    N_BYTE = NN*BYI1P
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)N_BYTE, 'I1', NN
    WRITE(UNIT=SELF%SCRATCH, IOSTAT=SELF%ERROR)X
  ENDSELECT
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY1_RANK4

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK1_R8P(SELF, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 3 components of rank 1 (R8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF   !< Writer.
  REAL(R8P),                  INTENT(IN)    :: X(1:)  !< X component.
  REAL(R8P),                  INTENT(IN)    :: Y(1:)  !< Y component.
  REAL(R8P),                  INTENT(IN)    :: Z(1:)  !< Z component.
  INTEGER(I4P)                              :: N_BYTE !< Number of bytes
  INTEGER(I4P)                              :: N      !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[(X(N), Y(N), Z(N), N=1,SIZE(X, DIM=1))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK1_R8P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK1_R4P(SELF, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 3 components of rank 1 (R4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF   !< Writer.
  REAL(R4P),                  INTENT(IN)    :: X(1:)  !< X component.
  REAL(R4P),                  INTENT(IN)    :: Y(1:)  !< Y component.
  REAL(R4P),                  INTENT(IN)    :: Z(1:)  !< Z component.
  INTEGER(I4P)                              :: N_BYTE !< Number of bytes
  INTEGER(I4P)                              :: N      !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[(X(N), Y(N), Z(N), N=1,SIZE(X, DIM=1))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK1_R4P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK1_I8P(SELF, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 3 components of rank 1 (I8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF   !< Writer.
  INTEGER(I8P),               INTENT(IN)    :: X(1:)  !< X component.
  INTEGER(I8P),               INTENT(IN)    :: Y(1:)  !< Y component.
  INTEGER(I8P),               INTENT(IN)    :: Z(1:)  !< Z component.
  INTEGER(I4P)                              :: N_BYTE !< Number of bytes
  INTEGER(I4P)                              :: N      !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[(X(N), Y(N), Z(N), N=1,SIZE(X, DIM=1))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK1_I8P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK1_I4P(SELF, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 3 components of rank 1 (I4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF   !< Writer.
  INTEGER(I4P),               INTENT(IN)    :: X(1:)  !< X component.
  INTEGER(I4P),               INTENT(IN)    :: Y(1:)  !< Y component.
  INTEGER(I4P),               INTENT(IN)    :: Z(1:)  !< Z component.
  INTEGER(I4P)                              :: N_BYTE !< Number of bytes
  INTEGER(I4P)                              :: N      !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[(X(N), Y(N), Z(N), N=1,SIZE(X, DIM=1))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK1_I4P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK1_I2P(SELF, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 3 components of rank 1 (I2P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF   !< Writer.
  INTEGER(I2P),               INTENT(IN)    :: X(1:)  !< X component.
  INTEGER(I2P),               INTENT(IN)    :: Y(1:)  !< Y component.
  INTEGER(I2P),               INTENT(IN)    :: Z(1:)  !< Z component.
  INTEGER(I4P)                              :: N_BYTE !< Number of bytes
  INTEGER(I4P)                              :: N      !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[(X(N), Y(N), Z(N), N=1,SIZE(X, DIM=1))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK1_I2P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK1_I1P(SELF, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 3 components of rank 1 (I1P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF   !< Writer.
  INTEGER(I1P),               INTENT(IN)    :: X(1:)  !< X component.
  INTEGER(I1P),               INTENT(IN)    :: Y(1:)  !< Y component.
  INTEGER(I1P),               INTENT(IN)    :: Z(1:)  !< Z component.
  INTEGER(I4P)                              :: N_BYTE !< Number of bytes
  INTEGER(I4P)                              :: N      !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[(X(N), Y(N), Z(N), N=1,SIZE(X, DIM=1))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK1_I1P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK2_R8P(SELF, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 3 components of rank 2 (R8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF     !< Writer.
  REAL(R8P),                  INTENT(IN)    :: X(1:,1:) !< X component.
  REAL(R8P),                  INTENT(IN)    :: Y(1:,1:) !< Y component.
  REAL(R8P),                  INTENT(IN)    :: Z(1:,1:) !< Z component.
  INTEGER(I4P)                              :: N_BYTE   !< Number of bytes
  INTEGER(I4P)                              :: N1       !< Counter.
  INTEGER(I4P)                              :: N2       !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[((X(N1,N2), Y(N1,N2), Z(N1,N2), N1=1,SIZE(X, DIM=1)),N2=1,SIZE(X, DIM=2))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK2_R8P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK2_R4P(SELF, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 3 components of rank 2 (R4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF     !< Writer.
  REAL(R4P),                  INTENT(IN)    :: X(1:,1:) !< X component.
  REAL(R4P),                  INTENT(IN)    :: Y(1:,1:) !< Y component.
  REAL(R4P),                  INTENT(IN)    :: Z(1:,1:) !< Z component.
  INTEGER(I4P)                              :: N_BYTE   !< Number of bytes
  INTEGER(I4P)                              :: N1       !< Counter.
  INTEGER(I4P)                              :: N2       !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[((X(N1,N2), Y(N1,N2), Z(N1,N2), N1=1,SIZE(X, DIM=1)),N2=1,SIZE(X, DIM=2))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK2_R4P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK2_I8P(SELF, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 3 components of rank 2 (I8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF     !< Writer.
  INTEGER(I8P),               INTENT(IN)    :: X(1:,1:) !< X component.
  INTEGER(I8P),               INTENT(IN)    :: Y(1:,1:) !< Y component.
  INTEGER(I8P),               INTENT(IN)    :: Z(1:,1:) !< Z component.
  INTEGER(I4P)                              :: N_BYTE   !< Number of bytes
  INTEGER(I4P)                              :: N1       !< Counter.
  INTEGER(I4P)                              :: N2       !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[((X(N1,N2), Y(N1,N2), Z(N1,N2), N1=1,SIZE(X, DIM=1)),N2=1,SIZE(X, DIM=2))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK2_I8P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK2_I4P(SELF, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 3 components of rank 2 (I4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF     !< Writer.
  INTEGER(I4P),               INTENT(IN)    :: X(1:,1:) !< X component.
  INTEGER(I4P),               INTENT(IN)    :: Y(1:,1:) !< Y component.
  INTEGER(I4P),               INTENT(IN)    :: Z(1:,1:) !< Z component.
  INTEGER(I4P)                              :: N_BYTE   !< Number of bytes
  INTEGER(I4P)                              :: N1       !< Counter.
  INTEGER(I4P)                              :: N2       !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[((X(N1,N2), Y(N1,N2), Z(N1,N2), N1=1,SIZE(X, DIM=1)),N2=1,SIZE(X, DIM=2))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK2_I4P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK2_I2P(SELF, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 3 components of rank 2 (I2P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF     !< Writer.
  INTEGER(I2P),               INTENT(IN)    :: X(1:,1:) !< X component.
  INTEGER(I2P),               INTENT(IN)    :: Y(1:,1:) !< Y component.
  INTEGER(I2P),               INTENT(IN)    :: Z(1:,1:) !< Z component.
  INTEGER(I4P)                              :: N_BYTE   !< Number of bytes
  INTEGER(I4P)                              :: N1       !< Counter.
  INTEGER(I4P)                              :: N2       !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[((X(N1,N2), Y(N1,N2), Z(N1,N2), N1=1,SIZE(X, DIM=1)),N2=1,SIZE(X, DIM=2))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK2_I2P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK2_I1P(SELF, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 3 components of rank 2 (I1P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF     !< Writer.
  INTEGER(I1P),               INTENT(IN)    :: X(1:,1:) !< X component.
  INTEGER(I1P),               INTENT(IN)    :: Y(1:,1:) !< Y component.
  INTEGER(I1P),               INTENT(IN)    :: Z(1:,1:) !< Z component.
  INTEGER(I4P)                              :: N_BYTE   !< Number of bytes
  INTEGER(I4P)                              :: N1       !< Counter.
  INTEGER(I4P)                              :: N2       !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[((X(N1,N2), Y(N1,N2), Z(N1,N2), N1=1,SIZE(X, DIM=1)),N2=1,SIZE(X, DIM=2))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK2_I1P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK3_R8P(SELF, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 3 components of rank 3 (R8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF        !< Writer.
  REAL(R8P),                  INTENT(IN)    :: X(1:,1:,1:) !< X component.
  REAL(R8P),                  INTENT(IN)    :: Y(1:,1:,1:) !< Y component.
  REAL(R8P),                  INTENT(IN)    :: Z(1:,1:,1:) !< Z component.
  INTEGER(I4P)                              :: N_BYTE      !< Number of bytes
  INTEGER(I4P)                              :: N1          !< Counter.
  INTEGER(I4P)                              :: N2          !< Counter.
  INTEGER(I4P)                              :: N3          !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[(((X(N1,N2,N3), Y(N1,N2,N3), Z(N1,N2,N3), &
                                           N1=1,SIZE(X, DIM=1)),N2=1,SIZE(X, DIM=2)),N3=1,SIZE(X, DIM=3))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK3_R8P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK3_R4P(SELF, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 3 components of rank 3 (R4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF        !< Writer.
  REAL(R4P),                  INTENT(IN)    :: X(1:,1:,1:) !< X component.
  REAL(R4P),                  INTENT(IN)    :: Y(1:,1:,1:) !< Y component.
  REAL(R4P),                  INTENT(IN)    :: Z(1:,1:,1:) !< Z component.
  INTEGER(I4P)                              :: N_BYTE      !< Number of bytes
  INTEGER(I4P)                              :: N1          !< Counter.
  INTEGER(I4P)                              :: N2          !< Counter.
  INTEGER(I4P)                              :: N3          !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[(((X(N1,N2,N3), Y(N1,N2,N3), Z(N1,N2,N3), &
                                           N1=1,SIZE(X, DIM=1)),N2=1,SIZE(X, DIM=2)),N3=1,SIZE(X, DIM=3))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK3_R4P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK3_I8P(SELF, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 3 components of rank 3 (I8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF        !< Writer.
  INTEGER(I8P),               INTENT(IN)    :: X(1:,1:,1:) !< X component.
  INTEGER(I8P),               INTENT(IN)    :: Y(1:,1:,1:) !< Y component.
  INTEGER(I8P),               INTENT(IN)    :: Z(1:,1:,1:) !< Z component.
  INTEGER(I4P)                              :: N_BYTE      !< Number of bytes
  INTEGER(I4P)                              :: N1          !< Counter.
  INTEGER(I4P)                              :: N2          !< Counter.
  INTEGER(I4P)                              :: N3          !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[(((X(N1,N2,N3), Y(N1,N2,N3), Z(N1,N2,N3), &
                                           N1=1,SIZE(X, DIM=1)),N2=1,SIZE(X, DIM=2)),N3=1,SIZE(X, DIM=3))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK3_I8P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK3_I4P(SELF, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 3 components of rank 3 (I4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF        !< Writer.
  INTEGER(I4P),               INTENT(IN)    :: X(1:,1:,1:) !< X component.
  INTEGER(I4P),               INTENT(IN)    :: Y(1:,1:,1:) !< Y component.
  INTEGER(I4P),               INTENT(IN)    :: Z(1:,1:,1:) !< Z component.
  INTEGER(I4P)                              :: N_BYTE      !< Number of bytes
  INTEGER(I4P)                              :: N1          !< Counter.
  INTEGER(I4P)                              :: N2          !< Counter.
  INTEGER(I4P)                              :: N3          !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[(((X(N1,N2,N3), Y(N1,N2,N3), Z(N1,N2,N3), &
                                           N1=1,SIZE(X, DIM=1)),N2=1,SIZE(X, DIM=2)),N3=1,SIZE(X, DIM=3))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK3_I4P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK3_I2P(SELF, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 3 components of rank 3 (I2P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF        !< Writer.
  INTEGER(I2P),               INTENT(IN)    :: X(1:,1:,1:) !< X component.
  INTEGER(I2P),               INTENT(IN)    :: Y(1:,1:,1:) !< Y component.
  INTEGER(I2P),               INTENT(IN)    :: Z(1:,1:,1:) !< Z component.
  INTEGER(I4P)                              :: N_BYTE      !< Number of bytes
  INTEGER(I4P)                              :: N1          !< Counter.
  INTEGER(I4P)                              :: N2          !< Counter.
  INTEGER(I4P)                              :: N3          !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[(((X(N1,N2,N3), Y(N1,N2,N3), Z(N1,N2,N3), &
                                           N1=1,SIZE(X, DIM=1)),N2=1,SIZE(X, DIM=2)),N3=1,SIZE(X, DIM=3))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK3_I2P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK3_I1P(SELF, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 3 components of rank 3 (I1P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF        !< Writer.
  INTEGER(I1P),               INTENT(IN)    :: X(1:,1:,1:) !< X component.
  INTEGER(I1P),               INTENT(IN)    :: Y(1:,1:,1:) !< Y component.
  INTEGER(I1P),               INTENT(IN)    :: Z(1:,1:,1:) !< Z component.
  INTEGER(I4P)                              :: N_BYTE      !< Number of bytes
  INTEGER(I4P)                              :: N1          !< Counter.
  INTEGER(I4P)                              :: N2          !< Counter.
  INTEGER(I4P)                              :: N3          !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[(((X(N1,N2,N3), Y(N1,N2,N3), Z(N1,N2,N3), &
                                           N1=1,SIZE(X, DIM=1)),N2=1,SIZE(X, DIM=2)),N3=1,SIZE(X, DIM=3))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY3_RANK3_I1P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK1_R8P(SELF, U, V, W, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 6 components of rank 1 (R8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF   !< Writer.
  REAL(R8P),                  INTENT(IN)    :: U(1:)  !< U component.
  REAL(R8P),                  INTENT(IN)    :: V(1:)  !< V component.
  REAL(R8P),                  INTENT(IN)    :: W(1:)  !< W component.
  REAL(R8P),                  INTENT(IN)    :: X(1:)  !< X component.
  REAL(R8P),                  INTENT(IN)    :: Y(1:)  !< Y component.
  REAL(R8P),                  INTENT(IN)    :: Z(1:)  !< Z component.
  INTEGER(I4P)                              :: N_BYTE !< Number of bytes
  INTEGER(I4P)                              :: N      !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[(U(N), V(N), W(N), X(N), Y(N), Z(N), N=1,SIZE(X, DIM=1))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK1_R8P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK1_R4P(SELF, U, V, W, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 6 components of rank 1 (R4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF   !< Writer.
  REAL(R4P),                  INTENT(IN)    :: U(1:)  !< U component.
  REAL(R4P),                  INTENT(IN)    :: V(1:)  !< V component.
  REAL(R4P),                  INTENT(IN)    :: W(1:)  !< W component.
  REAL(R4P),                  INTENT(IN)    :: X(1:)  !< X component.
  REAL(R4P),                  INTENT(IN)    :: Y(1:)  !< Y component.
  REAL(R4P),                  INTENT(IN)    :: Z(1:)  !< Z component.
  INTEGER(I4P)                              :: N_BYTE !< Number of bytes
  INTEGER(I4P)                              :: N      !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[(U(N), V(N), W(N), X(N), Y(N), Z(N), N=1,SIZE(X, DIM=1))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK1_R4P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK1_I8P(SELF, U, V, W, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 3 components of rank 1 (I8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF   !< Writer.
  INTEGER(I8P),               INTENT(IN)    :: U(1:)  !< U component.
  INTEGER(I8P),               INTENT(IN)    :: V(1:)  !< V component.
  INTEGER(I8P),               INTENT(IN)    :: W(1:)  !< W component.
  INTEGER(I8P),               INTENT(IN)    :: X(1:)  !< X component.
  INTEGER(I8P),               INTENT(IN)    :: Y(1:)  !< Y component.
  INTEGER(I8P),               INTENT(IN)    :: Z(1:)  !< Z component.
  INTEGER(I4P)                              :: N_BYTE !< Number of bytes
  INTEGER(I4P)                              :: N      !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[(U(N), V(N), W(N), X(N), Y(N), Z(N), N=1,SIZE(X, DIM=1))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK1_I8P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK1_I4P(SELF, U, V, W, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 6 components of rank 1 (I4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF   !< Writer.
  INTEGER(I4P),               INTENT(IN)    :: U(1:)  !< U component.
  INTEGER(I4P),               INTENT(IN)    :: V(1:)  !< V component.
  INTEGER(I4P),               INTENT(IN)    :: W(1:)  !< W component.
  INTEGER(I4P),               INTENT(IN)    :: X(1:)  !< X component.
  INTEGER(I4P),               INTENT(IN)    :: Y(1:)  !< Y component.
  INTEGER(I4P),               INTENT(IN)    :: Z(1:)  !< Z component.
  INTEGER(I4P)                              :: N_BYTE !< Number of bytes
  INTEGER(I4P)                              :: N      !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[(U(N), V(N), W(N), X(N), Y(N), Z(N), N=1,SIZE(X, DIM=1))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK1_I4P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK1_I2P(SELF, U, V, W, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 6 components of rank 1 (I2P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF   !< Writer.
  INTEGER(I2P),               INTENT(IN)    :: U(1:)  !< U component.
  INTEGER(I2P),               INTENT(IN)    :: V(1:)  !< V component.
  INTEGER(I2P),               INTENT(IN)    :: W(1:)  !< W component.
  INTEGER(I2P),               INTENT(IN)    :: X(1:)  !< X component.
  INTEGER(I2P),               INTENT(IN)    :: Y(1:)  !< Y component.
  INTEGER(I2P),               INTENT(IN)    :: Z(1:)  !< Z component.
  INTEGER(I4P)                              :: N_BYTE !< Number of bytes
  INTEGER(I4P)                              :: N      !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[(U(N), V(N), W(N), X(N), Y(N), Z(N), N=1,SIZE(X, DIM=1))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK1_I2P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK1_I1P(SELF, U, V, W, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 6 components of rank 1 (I1P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF   !< Writer.
  INTEGER(I1P),               INTENT(IN)    :: U(1:)  !< U component.
  INTEGER(I1P),               INTENT(IN)    :: V(1:)  !< V component.
  INTEGER(I1P),               INTENT(IN)    :: W(1:)  !< W component.
  INTEGER(I1P),               INTENT(IN)    :: X(1:)  !< X component.
  INTEGER(I1P),               INTENT(IN)    :: Y(1:)  !< Y component.
  INTEGER(I1P),               INTENT(IN)    :: Z(1:)  !< Z component.
  INTEGER(I4P)                              :: N_BYTE !< Number of bytes
  INTEGER(I4P)                              :: N      !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[(U(N), V(N), W(N), X(N), Y(N), Z(N), N=1,SIZE(X, DIM=1))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK1_I1P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK2_R8P(SELF, U, V, W, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 6 components of rank 2 (R8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF     !< Writer.
  REAL(R8P),                  INTENT(IN)    :: U(1:,1:) !< U component.
  REAL(R8P),                  INTENT(IN)    :: V(1:,1:) !< V component.
  REAL(R8P),                  INTENT(IN)    :: W(1:,1:) !< W component.
  REAL(R8P),                  INTENT(IN)    :: X(1:,1:) !< X component.
  REAL(R8P),                  INTENT(IN)    :: Y(1:,1:) !< Y component.
  REAL(R8P),                  INTENT(IN)    :: Z(1:,1:) !< Z component.
  INTEGER(I4P)                              :: N_BYTE   !< Number of bytes
  INTEGER(I4P)                              :: N1       !< Counter.
  INTEGER(I4P)                              :: N2       !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[((U(N1,N2), V(N1,N2), W(N1,N2), &
                                                X(N1,N2), Y(N1,N2), Z(N1,N2), N1=1,SIZE(X, DIM=1)),N2=1,SIZE(X, DIM=2))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK2_R8P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK2_R4P(SELF, U, V, W, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 6 components of rank 2 (R4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF     !< Writer.
  REAL(R4P),                  INTENT(IN)    :: U(1:,1:) !< U component.
  REAL(R4P),                  INTENT(IN)    :: V(1:,1:) !< V component.
  REAL(R4P),                  INTENT(IN)    :: W(1:,1:) !< W component.
  REAL(R4P),                  INTENT(IN)    :: X(1:,1:) !< X component.
  REAL(R4P),                  INTENT(IN)    :: Y(1:,1:) !< Y component.
  REAL(R4P),                  INTENT(IN)    :: Z(1:,1:) !< Z component.
  INTEGER(I4P)                              :: N_BYTE   !< Number of bytes
  INTEGER(I4P)                              :: N1       !< Counter.
  INTEGER(I4P)                              :: N2       !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[((U(N1,N2), V(N1,N2), W(N1,N2), &
                                                X(N1,N2), Y(N1,N2), Z(N1,N2), N1=1,SIZE(X, DIM=1)),N2=1,SIZE(X, DIM=2))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK2_R4P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK2_I8P(SELF, U, V, W, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 6 components of rank 2 (I8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF     !< Writer.
  INTEGER(I8P),               INTENT(IN)    :: U(1:,1:) !< U component.
  INTEGER(I8P),               INTENT(IN)    :: V(1:,1:) !< V component.
  INTEGER(I8P),               INTENT(IN)    :: W(1:,1:) !< W component.
  INTEGER(I8P),               INTENT(IN)    :: X(1:,1:) !< X component.
  INTEGER(I8P),               INTENT(IN)    :: Y(1:,1:) !< Y component.
  INTEGER(I8P),               INTENT(IN)    :: Z(1:,1:) !< Z component.
  INTEGER(I4P)                              :: N_BYTE   !< Number of bytes
  INTEGER(I4P)                              :: N1       !< Counter.
  INTEGER(I4P)                              :: N2       !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[((U(N1,N2), V(N1,N2), W(N1,N2), &
                                                X(N1,N2), Y(N1,N2), Z(N1,N2), N1=1,SIZE(X, DIM=1)),N2=1,SIZE(X, DIM=2))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK2_I8P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK2_I4P(SELF, U, V, W, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 6 components of rank 2 (I4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF     !< Writer.
  INTEGER(I4P),               INTENT(IN)    :: U(1:,1:) !< U component.
  INTEGER(I4P),               INTENT(IN)    :: V(1:,1:) !< V component.
  INTEGER(I4P),               INTENT(IN)    :: W(1:,1:) !< W component.
  INTEGER(I4P),               INTENT(IN)    :: X(1:,1:) !< X component.
  INTEGER(I4P),               INTENT(IN)    :: Y(1:,1:) !< Y component.
  INTEGER(I4P),               INTENT(IN)    :: Z(1:,1:) !< Z component.
  INTEGER(I4P)                              :: N_BYTE   !< Number of bytes
  INTEGER(I4P)                              :: N1       !< Counter.
  INTEGER(I4P)                              :: N2       !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[((U(N1,N2), V(N1,N2), W(N1,N2), &
                                                X(N1,N2), Y(N1,N2), Z(N1,N2), N1=1,SIZE(X, DIM=1)),N2=1,SIZE(X, DIM=2))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK2_I4P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK2_I2P(SELF, U, V, W, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 6 components of rank 2 (I2P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF     !< Writer.
  INTEGER(I2P),               INTENT(IN)    :: U(1:,1:) !< U component.
  INTEGER(I2P),               INTENT(IN)    :: V(1:,1:) !< V component.
  INTEGER(I2P),               INTENT(IN)    :: W(1:,1:) !< W component.
  INTEGER(I2P),               INTENT(IN)    :: X(1:,1:) !< X component.
  INTEGER(I2P),               INTENT(IN)    :: Y(1:,1:) !< Y component.
  INTEGER(I2P),               INTENT(IN)    :: Z(1:,1:) !< Z component.
  INTEGER(I4P)                              :: N_BYTE   !< Number of bytes
  INTEGER(I4P)                              :: N1       !< Counter.
  INTEGER(I4P)                              :: N2       !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[((U(N1,N2), V(N1,N2), W(N1,N2), &
                                                X(N1,N2), Y(N1,N2), Z(N1,N2), N1=1,SIZE(X, DIM=1)),N2=1,SIZE(X, DIM=2))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK2_I2P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK2_I1P(SELF, U, V, W, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 6 components of rank 2 (I1P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF     !< Writer.
  INTEGER(I1P),               INTENT(IN)    :: U(1:,1:) !< U component.
  INTEGER(I1P),               INTENT(IN)    :: V(1:,1:) !< V component.
  INTEGER(I1P),               INTENT(IN)    :: W(1:,1:) !< W component.
  INTEGER(I1P),               INTENT(IN)    :: X(1:,1:) !< X component.
  INTEGER(I1P),               INTENT(IN)    :: Y(1:,1:) !< Y component.
  INTEGER(I1P),               INTENT(IN)    :: Z(1:,1:) !< Z component.
  INTEGER(I4P)                              :: N_BYTE   !< Number of bytes
  INTEGER(I4P)                              :: N1       !< Counter.
  INTEGER(I4P)                              :: N2       !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[((U(N1,N2), V(N1,N2), W(N1,N2), &
                                                X(N1,N2), Y(N1,N2), Z(N1,N2), N1=1,SIZE(X, DIM=1)),N2=1,SIZE(X, DIM=2))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK2_I1P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK3_R8P(SELF, U, V, W, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 6 components of rank 3 (R8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF        !< Writer.
  REAL(R8P),                  INTENT(IN)    :: U(1:,1:,1:) !< U component.
  REAL(R8P),                  INTENT(IN)    :: V(1:,1:,1:) !< V component.
  REAL(R8P),                  INTENT(IN)    :: W(1:,1:,1:) !< W component.
  REAL(R8P),                  INTENT(IN)    :: X(1:,1:,1:) !< X component.
  REAL(R8P),                  INTENT(IN)    :: Y(1:,1:,1:) !< Y component.
  REAL(R8P),                  INTENT(IN)    :: Z(1:,1:,1:) !< Z component.
  INTEGER(I4P)                              :: N_BYTE      !< Number of bytes
  INTEGER(I4P)                              :: N1          !< Counter.
  INTEGER(I4P)                              :: N2          !< Counter.
  INTEGER(I4P)                              :: N3          !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[(((U(N1,N2,N3), V(N1,N2,N3), W(N1,N2,N3), &
                                                  X(N1,N2,N3), Y(N1,N2,N3), Z(N1,N2,N3), &
                                           N1=1,SIZE(X, DIM=1)),N2=1,SIZE(X, DIM=2)),N3=1,SIZE(X, DIM=3))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK3_R8P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK3_R4P(SELF, U, V, W, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 6 components of rank 3 (R4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF        !< Writer.
  REAL(R4P),                  INTENT(IN)    :: U(1:,1:,1:) !< U component.
  REAL(R4P),                  INTENT(IN)    :: V(1:,1:,1:) !< V component.
  REAL(R4P),                  INTENT(IN)    :: W(1:,1:,1:) !< W component.
  REAL(R4P),                  INTENT(IN)    :: X(1:,1:,1:) !< X component.
  REAL(R4P),                  INTENT(IN)    :: Y(1:,1:,1:) !< Y component.
  REAL(R4P),                  INTENT(IN)    :: Z(1:,1:,1:) !< Z component.
  INTEGER(I4P)                              :: N_BYTE      !< Number of bytes
  INTEGER(I4P)                              :: N1          !< Counter.
  INTEGER(I4P)                              :: N2          !< Counter.
  INTEGER(I4P)                              :: N3          !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[(((U(N1,N2,N3), V(N1,N2,N3), W(N1,N2,N3), X(N1,N2,N3), Y(N1,N2,N3), Z(N1,N2,N3), &
                                           N1=1,SIZE(X, DIM=1)),N2=1,SIZE(X, DIM=2)),N3=1,SIZE(X, DIM=3))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK3_R4P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK3_I8P(SELF, U, V, W, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 6 components of rank 3 (I8P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF        !< Writer.
  INTEGER(I8P),               INTENT(IN)    :: U(1:,1:,1:) !< U component.
  INTEGER(I8P),               INTENT(IN)    :: V(1:,1:,1:) !< V component.
  INTEGER(I8P),               INTENT(IN)    :: W(1:,1:,1:) !< W component.
  INTEGER(I8P),               INTENT(IN)    :: X(1:,1:,1:) !< X component.
  INTEGER(I8P),               INTENT(IN)    :: Y(1:,1:,1:) !< Y component.
  INTEGER(I8P),               INTENT(IN)    :: Z(1:,1:,1:) !< Z component.
  INTEGER(I4P)                              :: N_BYTE      !< Number of bytes
  INTEGER(I4P)                              :: N1          !< Counter.
  INTEGER(I4P)                              :: N2          !< Counter.
  INTEGER(I4P)                              :: N3          !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[(((U(N1,N2,N3), V(N1,N2,N3), W(N1,N2,N3), X(N1,N2,N3), Y(N1,N2,N3), Z(N1,N2,N3), &
                                           N1=1,SIZE(X, DIM=1)),N2=1,SIZE(X, DIM=2)),N3=1,SIZE(X, DIM=3))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK3_I8P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK3_I4P(SELF, U, V, W, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 6 components of rank 3 (I4P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF        !< Writer.
  INTEGER(I4P),               INTENT(IN)    :: U(1:,1:,1:) !< U component.
  INTEGER(I4P),               INTENT(IN)    :: V(1:,1:,1:) !< V component.
  INTEGER(I4P),               INTENT(IN)    :: W(1:,1:,1:) !< W component.
  INTEGER(I4P),               INTENT(IN)    :: X(1:,1:,1:) !< X component.
  INTEGER(I4P),               INTENT(IN)    :: Y(1:,1:,1:) !< Y component.
  INTEGER(I4P),               INTENT(IN)    :: Z(1:,1:,1:) !< Z component.
  INTEGER(I4P)                              :: N_BYTE      !< Number of bytes
  INTEGER(I4P)                              :: N1          !< Counter.
  INTEGER(I4P)                              :: N2          !< Counter.
  INTEGER(I4P)                              :: N3          !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[(((U(N1,N2,N3), V(N1,N2,N3), W(N1,N2,N3), X(N1,N2,N3), Y(N1,N2,N3), Z(N1,N2,N3), &
                                           N1=1,SIZE(X, DIM=1)),N2=1,SIZE(X, DIM=2)),N3=1,SIZE(X, DIM=3))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK3_I4P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK3_I2P(SELF, U, V, W, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 6 components of rank 3 (I2P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF        !< Writer.
  INTEGER(I2P),               INTENT(IN)    :: U(1:,1:,1:) !< U component.
  INTEGER(I2P),               INTENT(IN)    :: V(1:,1:,1:) !< V component.
  INTEGER(I2P),               INTENT(IN)    :: W(1:,1:,1:) !< W component.
  INTEGER(I2P),               INTENT(IN)    :: X(1:,1:,1:) !< X component.
  INTEGER(I2P),               INTENT(IN)    :: Y(1:,1:,1:) !< Y component.
  INTEGER(I2P),               INTENT(IN)    :: Z(1:,1:,1:) !< Z component.
  INTEGER(I4P)                              :: N_BYTE      !< Number of bytes
  INTEGER(I4P)                              :: N1          !< Counter.
  INTEGER(I4P)                              :: N2          !< Counter.
  INTEGER(I4P)                              :: N3          !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[(((U(N1,N2,N3), V(N1,N2,N3), W(N1,N2,N3), X(N1,N2,N3), Y(N1,N2,N3), Z(N1,N2,N3), &
                                           N1=1,SIZE(X, DIM=1)),N2=1,SIZE(X, DIM=2)),N3=1,SIZE(X, DIM=3))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK3_I2P

  FUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK3_I1P(SELF, U, V, W, X, Y, Z) RESULT(N_BYTE)
  !< Write a dataarray with 6 components of rank 3 (I1P).
  CLASS(XML_WRITER_APPENDED), INTENT(INOUT) :: SELF        !< Writer.
  INTEGER(I1P),               INTENT(IN)    :: U(1:,1:,1:) !< U component.
  INTEGER(I1P),               INTENT(IN)    :: V(1:,1:,1:) !< V component.
  INTEGER(I1P),               INTENT(IN)    :: W(1:,1:,1:) !< W component.
  INTEGER(I1P),               INTENT(IN)    :: X(1:,1:,1:) !< X component.
  INTEGER(I1P),               INTENT(IN)    :: Y(1:,1:,1:) !< Y component.
  INTEGER(I1P),               INTENT(IN)    :: Z(1:,1:,1:) !< Z component.
  INTEGER(I4P)                              :: N_BYTE      !< Number of bytes
  INTEGER(I4P)                              :: N1          !< Counter.
  INTEGER(I4P)                              :: N2          !< Counter.
  INTEGER(I4P)                              :: N3          !< Counter.

  N_BYTE = SELF%WRITE_ON_SCRATCH_DATAARRAY(X=[(((U(N1,N2,N3), V(N1,N2,N3), W(N1,N2,N3), X(N1,N2,N3), Y(N1,N2,N3), Z(N1,N2,N3), &
                                           N1=1,SIZE(X, DIM=1)),N2=1,SIZE(X, DIM=2)),N3=1,SIZE(X, DIM=3))])
  ENDFUNCTION WRITE_ON_SCRATCH_DATAARRAY6_RANK3_I1P
ENDMODULE ModLib_XMLWriterAppended

!< VTK file XMl writer, binary local.
MODULE ModLib_XMLWriterBinary
!< VTK file XMl writer, binary local.
!<
!< This writer is (currently) the only one able also to create a *volatile* file (enabling volatile option).
!< Instead of saving data into a real file on the system, this writer can save data into a (volatile) characters string that can be
!< used also on (slave) processes that have not access to filesystem: the volatile string can be easily sent to other (master)
!< processes having access to filesytem. The volatile writer mode is designed to facilitate the use of the library in parallel
!< envinroments wehere not all processes/threads have access to filesystem.
USE ModLib_Penf
USE ModLib_Stringifor
USE ModLib_VTKDataEncoder
USE ModLib_XMLWriterAbstract

IMPLICIT NONE
PRIVATE
PUBLIC :: XML_WRITER_BINARY_LOCAL

TYPE, EXTENDS(XML_WRITER_ABSTRACT) :: XML_WRITER_BINARY_LOCAL
   !< VTK file XML writer, binary local.
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
ENDTYPE XML_WRITER_BINARY_LOCAL
CONTAINS
   FUNCTION INITIALIZE(SELF, FORMAT, FILENAME, MESH_TOPOLOGY, NX1, NX2, NY1, NY2, NZ1, NZ2, &
                       IS_VOLATILE, MESH_KIND) RESULT(ERROR)
   !< Initialize writer.
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF          !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: FORMAT        !< File format: binary.
   CHARACTER(*),                   INTENT(IN)           :: FILENAME      !< File name.
   CHARACTER(*),                   INTENT(IN)           :: MESH_TOPOLOGY !< Mesh topology.
   INTEGER(I4P),                   INTENT(IN), OPTIONAL :: NX1           !< Initial node of x axis.
   INTEGER(I4P),                   INTENT(IN), OPTIONAL :: NX2           !< Final node of x axis.
   INTEGER(I4P),                   INTENT(IN), OPTIONAL :: NY1           !< Initial node of y axis.
   INTEGER(I4P),                   INTENT(IN), OPTIONAL :: NY2           !< Final node of y axis.
   INTEGER(I4P),                   INTENT(IN), OPTIONAL :: NZ1           !< Initial node of z axis.
   INTEGER(I4P),                   INTENT(IN), OPTIONAL :: NZ2           !< Final node of z axis.
   CHARACTER(*),                   INTENT(IN), OPTIONAL :: MESH_KIND     !< Kind of mesh data: Float64, Float32, ECC.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_VOLATILE   !< Flag to check volatile writer.
   INTEGER(I4P)                                         :: ERROR         !< Error status.

   SELF%IS_VOLATILE = .FALSE. ; IF (PRESENT(IS_VOLATILE)) SELF%IS_VOLATILE = IS_VOLATILE
   SELF%TOPOLOGY = TRIM(ADJUSTL(MESH_TOPOLOGY))
   SELF%FORMAT_CH = FORMAT
   SELF%FORMAT_CH = SELF%FORMAT_CH%LOWER()
   CALL SELF%OPEN_XML_FILE(FILENAME=FILENAME)
   CALL SELF%WRITE_HEADER_TAG
   CALL SELF%WRITE_TOPOLOGY_TAG(NX1=NX1, NX2=NX2, NY1=NY1, NY2=NY2, NZ1=NZ1, NZ2=NZ2, MESH_KIND=MESH_KIND)
   ERROR = SELF%ERROR
   ENDFUNCTION INITIALIZE

   FUNCTION FINALIZE(SELF) RESULT(ERROR)
   !< Finalize writer.
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT) :: SELF  !< Writer.
   INTEGER(I4P)                                  :: ERROR !< Error status.

   CALL SELF%WRITE_END_TAG(NAME=SELF%TOPOLOGY%CHARS())
   CALL SELF%WRITE_END_TAG(NAME='VTKFile')
   CALL SELF%CLOSE_XML_FILE
   ERROR = SELF%ERROR
   ENDFUNCTION FINALIZE

   ! write_dataarray methods
   FUNCTION WRITE_DATAARRAY1_RANK1_R8P(SELF, DATA_NAME, X, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (R8P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   REAL(R8P),                      INTENT(IN)           :: X(1:)        !< Data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Float64'
   N_COMPONENTS = 1
   CODE = ENCODE_BINARY_DATAARRAY(X=X)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY1_RANK1_R8P

   FUNCTION WRITE_DATAARRAY1_RANK1_R4P(SELF, DATA_NAME, X, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (R4P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   REAL(R4P),                      INTENT(IN)           :: X(1:)        !< Data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Float32'
   N_COMPONENTS = 1
   CODE = ENCODE_BINARY_DATAARRAY(X=X)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY1_RANK1_R4P

   FUNCTION WRITE_DATAARRAY1_RANK1_I8P(SELF, DATA_NAME, X, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I8P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   INTEGER(I8P),                   INTENT(IN)           :: X(1:)        !< Data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Int64'
   N_COMPONENTS = 1
   CODE = ENCODE_BINARY_DATAARRAY(X=X)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY1_RANK1_I8P

   FUNCTION WRITE_DATAARRAY1_RANK1_I4P(SELF, DATA_NAME, X, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I4P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   INTEGER(I4P),                   INTENT(IN)           :: X(1:)        !< Data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Int32'
   N_COMPONENTS = 1
   CODE = ENCODE_BINARY_DATAARRAY(X=X)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY1_RANK1_I4P

   FUNCTION WRITE_DATAARRAY1_RANK1_I2P(SELF, DATA_NAME, X, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I2P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   INTEGER(I2P),                   INTENT(IN)           :: X(1:)        !< Data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Int16'
   N_COMPONENTS = 1
   CODE = ENCODE_BINARY_DATAARRAY(X=X)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY1_RANK1_I2P

   FUNCTION WRITE_DATAARRAY1_RANK1_I1P(SELF, DATA_NAME, X, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I1P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   INTEGER(I1P),                   INTENT(IN)           :: X(1:)        !< Data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Int8'
   N_COMPONENTS = 1
   CODE = ENCODE_BINARY_DATAARRAY(X=X)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY1_RANK1_I1P

   FUNCTION WRITE_DATAARRAY1_RANK2_R8P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF          !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME     !< Data name.
   REAL(R8P),                      INTENT(IN)           :: X(1:,1:)      !< Data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples" instead of "NUMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR         !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE     !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS  !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE          !< Data variable encoded, binary or BASE64 CODEC.

   DATA_TYPE = 'Float64'
   N_COMPONENTS = SIZE(X, DIM=1)
   IF (PRESENT(ONE_COMPONENT)) THEN
     IF (ONE_COMPONENT) N_COMPONENTS = 1
   ENDIF
   CODE = ENCODE_BINARY_DATAARRAY(X=X)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY1_RANK2_R8P

   FUNCTION WRITE_DATAARRAY1_RANK2_R4P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF          !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME     !< Data name.
   REAL(R4P),                      INTENT(IN)           :: X(1:,1:)      !< Data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples" instead of "NUMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR         !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE     !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS  !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE          !< Data variable encoded, binary or BASE64 CODEC.

   DATA_TYPE = 'Float32'
   N_COMPONENTS = SIZE(X, DIM=1)
   IF (PRESENT(ONE_COMPONENT)) THEN
     IF (ONE_COMPONENT) N_COMPONENTS = 1
   ENDIF
   CODE = ENCODE_BINARY_DATAARRAY(X=X)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY1_RANK2_R4P

   FUNCTION WRITE_DATAARRAY1_RANK2_I8P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF          !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME     !< Data name.
   INTEGER(I8P),                   INTENT(IN)           :: X(1:,1:)      !< Data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples" instead of "NUMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR         !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE     !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS  !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE          !< Data variable encoded, binary or BASE64 CODEC.

   DATA_TYPE = 'Int64'
   N_COMPONENTS = SIZE(X, DIM=1)
   IF (PRESENT(ONE_COMPONENT)) THEN
     IF (ONE_COMPONENT) N_COMPONENTS = 1
   ENDIF
   CODE = ENCODE_BINARY_DATAARRAY(X=X)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY1_RANK2_I8P

   FUNCTION WRITE_DATAARRAY1_RANK2_I4P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF          !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME     !< Data name.
   INTEGER(I4P),                   INTENT(IN)           :: X(1:,1:)      !< Data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples" instead of "NUMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR         !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE     !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS  !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE          !< Data variable encoded, binary or BASE64 CODEC.

   DATA_TYPE = 'Int32'
   N_COMPONENTS = SIZE(X, DIM=1)
   IF (PRESENT(ONE_COMPONENT)) THEN
     IF (ONE_COMPONENT) N_COMPONENTS = 1
   ENDIF
   CODE = ENCODE_BINARY_DATAARRAY(X=X)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY1_RANK2_I4P

   FUNCTION WRITE_DATAARRAY1_RANK2_I2P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF          !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME     !< Data name.
   INTEGER(I2P),                   INTENT(IN)           :: X(1:,1:)      !< Data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples" instead of "NUMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR         !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE     !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS  !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE          !< Data variable encoded, binary or BASE64 CODEC.

   DATA_TYPE = 'Int16'
   N_COMPONENTS = SIZE(X, DIM=1)
   IF (PRESENT(ONE_COMPONENT)) THEN
     IF (ONE_COMPONENT) N_COMPONENTS = 1
   ENDIF
   CODE = ENCODE_BINARY_DATAARRAY(X=X)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY1_RANK2_I2P

   FUNCTION WRITE_DATAARRAY1_RANK2_I1P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF          !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME     !< Data name.
   INTEGER(I1P),                   INTENT(IN)           :: X(1:,1:)      !< Data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples" instead of "NUMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR         !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE     !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS  !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE          !< Data variable encoded, binary or BASE64 CODEC.

   DATA_TYPE = 'Int8'
   N_COMPONENTS = SIZE(X, DIM=1)
   IF (PRESENT(ONE_COMPONENT)) THEN
     IF (ONE_COMPONENT) N_COMPONENTS = 1
   ENDIF
   CODE = ENCODE_BINARY_DATAARRAY(X=X)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY1_RANK2_I1P

   FUNCTION WRITE_DATAARRAY1_RANK3_R8P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF          !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME     !< Data name.
   REAL(R8P),                      INTENT(IN)           :: X(1:,1:,1:)   !< Data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples" instead of "NUMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR         !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE     !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS  !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE          !< Data variable encoded, binary or BASE64 CODEC.

   DATA_TYPE = 'Float64'
   N_COMPONENTS = SIZE(X, DIM=1)
   IF (PRESENT(ONE_COMPONENT)) THEN
     IF (ONE_COMPONENT) N_COMPONENTS = 1
   ENDIF
   CODE = ENCODE_BINARY_DATAARRAY(X=X)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY1_RANK3_R8P

   FUNCTION WRITE_DATAARRAY1_RANK3_R4P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF          !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME     !< Data name.
   REAL(R4P),                      INTENT(IN)           :: X(1:,1:,1:)   !< Data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples" instead of "NUMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR         !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE     !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS  !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE          !< Data variable encoded, binary or BASE64 CODEC.

   DATA_TYPE = 'Float32'
   N_COMPONENTS = SIZE(X, DIM=1)
   IF (PRESENT(ONE_COMPONENT)) THEN
     IF (ONE_COMPONENT) N_COMPONENTS = 1
   ENDIF
   CODE = ENCODE_BINARY_DATAARRAY(X=X)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY1_RANK3_R4P

   FUNCTION WRITE_DATAARRAY1_RANK3_I8P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF          !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME     !< Data name.
   INTEGER(I8P),                   INTENT(IN)           :: X(1:,1:,1:)   !< Data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples" instead of "NUMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR         !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE     !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS  !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE          !< Data variable encoded, binary or BASE64 CODEC.

   DATA_TYPE = 'Int64'
   N_COMPONENTS = SIZE(X, DIM=1)
   IF (PRESENT(ONE_COMPONENT)) THEN
     IF (ONE_COMPONENT) N_COMPONENTS = 1
   ENDIF
   CODE = ENCODE_BINARY_DATAARRAY(X=X)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY1_RANK3_I8P

   FUNCTION WRITE_DATAARRAY1_RANK3_I4P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF          !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME     !< Data name.
   INTEGER(I4P),                   INTENT(IN)           :: X(1:,1:,1:)   !< Data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples" instead of "NUMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR         !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE     !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS  !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE          !< Data variable encoded, binary or BASE64 CODEC.

   DATA_TYPE = 'Int32'
   N_COMPONENTS = SIZE(X, DIM=1)
   IF (PRESENT(ONE_COMPONENT)) THEN
     IF (ONE_COMPONENT) N_COMPONENTS = 1
   ENDIF
   CODE = ENCODE_BINARY_DATAARRAY(X=X)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY1_RANK3_I4P

   FUNCTION WRITE_DATAARRAY1_RANK3_I2P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF          !< Writer.
   CHARACTER(*),                  INTENT(IN)           :: DATA_NAME     !< Data name.
   INTEGER(I2P),                  INTENT(IN)           :: X(1:,1:,1:)   !< Data variable.
   LOGICAL,                       INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
   LOGICAL,                       INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                        :: ERROR         !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                       :: DATA_TYPE     !< Data type.
   INTEGER(I4P)                                        :: N_COMPONENTS  !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                       :: CODE          !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Int16'
   N_COMPONENTS = SIZE(X, DIM=1)
   IF (PRESENT(ONE_COMPONENT)) THEN
     IF (ONE_COMPONENT) N_COMPONENTS = 1
   ENDIF
   CODE = ENCODE_BINARY_DATAARRAY(X=X)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY1_RANK3_I2P

   FUNCTION WRITE_DATAARRAY1_RANK3_I1P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF          !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME     !< Data name.
   INTEGER(I1P),                   INTENT(IN)           :: X(1:,1:,1:)   !< Data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples" instead of "NUMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR         !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE     !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS  !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE          !< Data variable encoded, binary or BASE64 CODEC.

   DATA_TYPE = 'Int8'
   N_COMPONENTS = SIZE(X, DIM=1)
   IF (PRESENT(ONE_COMPONENT)) THEN
     IF (ONE_COMPONENT) N_COMPONENTS = 1
   ENDIF
   CODE = ENCODE_BINARY_DATAARRAY(X=X)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY1_RANK3_I1P

   FUNCTION WRITE_DATAARRAY1_RANK4_R8P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF           !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME      !< Data name.
   REAL(R8P),                      INTENT(IN)           :: X(1:,1:,1:,1:) !< Data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: ONE_COMPONENT  !< Force one component.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES      !< Use "NumberOfTuples" instead of "NUMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR          !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE      !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS   !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE           !< Data variable encoded, binary or BASE64 CODEC.

   DATA_TYPE = 'Float64'
   N_COMPONENTS = SIZE(X, DIM=1)
   IF (PRESENT(ONE_COMPONENT)) THEN
     IF (ONE_COMPONENT) N_COMPONENTS = 1
   ENDIF
   CODE = ENCODE_BINARY_DATAARRAY(X=X)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY1_RANK4_R8P

   FUNCTION WRITE_DATAARRAY1_RANK4_R4P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF           !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME      !< Data name.
   REAL(R4P),                      INTENT(IN)           :: X(1:,1:,1:,1:) !< Data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: ONE_COMPONENT  !< Force one component.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES      !< Use "NumberOfTuples" instead of "NUMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR          !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE      !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS   !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE           !< Data variable encoded, binary or BASE64 CODEC.

   DATA_TYPE = 'Float32'
   N_COMPONENTS = SIZE(X, DIM=1)
   IF (PRESENT(ONE_COMPONENT)) THEN
     IF (ONE_COMPONENT) N_COMPONENTS = 1
   ENDIF
   CODE = ENCODE_BINARY_DATAARRAY(X=X)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY1_RANK4_R4P

   FUNCTION WRITE_DATAARRAY1_RANK4_I8P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF           !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME      !< Data name.
   INTEGER(I8P),                   INTENT(IN)           :: X(1:,1:,1:,1:) !< Data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: ONE_COMPONENT  !< Force one component.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES      !< Use "NumberOfTuples" instead of "NUMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR          !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE      !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS   !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE           !< Data variable encoded, binary or BASE64 CODEC.

   DATA_TYPE = 'Int64'
   N_COMPONENTS = SIZE(X, DIM=1)
   IF (PRESENT(ONE_COMPONENT)) THEN
     IF (ONE_COMPONENT) N_COMPONENTS = 1
   ENDIF
   CODE = ENCODE_BINARY_DATAARRAY(X=X)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY1_RANK4_I8P

   FUNCTION WRITE_DATAARRAY1_RANK4_I4P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF           !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME      !< Data name.
   INTEGER(I4P),                   INTENT(IN)           :: X(1:,1:,1:,1:) !< Data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: ONE_COMPONENT  !< Force one component.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES      !< Use "NumberOfTuples" instead of "NUMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR          !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE      !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS   !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE           !< Data variable encoded, binary or BASE64 CODEC.

   DATA_TYPE = 'Int32'
   N_COMPONENTS = SIZE(X, DIM=1)
   IF (PRESENT(ONE_COMPONENT)) THEN
     IF (ONE_COMPONENT) N_COMPONENTS = 1
   ENDIF
   CODE = ENCODE_BINARY_DATAARRAY(X=X)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY1_RANK4_I4P

   FUNCTION WRITE_DATAARRAY1_RANK4_I2P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF           !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME      !< Data name.
   INTEGER(I2P),                   INTENT(IN)           :: X(1:,1:,1:,1:) !< Data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: ONE_COMPONENT  !< Force one component.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES      !< Use "NumberOfTuples" instead of "NUMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR          !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE      !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS   !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE           !< Data variable encoded, binary or BASE64 CODEC.

   DATA_TYPE = 'Int16'
   N_COMPONENTS = SIZE(X, DIM=1)
   IF (PRESENT(ONE_COMPONENT)) THEN
     IF (ONE_COMPONENT) N_COMPONENTS = 1
   ENDIF
   CODE = ENCODE_BINARY_DATAARRAY(X=X)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY1_RANK4_I2P

   FUNCTION WRITE_DATAARRAY1_RANK4_I1P(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF           !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME      !< Data name.
   INTEGER(I1P),                   INTENT(IN)           :: X(1:,1:,1:,1:) !< Data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: ONE_COMPONENT  !< Force one component.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES      !< Use "NumberOfTuples" instead of "NUMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR          !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE      !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS   !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE           !< Data variable encoded, binary or BASE64 CODEC.

   DATA_TYPE = 'Int8'
   N_COMPONENTS = SIZE(X, DIM=1)
   IF (PRESENT(ONE_COMPONENT)) THEN
     IF (ONE_COMPONENT) N_COMPONENTS = 1
   ENDIF
   CODE = ENCODE_BINARY_DATAARRAY(X=X)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY1_RANK4_I1P

   FUNCTION WRITE_DATAARRAY3_RANK1_R8P(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   REAL(R8P),                      INTENT(IN)           :: X(1:)        !< X component of data variable.
   REAL(R8P),                      INTENT(IN)           :: Y(1:)        !< Y component of data variable.
   REAL(R8P),                      INTENT(IN)           :: Z(1:)        !< Z component of data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Float64'
   N_COMPONENTS = 3
   CODE = ENCODE_BINARY_DATAARRAY(X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY3_RANK1_R8P

   FUNCTION WRITE_DATAARRAY3_RANK1_R4P(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   REAL(R4P),                      INTENT(IN)           :: X(1:)        !< X component of data variable.
   REAL(R4P),                      INTENT(IN)           :: Y(1:)        !< Y component of data variable.
   REAL(R4P),                      INTENT(IN)           :: Z(1:)        !< Z component of data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Float32'
   N_COMPONENTS = 3
   CODE = ENCODE_BINARY_DATAARRAY(X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY3_RANK1_R4P

   FUNCTION WRITE_DATAARRAY3_RANK1_I8P(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   INTEGER(I8P),                   INTENT(IN)           :: X(1:)        !< X component of data variable.
   INTEGER(I8P),                   INTENT(IN)           :: Y(1:)        !< Y component of data variable.
   INTEGER(I8P),                   INTENT(IN)           :: Z(1:)        !< Z component of data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Int64'
   N_COMPONENTS = 3
   CODE = ENCODE_BINARY_DATAARRAY(X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY3_RANK1_I8P

   FUNCTION WRITE_DATAARRAY3_RANK1_I4P(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I4P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   INTEGER(I4P),                   INTENT(IN)           :: X(1:)        !< X component of data variable.
   INTEGER(I4P),                   INTENT(IN)           :: Y(1:)        !< Y component of data variable.
   INTEGER(I4P),                   INTENT(IN)           :: Z(1:)        !< Z component of data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Int32'
   N_COMPONENTS = 3
   CODE = ENCODE_BINARY_DATAARRAY(X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY3_RANK1_I4P

   FUNCTION WRITE_DATAARRAY3_RANK1_I2P(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I2P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   INTEGER(I2P),                   INTENT(IN)           :: X(1:)        !< X component of data variable.
   INTEGER(I2P),                   INTENT(IN)           :: Y(1:)        !< Y component of data variable.
   INTEGER(I2P),                   INTENT(IN)           :: Z(1:)        !< Z component of data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Int16'
   N_COMPONENTS = 3
   CODE = ENCODE_BINARY_DATAARRAY(X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY3_RANK1_I2P

   FUNCTION WRITE_DATAARRAY3_RANK1_I1P(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I1P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   INTEGER(I1P),                   INTENT(IN)           :: X(1:)        !< X component of data variable.
   INTEGER(I1P),                   INTENT(IN)           :: Y(1:)        !< Y component of data variable.
   INTEGER(I1P),                   INTENT(IN)           :: Z(1:)        !< Z component of data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Int8'
   N_COMPONENTS = 3
   CODE = ENCODE_BINARY_DATAARRAY(X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY3_RANK1_I1P

   FUNCTION WRITE_DATAARRAY3_RANK3_R8P(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   REAL(R8P),                      INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
   REAL(R8P),                      INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
   REAL(R8P),                      INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Float64'
   N_COMPONENTS = 3
   CODE = ENCODE_BINARY_DATAARRAY(X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY3_RANK3_R8P

   FUNCTION WRITE_DATAARRAY3_RANK3_R4P(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   REAL(R4P),                      INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
   REAL(R4P),                      INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
   REAL(R4P),                      INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Float32'
   N_COMPONENTS = 3
   CODE = ENCODE_BINARY_DATAARRAY(X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY3_RANK3_R4P

   FUNCTION WRITE_DATAARRAY3_RANK3_I8P(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   INTEGER(I8P),                   INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
   INTEGER(I8P),                   INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
   INTEGER(I8P),                   INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Int64'
   N_COMPONENTS = 3
   CODE = ENCODE_BINARY_DATAARRAY(X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY3_RANK3_I8P

   FUNCTION WRITE_DATAARRAY3_RANK3_I4P(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I4P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   INTEGER(I4P),                   INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
   INTEGER(I4P),                   INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
   INTEGER(I4P),                   INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Int32'
   N_COMPONENTS = 3
   CODE = ENCODE_BINARY_DATAARRAY(X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY3_RANK3_I4P

   FUNCTION WRITE_DATAARRAY3_RANK3_I2P(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I2P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   INTEGER(I2P),                   INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
   INTEGER(I2P),                   INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
   INTEGER(I2P),                   INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Int16'
   N_COMPONENTS = 3
   CODE = ENCODE_BINARY_DATAARRAY(X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY3_RANK3_I2P

   FUNCTION WRITE_DATAARRAY3_RANK3_I1P(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I1P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   INTEGER(I1P),                   INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
   INTEGER(I1P),                   INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
   INTEGER(I1P),                   INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Int8'
   N_COMPONENTS = 3
   CODE = ENCODE_BINARY_DATAARRAY(X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY3_RANK3_I1P

   FUNCTION WRITE_DATAARRAY6_RANK1_R8P(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (R8P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   REAL(R8P),                      INTENT(IN)           :: U(1:)        !< U component of data variable.
   REAL(R8P),                      INTENT(IN)           :: V(1:)        !< V component of data variable.
   REAL(R8P),                      INTENT(IN)           :: W(1:)        !< W component of data variable.
   REAL(R8P),                      INTENT(IN)           :: X(1:)        !< X component of data variable.
   REAL(R8P),                      INTENT(IN)           :: Y(1:)        !< Y component of data variable.
   REAL(R8P),                      INTENT(IN)           :: Z(1:)        !< Z component of data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Float64'
   N_COMPONENTS = 6
   CODE = ENCODE_BINARY_DATAARRAY(U=U, V=V, W=W, X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY6_RANK1_R8P

   FUNCTION WRITE_DATAARRAY6_RANK1_R4P(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   REAL(R4P),                      INTENT(IN)           :: U(1:)        !< U component of data variable.
   REAL(R4P),                      INTENT(IN)           :: V(1:)        !< V component of data variable.
   REAL(R4P),                      INTENT(IN)           :: W(1:)        !< W component of data variable.
   REAL(R4P),                      INTENT(IN)           :: X(1:)        !< X component of data variable.
   REAL(R4P),                      INTENT(IN)           :: Y(1:)        !< Y component of data variable.
   REAL(R4P),                      INTENT(IN)           :: Z(1:)        !< Z component of data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Float32'
   N_COMPONENTS = 6
   CODE = ENCODE_BINARY_DATAARRAY(U=U, V=V, W=W, X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY6_RANK1_R4P

   FUNCTION WRITE_DATAARRAY6_RANK1_I8P(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   INTEGER(I8P),                   INTENT(IN)           :: U(1:)        !< U component of data variable.
   INTEGER(I8P),                   INTENT(IN)           :: V(1:)        !< V component of data variable.
   INTEGER(I8P),                   INTENT(IN)           :: W(1:)        !< W component of data variable.
   INTEGER(I8P),                   INTENT(IN)           :: X(1:)        !< X component of data variable.
   INTEGER(I8P),                   INTENT(IN)           :: Y(1:)        !< Y component of data variable.
   INTEGER(I8P),                   INTENT(IN)           :: Z(1:)        !< Z component of data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Int64'
   N_COMPONENTS = 6
   CODE = ENCODE_BINARY_DATAARRAY(U=U, V=V, W=W, X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY6_RANK1_I8P

   FUNCTION WRITE_DATAARRAY6_RANK1_I4P(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I4P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   INTEGER(I4P),                   INTENT(IN)           :: U(1:)        !< U component of data variable.
   INTEGER(I4P),                   INTENT(IN)           :: V(1:)        !< V component of data variable.
   INTEGER(I4P),                   INTENT(IN)           :: W(1:)        !< W component of data variable.
   INTEGER(I4P),                   INTENT(IN)           :: X(1:)        !< X component of data variable.
   INTEGER(I4P),                   INTENT(IN)           :: Y(1:)        !< Y component of data variable.
   INTEGER(I4P),                   INTENT(IN)           :: Z(1:)        !< Z component of data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Int32'
   N_COMPONENTS = 6
   CODE = ENCODE_BINARY_DATAARRAY(U=U, V=V, W=W, X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY6_RANK1_I4P

   FUNCTION WRITE_DATAARRAY6_RANK1_I2P(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I2P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   INTEGER(I2P),                   INTENT(IN)           :: U(1:)        !< U component of data variable.
   INTEGER(I2P),                   INTENT(IN)           :: V(1:)        !< V component of data variable.
   INTEGER(I2P),                   INTENT(IN)           :: W(1:)        !< W component of data variable.
   INTEGER(I2P),                   INTENT(IN)           :: X(1:)        !< X component of data variable.
   INTEGER(I2P),                   INTENT(IN)           :: Y(1:)        !< Y component of data variable.
   INTEGER(I2P),                   INTENT(IN)           :: Z(1:)        !< Z component of data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Int16'
   N_COMPONENTS = 6
   CODE = ENCODE_BINARY_DATAARRAY(U=U, V=V, W=W, X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY6_RANK1_I2P

   FUNCTION WRITE_DATAARRAY6_RANK1_I1P(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I1P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   INTEGER(I1P),                   INTENT(IN)           :: U(1:)        !< U component of data variable.
   INTEGER(I1P),                   INTENT(IN)           :: V(1:)        !< V component of data variable.
   INTEGER(I1P),                   INTENT(IN)           :: W(1:)        !< W component of data variable.
   INTEGER(I1P),                   INTENT(IN)           :: X(1:)        !< X component of data variable.
   INTEGER(I1P),                   INTENT(IN)           :: Y(1:)        !< Y component of data variable.
   INTEGER(I1P),                   INTENT(IN)           :: Z(1:)        !< Z component of data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Int8'
   N_COMPONENTS = 6
   CODE = ENCODE_BINARY_DATAARRAY(U=U, V=V, W=W, X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY6_RANK1_I1P

   FUNCTION WRITE_DATAARRAY6_RANK3_R8P(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   REAL(R8P),                      INTENT(IN)           :: U(1:,1:,1:)  !< U component of data variable.
   REAL(R8P),                      INTENT(IN)           :: V(1:,1:,1:)  !< V component of data variable.
   REAL(R8P),                      INTENT(IN)           :: W(1:,1:,1:)  !< W component of data variable.
   REAL(R8P),                      INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
   REAL(R8P),                      INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
   REAL(R8P),                      INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Float64'
   N_COMPONENTS = 6
   CODE = ENCODE_BINARY_DATAARRAY(U=U, V=V, W=W, X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY6_RANK3_R8P

   FUNCTION WRITE_DATAARRAY6_RANK3_R4P(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   REAL(R4P),                      INTENT(IN)           :: U(1:,1:,1:)  !< U component of data variable.
   REAL(R4P),                      INTENT(IN)           :: V(1:,1:,1:)  !< V component of data variable.
   REAL(R4P),                      INTENT(IN)           :: W(1:,1:,1:)  !< W component of data variable.
   REAL(R4P),                      INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
   REAL(R4P),                      INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
   REAL(R4P),                      INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Float32'
   N_COMPONENTS = 6
   CODE = ENCODE_BINARY_DATAARRAY(U=U, V=V, W=W, X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY6_RANK3_R4P

   FUNCTION WRITE_DATAARRAY6_RANK3_I8P(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   INTEGER(I8P),                   INTENT(IN)           :: U(1:,1:,1:)  !< U component of data variable.
   INTEGER(I8P),                   INTENT(IN)           :: V(1:,1:,1:)  !< V component of data variable.
   INTEGER(I8P),                   INTENT(IN)           :: W(1:,1:,1:)  !< W component of data variable.
   INTEGER(I8P),                   INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
   INTEGER(I8P),                   INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
   INTEGER(I8P),                   INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Int64'
   N_COMPONENTS = 6
   CODE = ENCODE_BINARY_DATAARRAY(U=U, V=V, W=W, X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY6_RANK3_I8P

   FUNCTION WRITE_DATAARRAY6_RANK3_I4P(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I4P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   INTEGER(I4P),                   INTENT(IN)           :: U(1:,1:,1:)  !< U component of data variable.
   INTEGER(I4P),                   INTENT(IN)           :: V(1:,1:,1:)  !< V component of data variable.
   INTEGER(I4P),                   INTENT(IN)           :: W(1:,1:,1:)  !< W component of data variable.
   INTEGER(I4P),                   INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
   INTEGER(I4P),                   INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
   INTEGER(I4P),                   INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Int32'
   N_COMPONENTS = 6
   CODE = ENCODE_BINARY_DATAARRAY(U=U, V=V, W=W, X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY6_RANK3_I4P

   FUNCTION WRITE_DATAARRAY6_RANK3_I2P(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I2P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   INTEGER(I2P),                   INTENT(IN)           :: U(1:,1:,1:)  !< U component of data variable.
   INTEGER(I2P),                   INTENT(IN)           :: V(1:,1:,1:)  !< V component of data variable.
   INTEGER(I2P),                   INTENT(IN)           :: W(1:,1:,1:)  !< W component of data variable.
   INTEGER(I2P),                   INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
   INTEGER(I2P),                   INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
   INTEGER(I2P),                   INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Int16'
   N_COMPONENTS = 6
   CODE = ENCODE_BINARY_DATAARRAY(U=U, V=V, W=W, X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY6_RANK3_I2P

   FUNCTION WRITE_DATAARRAY6_RANK3_I1P(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
   !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I1P).
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),                   INTENT(IN)           :: DATA_NAME    !< Data name.
   INTEGER(I1P),                   INTENT(IN)           :: U(1:,1:,1:)  !< U component of data variable.
   INTEGER(I1P),                   INTENT(IN)           :: V(1:,1:,1:)  !< V component of data variable.
   INTEGER(I1P),                   INTENT(IN)           :: W(1:,1:,1:)  !< W component of data variable.
   INTEGER(I1P),                   INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
   INTEGER(I1P),                   INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
   INTEGER(I1P),                   INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
   LOGICAL,                        INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NuMBEROFCOMPONENTS".
   INTEGER(I4P)                                         :: ERROR        !< Error status.
   CHARACTER(LEN=:), ALLOCATABLE                        :: DATA_TYPE    !< Data type.
   INTEGER(I4P)                                         :: N_COMPONENTS !< Number of components.
   CHARACTER(LEN=:), ALLOCATABLE                        :: CODE         !< Data variable encoded, binary or BaSE64 CODEC.

   DATA_TYPE = 'Int8'
   N_COMPONENTS = 6
   CODE = ENCODE_BINARY_DATAARRAY(U=U, V=V, W=W, X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_DATAARRAY_TAG(DATA_TYPE=DATA_TYPE, NUMBER_OF_COMPONENTS=N_COMPONENTS, DATA_NAME=DATA_NAME, DATA_CONTENT=CODE, &
                                 IS_TUPLES=IS_TUPLES)
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY6_RANK3_I1P

   SUBROUTINE WRITE_DATAARRAY_APPENDED(SELF)
   !< Do nothing, binary local is not appended.
   CLASS(XML_WRITER_BINARY_LOCAL), INTENT(INOUT) :: SELF !< Writer.
   ENDSUBROUTINE WRITE_DATAARRAY_APPENDED
ENDMODULE ModLib_XMLWriterBinary

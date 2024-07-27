!< VTK file abstract XML writer.
MODULE ModLib_XMLWriterAbstract
!< VTK file abstract XML writer.
USE ModLib_FoXy
USE ModLib_Penf
USE ModLib_Stringifor
USE ModLib_VTKParameters

IMPLICIT NONE
PRIVATE
PUBLIC :: XML_WRITER_ABSTRACT

TYPE, ABSTRACT :: XML_WRITER_ABSTRACT
  !< VTK file abstract XML writer.
  TYPE(STRING)  :: FORMAT_CH                       !< Output format, string code.
  TYPE(STRING)  :: TOPOLOGY                        !< Mesh topology.
  INTEGER(I4P)  :: INDENT=0_I4P                    !< Indent count.
  INTEGER(I8P)  :: IOFFSET=0_I8P                   !< Offset count.
  INTEGER(I4P)  :: XML=0_I4P                       !< XML Logical unit.
  INTEGER(I4P)  :: VTM_BLOCK(1:2)=[-1_I4P, -1_I4P] !< Block indexes.
  INTEGER(I4P)  :: ERROR=0_I4P                     !< IO Error status.
  TYPE(XML_TAG) :: TAG                             !< XML tags handler.
  LOGICAL       :: IS_VOLATILE=.FALSE.             !< Flag to check volatile writer.
  TYPE(STRING)  :: XML_VOLATILE                    !< XML file volatile (not a physical file).
  CONTAINS
    ! public methods (some deferred)
    PROCEDURE,                                 PASS(SELF) :: CLOSE_XML_FILE               !< Close xml file.
    PROCEDURE,                                 PASS(SELF) :: OPEN_XML_FILE                !< Open xml file.
    PROCEDURE,                                 PASS(SELF) :: FREE                         !< Free allocated meMORY.
    PROCEDURE,                                 PASS(SELF) :: GET_XML_VOLATILE             !< Return the XML voLATILE STRING FILE.
    PROCEDURE,                                 PASS(SELF) :: WRITE_CONNECTIVITY           !< Write connectivitY.
    PROCEDURE,                                 PASS(SELF) :: WRITE_DATAARRAY_LOCATION_TAG !< Write dataarray lOCATION TAG.
    PROCEDURE,                                 PASS(SELF) :: WRITE_DATAARRAY_TAG          !< Write dataarray tAG.
    PROCEDURE,                                 PASS(SELF) :: WRITE_DATAARRAY_TAG_APPENDED !< Write dataarray aPPENDED TAG.
    PROCEDURE,                                 PASS(SELF) :: WRITE_END_TAG                !< Write `</tag_name>` END TAG.
    PROCEDURE,                                 PASS(SELF) :: WRITE_HEADER_TAG             !< Write header tag.
    PROCEDURE,                                 PASS(SELF) :: WRITE_PARALLEL_OPEN_BLOCK    !< Write parallel opEN BLOCK.
    PROCEDURE,                                 PASS(SELF) :: WRITE_PARALLEL_CLOSE_BLOCK   !< Write parallel clOSE BLOCK.
    PROCEDURE,                                 PASS(SELF) :: WRITE_PARALLEL_DATAARRAY     !< Write parallel daTAARRAY.
    PROCEDURE,                                 PASS(SELF) :: WRITE_PARALLEL_GEO           !< Write parallel geO.
    PROCEDURE,                                 PASS(SELF) :: WRITE_SELF_CLOSING_TAG       !< Write self closinG TAG.
    PROCEDURE,                                 PASS(SELF) :: WRITE_START_TAG              !< Write start tag.
    PROCEDURE,                                 PASS(SELF) :: WRITE_TAG                    !< Write tag.
    PROCEDURE,                                 PASS(SELF) :: WRITE_TOPOLOGY_TAG           !< Write topology taG.
    PROCEDURE(INITIALIZE_INTERFACE), DEFERRED, PASS(SELF) :: INITIALIZE                   !< Initialize writer.
    PROCEDURE(FINALIZE_INTERFACE),   DEFERRED, PASS(SELF) :: FINALIZE                     !< Finalize writer.
    GENERIC :: WRITE_DATAARRAY =>          &
               WRITE_DATAARRAY1_RANK1_R8P, &
               WRITE_DATAARRAY1_RANK1_R4P, &
               WRITE_DATAARRAY1_RANK1_I8P, &
               WRITE_DATAARRAY1_RANK1_I4P, &
               WRITE_DATAARRAY1_RANK1_I2P, &
               WRITE_DATAARRAY1_RANK1_I1P, &
               WRITE_DATAARRAY1_RANK2_R8P, &
               WRITE_DATAARRAY1_RANK2_R4P, &
               WRITE_DATAARRAY1_RANK2_I8P, &
               WRITE_DATAARRAY1_RANK2_I4P, &
               WRITE_DATAARRAY1_RANK2_I2P, &
               WRITE_DATAARRAY1_RANK2_I1P, &
               WRITE_DATAARRAY1_RANK3_R8P, &
               WRITE_DATAARRAY1_RANK3_R4P, &
               WRITE_DATAARRAY1_RANK3_I8P, &
               WRITE_DATAARRAY1_RANK3_I4P, &
               WRITE_DATAARRAY1_RANK3_I2P, &
               WRITE_DATAARRAY1_RANK3_I1P, &
               WRITE_DATAARRAY1_RANK4_R8P, &
               WRITE_DATAARRAY1_RANK4_R4P, &
               WRITE_DATAARRAY1_RANK4_I8P, &
               WRITE_DATAARRAY1_RANK4_I4P, &
               WRITE_DATAARRAY1_RANK4_I2P, &
               WRITE_DATAARRAY1_RANK4_I1P, &
               WRITE_DATAARRAY3_RANK1_R8P, &
               WRITE_DATAARRAY3_RANK1_R4P, &
               WRITE_DATAARRAY3_RANK1_I8P, &
               WRITE_DATAARRAY3_RANK1_I4P, &
               WRITE_DATAARRAY3_RANK1_I2P, &
               WRITE_DATAARRAY3_RANK1_I1P, &
               WRITE_DATAARRAY3_RANK3_R8P, &
               WRITE_DATAARRAY3_RANK3_R4P, &
               WRITE_DATAARRAY3_RANK3_I8P, &
               WRITE_DATAARRAY3_RANK3_I4P, &
               WRITE_DATAARRAY3_RANK3_I2P, &
               WRITE_DATAARRAY3_RANK3_I1P, &
               WRITE_DATAARRAY6_RANK1_R8P, &
               WRITE_DATAARRAY6_RANK1_R4P, &
               WRITE_DATAARRAY6_RANK1_I8P, &
               WRITE_DATAARRAY6_RANK1_I4P, &
               WRITE_DATAARRAY6_RANK1_I2P, &
               WRITE_DATAARRAY6_RANK1_I1P, &
               WRITE_DATAARRAY6_RANK3_R8P, &
               WRITE_DATAARRAY6_RANK3_R4P, &
               WRITE_DATAARRAY6_RANK3_I8P, &
               WRITE_DATAARRAY6_RANK3_I4P, &
               WRITE_DATAARRAY6_RANK3_I2P, &
               WRITE_DATAARRAY6_RANK3_I1P, &
               WRITE_DATAARRAY_LOCATION_TAG !< Write data (array).
    GENERIC :: WRITE_FIELDDATA =>      &
               WRITE_FIELDDATA1_RANK0, &
               WRITE_FIELDDATA_TAG !< Write FieldData tag.
    GENERIC :: WRITE_GEO =>                    &
               WRITE_GEO_STRG_DATA1_RANK2_R8P, &
               WRITE_GEO_STRG_DATA1_RANK2_R4P, &
               WRITE_GEO_STRG_DATA1_RANK4_R8P, &
               WRITE_GEO_STRG_DATA1_RANK4_R4P, &
               WRITE_GEO_STRG_DATA3_RANK1_R8P, &
               WRITE_GEO_STRG_DATA3_RANK1_R4P, &
               WRITE_GEO_STRG_DATA3_RANK3_R8P, &
               WRITE_GEO_STRG_DATA3_RANK3_R4P, &
               WRITE_GEO_RECT_DATA3_RANK1_R8P, &
               WRITE_GEO_RECT_DATA3_RANK1_R4P, &
               WRITE_GEO_UNST_DATA1_RANK2_R8P, &
               WRITE_GEO_UNST_DATA1_RANK2_R4P, &
               WRITE_GEO_UNST_DATA3_RANK1_R8P, &
               WRITE_GEO_UNST_DATA3_RANK1_R4P !< Write mesh.
    GENERIC :: WRITE_PARALLEL_BLOCK_FILES =>     &
               WRITE_PARALLEL_BLOCK_FILE,        &
               WRITE_PARALLEL_BLOCK_FILES_ARRAY, &
               WRITE_PARALLEL_BLOCK_FILES_STRING !< Write block list of files.
    GENERIC :: WRITE_PIECE =>              &
               WRITE_PIECE_START_TAG,      &
               WRITE_PIECE_START_TAG_UNST, &
               WRITE_PIECE_END_TAG !< Write Piece start/end tag.
    ! deferred methods
    PROCEDURE(WRITE_DATAARRAY1_RANK1_R8P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY1_RANK1_R8P !< DatA 1, RANK 1, R8P.
    PROCEDURE(WRITE_DATAARRAY1_RANK1_R4P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY1_RANK1_R4P !< DatA 1, RANK 1, R4P.
    PROCEDURE(WRITE_DATAARRAY1_RANK1_I8P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY1_RANK1_I8P !< DatA 1, RANK 1, I8P.
    PROCEDURE(WRITE_DATAARRAY1_RANK1_I4P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY1_RANK1_I4P !< DatA 1, RANK 1, I4P.
    PROCEDURE(WRITE_DATAARRAY1_RANK1_I2P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY1_RANK1_I2P !< DatA 1, RANK 1, I2P.
    PROCEDURE(WRITE_DATAARRAY1_RANK1_I1P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY1_RANK1_I1P !< DatA 1, RANK 1, I1P.
    PROCEDURE(WRITE_DATAARRAY1_RANK2_R8P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY1_RANK2_R8P !< DatA 1, RANK 2, R8P.
    PROCEDURE(WRITE_DATAARRAY1_RANK2_R4P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY1_RANK2_R4P !< DatA 1, RANK 2, R4P.
    PROCEDURE(WRITE_DATAARRAY1_RANK2_I8P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY1_RANK2_I8P !< DatA 1, RANK 2, I8P.
    PROCEDURE(WRITE_DATAARRAY1_RANK2_I4P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY1_RANK2_I4P !< DatA 1, RANK 2, I4P.
    PROCEDURE(WRITE_DATAARRAY1_RANK2_I2P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY1_RANK2_I2P !< DatA 1, RANK 2, I2P.
    PROCEDURE(WRITE_DATAARRAY1_RANK2_I1P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY1_RANK2_I1P !< DatA 1, RANK 2, I1P.
    PROCEDURE(WRITE_DATAARRAY1_RANK3_R8P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY1_RANK3_R8P !< DatA 1, RANK 3, R8P.
    PROCEDURE(WRITE_DATAARRAY1_RANK3_R4P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY1_RANK3_R4P !< DatA 1, RANK 3, R4P.
    PROCEDURE(WRITE_DATAARRAY1_RANK3_I8P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY1_RANK3_I8P !< DatA 1, RANK 3, I8P.
    PROCEDURE(WRITE_DATAARRAY1_RANK3_I4P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY1_RANK3_I4P !< DatA 1, RANK 3, I4P.
    PROCEDURE(WRITE_DATAARRAY1_RANK3_I2P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY1_RANK3_I2P !< DatA 1, RANK 3, I2P.
    PROCEDURE(WRITE_DATAARRAY1_RANK3_I1P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY1_RANK3_I1P !< DatA 1, RANK 3, I1P.
    PROCEDURE(WRITE_DATAARRAY1_RANK4_R8P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY1_RANK4_R8P !< DatA 1, RANK 4, R8P.
    PROCEDURE(WRITE_DATAARRAY1_RANK4_R4P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY1_RANK4_R4P !< DatA 1, RANK 4, R4P.
    PROCEDURE(WRITE_DATAARRAY1_RANK4_I8P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY1_RANK4_I8P !< DatA 1, RANK 4, I8P.
    PROCEDURE(WRITE_DATAARRAY1_RANK4_I4P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY1_RANK4_I4P !< DatA 1, RANK 4, I4P.
    PROCEDURE(WRITE_DATAARRAY1_RANK4_I2P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY1_RANK4_I2P !< DatA 1, RANK 4, I2P.
    PROCEDURE(WRITE_DATAARRAY1_RANK4_I1P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY1_RANK4_I1P !< DatA 1, RANK 4, I1P.
    PROCEDURE(WRITE_DATAARRAY3_RANK1_R8P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY3_RANK1_R8P !< DatA 3, RANK 1, R8P.
    PROCEDURE(WRITE_DATAARRAY3_RANK1_R4P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY3_RANK1_R4P !< DatA 3, RANK 1, R4P.
    PROCEDURE(WRITE_DATAARRAY3_RANK1_I8P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY3_RANK1_I8P !< DatA 3, RANK 1, I8P.
    PROCEDURE(WRITE_DATAARRAY3_RANK1_I4P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY3_RANK1_I4P !< DatA 3, RANK 1, I4P.
    PROCEDURE(WRITE_DATAARRAY3_RANK1_I2P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY3_RANK1_I2P !< DatA 3, RANK 1, I2P.
    PROCEDURE(WRITE_DATAARRAY3_RANK1_I1P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY3_RANK1_I1P !< DatA 3, RANK 1, I1P.
    PROCEDURE(WRITE_DATAARRAY3_RANK3_R8P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY3_RANK3_R8P !< DatA 3, RANK 3, R8P.
    PROCEDURE(WRITE_DATAARRAY3_RANK3_R4P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY3_RANK3_R4P !< DatA 3, RANK 3, R4P.
    PROCEDURE(WRITE_DATAARRAY3_RANK3_I8P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY3_RANK3_I8P !< DatA 3, RANK 3, I8P.
    PROCEDURE(WRITE_DATAARRAY3_RANK3_I4P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY3_RANK3_I4P !< DatA 3, RANK 3, I4P.
    PROCEDURE(WRITE_DATAARRAY3_RANK3_I2P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY3_RANK3_I2P !< DatA 3, RANK 3, I2P.
    PROCEDURE(WRITE_DATAARRAY3_RANK3_I1P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY3_RANK3_I1P !< DatA 3, RANK 3, I1P.
    PROCEDURE(WRITE_DATAARRAY6_RANK1_R8P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY6_RANK1_R8P !< DatA 3, RANK 1, R8P.
    PROCEDURE(WRITE_DATAARRAY6_RANK1_R4P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY6_RANK1_R4P !< DatA 3, RANK 1, R4P.
    PROCEDURE(WRITE_DATAARRAY6_RANK1_I8P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY6_RANK1_I8P !< DatA 3, RANK 1, I8P.
    PROCEDURE(WRITE_DATAARRAY6_RANK1_I4P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY6_RANK1_I4P !< DatA 3, RANK 1, I4P.
    PROCEDURE(WRITE_DATAARRAY6_RANK1_I2P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY6_RANK1_I2P !< DatA 3, RANK 1, I2P.
    PROCEDURE(WRITE_DATAARRAY6_RANK1_I1P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY6_RANK1_I1P !< DatA 3, RANK 1, I1P.
    PROCEDURE(WRITE_DATAARRAY6_RANK3_R8P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY6_RANK3_R8P !< DatA 3, RANK 3, R8P.
    PROCEDURE(WRITE_DATAARRAY6_RANK3_R4P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY6_RANK3_R4P !< DatA 3, RANK 3, R4P.
    PROCEDURE(WRITE_DATAARRAY6_RANK3_I8P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY6_RANK3_I8P !< DatA 3, RANK 3, I8P.
    PROCEDURE(WRITE_DATAARRAY6_RANK3_I4P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY6_RANK3_I4P !< DatA 3, RANK 3, I4P.
    PROCEDURE(WRITE_DATAARRAY6_RANK3_I2P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY6_RANK3_I2P !< DatA 3, RANK 3, I2P.
    PROCEDURE(WRITE_DATAARRAY6_RANK3_I1P_INTERFACE), DEFERRED, PASS(SELF) :: WRITE_DATAARRAY6_RANK3_I1P !< DatA 3, RANK 3, I1P.
    PROCEDURE(WRITE_DATAARRAY_APPENDED_INTERFACE),   DEFERRED, PASS(SELF) :: WRITE_DATAARRAY_APPENDED   !< WriTE APPENDED.
    ! private methods
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_FIELDDATA1_RANK0            !< Write FieldData tag (data 1, rank 0, R8P).
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_FIELDDATA_TAG               !< Write FieldData tag.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_GEO_STRG_DATA1_RANK2_R8P    !< Write **StructuredGrid** mesh (data 1, RANK 2, R8P).
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_GEO_STRG_DATA1_RANK2_R4P    !< Write **StructuredGrid** mesh (data 1, RANK 2, R4P).
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_GEO_STRG_DATA1_RANK4_R8P    !< Write **StructuredGrid** mesh (data 1, RANK 4, R8P).
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_GEO_STRG_DATA1_RANK4_R4P    !< Write **StructuredGrid** mesh (data 1, RANK 4, R4P).
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_GEO_STRG_DATA3_RANK1_R8P    !< Write **StructuredGrid** mesh (data 3, RANK 1, R8P).
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_GEO_STRG_DATA3_RANK1_R4P    !< Write **StructuredGrid** mesh (data 3, RANK 1, R4P).
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_GEO_STRG_DATA3_RANK3_R8P    !< Write **StructuredGrid** mesh (data 3, RANK 3, R8P).
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_GEO_STRG_DATA3_RANK3_R4P    !< Write **StructuredGrid** mesh (data 3, RANK 3, R4P).
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_GEO_RECT_DATA3_RANK1_R8P    !< Write **RectilinearGrid** mesh (datA 3, RANK 1, R8P).
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_GEO_RECT_DATA3_RANK1_R4P    !< Write **RectilinearGrid** mesh (datA 3, RANK 1, R4P).
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_GEO_UNST_DATA1_RANK2_R8P    !< Write **UnstructuredGrid** mesh (daTA 1, RANK 2, R8P).
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_GEO_UNST_DATA1_RANK2_R4P    !< Write **UnstructuredGrid** mesh (daTA 1, RANK 2, R4P).
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_GEO_UNST_DATA3_RANK1_R8P    !< Write **UnstructuredGrid** mesh (daTA 3, RANK 1, R8P).
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_GEO_UNST_DATA3_RANK1_R4P    !< Write **UnstructuredGrid** mesh (daTA 3, RANK 1, R4P).
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_PIECE_START_TAG             !< Write `<Piece ...>` start tag.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_PIECE_START_TAG_UNST        !< Write `<Piece ...>` start tag for uNSTRUCTURED TOPOLOGY.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_PIECE_END_TAG               !< Write `</Piece>` end tag.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_PARALLEL_BLOCK_FILE         !< Write single file that belong to thE CURRENT BLOCK.
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_PARALLEL_BLOCK_FILES_ARRAY  !< Write block list of files (array inPUT).
    PROCEDURE, PASS(SELF), PRIVATE :: WRITE_PARALLEL_BLOCK_FILES_STRING !< Write block list of files (string iNPUT).
ENDTYPE XML_WRITER_ABSTRACT

ABSTRACT INTERFACE
  FUNCTION INITIALIZE_INTERFACE(SELF, FORMAT, FILENAME, MESH_TOPOLOGY, NX1, NX2, NY1, NY2, NZ1, NZ2, &
                                IS_VOLATILE, MESH_KIND) RESULT(ERROR)
  !< Initialize writer.
  IMPORT :: XML_WRITER_ABSTRACT, I4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF          !< Writer.
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
  ENDFUNCTION INITIALIZE_INTERFACE

   FUNCTION FINALIZE_INTERFACE(SELF) RESULT(ERROR)
   !< Finalize writer.
   IMPORT :: XML_WRITER_ABSTRACT, I4P
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF  !< Writer.
   INTEGER(I4P)                              :: ERROR !< Error status.
   ENDFUNCTION FINALIZE_INTERFACE

  FUNCTION WRITE_DATAARRAY1_RANK1_R8P_INTERFACE(SELF, DATA_NAME, X, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (R8P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P, R8P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  REAL(R8P),                  INTENT(IN)           :: X(1:)        !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY1_RANK1_R8P_INTERFACE

  FUNCTION WRITE_DATAARRAY1_RANK1_R4P_INTERFACE(SELF, DATA_NAME, X, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (R4P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P, R4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  REAL(R4P),                  INTENT(IN)           :: X(1:)        !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY1_RANK1_R4P_INTERFACE

  FUNCTION WRITE_DATAARRAY1_RANK1_I8P_INTERFACE(SELF, DATA_NAME, X, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I8P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P, I8P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I8P),               INTENT(IN)           :: X(1:)        !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY1_RANK1_I8P_INTERFACE

  FUNCTION WRITE_DATAARRAY1_RANK1_I4P_INTERFACE(SELF, DATA_NAME, X, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I4P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I4P),               INTENT(IN)           :: X(1:)        !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY1_RANK1_I4P_INTERFACE

  FUNCTION WRITE_DATAARRAY1_RANK1_I2P_INTERFACE(SELF, DATA_NAME, X, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I2P).
  IMPORT :: XML_WRITER_ABSTRACT, I2P, I4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I2P),               INTENT(IN)           :: X(1:)        !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY1_RANK1_I2P_INTERFACE

  FUNCTION WRITE_DATAARRAY1_RANK1_I1P_INTERFACE(SELF, DATA_NAME, X, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (I1P).
  IMPORT :: XML_WRITER_ABSTRACT, I1P, I4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I1P),               INTENT(IN)           :: X(1:)        !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY1_RANK1_I1P_INTERFACE

  FUNCTION WRITE_DATAARRAY1_RANK2_R8P_INTERFACE(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P, R8P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF          !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME     !< Data name.
  REAL(R8P),                  INTENT(IN)           :: X(1:,1:)      !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR         !< Error status.
  ENDFUNCTION WRITE_DATAARRAY1_RANK2_R8P_INTERFACE

  FUNCTION WRITE_DATAARRAY1_RANK2_R4P_INTERFACE(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P, R4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF          !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME     !< Data name.
  REAL(R4P),                  INTENT(IN)           :: X(1:,1:)      !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR         !< Error status.
  ENDFUNCTION WRITE_DATAARRAY1_RANK2_R4P_INTERFACE

  FUNCTION WRITE_DATAARRAY1_RANK2_I8P_INTERFACE(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P, I8P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF          !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME     !< Data name.
  INTEGER(I8P),               INTENT(IN)           :: X(1:,1:)      !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR         !< Error status.
  ENDFUNCTION WRITE_DATAARRAY1_RANK2_I8P_INTERFACE

  FUNCTION WRITE_DATAARRAY1_RANK2_I4P_INTERFACE(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF          !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME     !< Data name.
  INTEGER(I4P),               INTENT(IN)           :: X(1:,1:)      !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR         !< Error status.
  ENDFUNCTION WRITE_DATAARRAY1_RANK2_I4P_INTERFACE

  FUNCTION WRITE_DATAARRAY1_RANK2_I2P_INTERFACE(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  IMPORT :: XML_WRITER_ABSTRACT, I2P, I4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF          !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME     !< Data name.
  INTEGER(I2P),               INTENT(IN)           :: X(1:,1:)      !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR         !< Error status.
  ENDFUNCTION WRITE_DATAARRAY1_RANK2_I2P_INTERFACE

  FUNCTION WRITE_DATAARRAY1_RANK2_I1P_INTERFACE(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  IMPORT :: XML_WRITER_ABSTRACT, I1P, I4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF          !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME     !< Data name.
  INTEGER(I1P),               INTENT(IN)           :: X(1:,1:)      !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR         !< Error status.
  ENDFUNCTION WRITE_DATAARRAY1_RANK2_I1P_INTERFACE

  FUNCTION WRITE_DATAARRAY1_RANK3_R8P_INTERFACE(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P, R8P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF          !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME     !< Data name.
  REAL(R8P),                  INTENT(IN)           :: X(1:,1:,1:)   !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR         !< Error status.
  ENDFUNCTION WRITE_DATAARRAY1_RANK3_R8P_INTERFACE

  FUNCTION WRITE_DATAARRAY1_RANK3_R4P_INTERFACE(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P, R4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF          !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME     !< Data name.
  REAL(R4P),                  INTENT(IN)           :: X(1:,1:,1:)   !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR         !< Error status.
  ENDFUNCTION WRITE_DATAARRAY1_RANK3_R4P_INTERFACE

  FUNCTION WRITE_DATAARRAY1_RANK3_I8P_INTERFACE(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P, I8P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF          !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME     !< Data name.
  INTEGER(I8P),               INTENT(IN)           :: X(1:,1:,1:)   !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR         !< Error status.
  ENDFUNCTION WRITE_DATAARRAY1_RANK3_I8P_INTERFACE

  FUNCTION WRITE_DATAARRAY1_RANK3_I4P_INTERFACE(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF          !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME     !< Data name.
  INTEGER(I4P),               INTENT(IN)           :: X(1:,1:,1:)   !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR         !< Error status.
  ENDFUNCTION WRITE_DATAARRAY1_RANK3_I4P_INTERFACE

  FUNCTION WRITE_DATAARRAY1_RANK3_I2P_INTERFACE(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  IMPORT :: XML_WRITER_ABSTRACT, I2P, I4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF          !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME     !< Data name.
  INTEGER(I2P),               INTENT(IN)           :: X(1:,1:,1:)   !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR         !< Error status.
  ENDFUNCTION WRITE_DATAARRAY1_RANK3_I2P_INTERFACE

  FUNCTION WRITE_DATAARRAY1_RANK3_I1P_INTERFACE(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  IMPORT :: XML_WRITER_ABSTRACT, I1P, I4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF          !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME     !< Data name.
  INTEGER(I1P),               INTENT(IN)           :: X(1:,1:,1:)   !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES     !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR         !< Error status.
  ENDFUNCTION WRITE_DATAARRAY1_RANK3_I1P_INTERFACE

  FUNCTION WRITE_DATAARRAY1_RANK4_R8P_INTERFACE(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R8P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P, R8P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF           !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME      !< Data name.
  REAL(R8P),                  INTENT(IN)           :: X(1:,1:,1:,1:) !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT  !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES      !< Use "NumberOfTuples" instead of "NumbeROFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR          !< Error status.
  ENDFUNCTION WRITE_DATAARRAY1_RANK4_R8P_INTERFACE

  FUNCTION WRITE_DATAARRAY1_RANK4_R4P_INTERFACE(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (R4P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P, R4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF           !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME      !< Data name.
  REAL(R4P),                  INTENT(IN)           :: X(1:,1:,1:,1:) !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT  !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES      !< Use "NumberOfTuples" instead of "NumbeROFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR          !< Error status.
  ENDFUNCTION WRITE_DATAARRAY1_RANK4_R4P_INTERFACE

  FUNCTION WRITE_DATAARRAY1_RANK4_I8P_INTERFACE(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I8P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P, I8P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF           !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME      !< Data name.
  INTEGER(I8P),               INTENT(IN)           :: X(1:,1:,1:,1:) !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT  !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES      !< Use "NumberOfTuples" instead of "NumbeROFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR          !< Error status.
  ENDFUNCTION WRITE_DATAARRAY1_RANK4_I8P_INTERFACE

  FUNCTION WRITE_DATAARRAY1_RANK4_I4P_INTERFACE(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I4P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF           !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME      !< Data name.
  INTEGER(I4P),               INTENT(IN)           :: X(1:,1:,1:,1:) !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT  !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES      !< Use "NumberOfTuples" instead of "NumbeROFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR          !< Error status.
  ENDFUNCTION WRITE_DATAARRAY1_RANK4_I4P_INTERFACE

  FUNCTION WRITE_DATAARRAY1_RANK4_I2P_INTERFACE(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I2P).
  IMPORT :: XML_WRITER_ABSTRACT, I2P, I4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF           !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME      !< Data name.
  INTEGER(I2P),               INTENT(IN)           :: X(1:,1:,1:,1:) !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT  !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES      !< Use "NumberOfTuples" instead of "NumbeROFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR          !< Error status.
  ENDFUNCTION WRITE_DATAARRAY1_RANK4_I2P_INTERFACE

  FUNCTION WRITE_DATAARRAY1_RANK4_I1P_INTERFACE(SELF, DATA_NAME, X, ONE_COMPONENT, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (I1P).
  IMPORT :: XML_WRITER_ABSTRACT, I1P, I4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF           !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME      !< Data name.
  INTEGER(I1P),               INTENT(IN)           :: X(1:,1:,1:,1:) !< Data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: ONE_COMPONENT  !< Force one component.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES      !< Use "NumberOfTuples" instead of "NumbeROFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR          !< Error status.
  ENDFUNCTION WRITE_DATAARRAY1_RANK4_I1P_INTERFACE

  FUNCTION WRITE_DATAARRAY3_RANK1_R8P_INTERFACE(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P, R8P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  REAL(R8P),                  INTENT(IN)           :: X(1:)        !< X component of data variable.
  REAL(R8P),                  INTENT(IN)           :: Y(1:)        !< Y component of data variable.
  REAL(R8P),                  INTENT(IN)           :: Z(1:)        !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY3_RANK1_R8P_INTERFACE

  FUNCTION WRITE_DATAARRAY3_RANK1_R4P_INTERFACE(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P, R4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  REAL(R4P),                  INTENT(IN)           :: X(1:)        !< X component of data variable.
  REAL(R4P),                  INTENT(IN)           :: Y(1:)        !< Y component of data variable.
  REAL(R4P),                  INTENT(IN)           :: Z(1:)        !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY3_RANK1_R4P_INTERFACE

  FUNCTION WRITE_DATAARRAY3_RANK1_I8P_INTERFACE(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P, I8P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I8P),               INTENT(IN)           :: X(1:)        !< X component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: Y(1:)        !< Y component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: Z(1:)        !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY3_RANK1_I8P_INTERFACE

  FUNCTION WRITE_DATAARRAY3_RANK1_I4P_INTERFACE(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I4P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I4P),               INTENT(IN)           :: X(1:)        !< X component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: Y(1:)        !< Y component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: Z(1:)        !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY3_RANK1_I4P_INTERFACE

  FUNCTION WRITE_DATAARRAY3_RANK1_I2P_INTERFACE(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I2P).
  IMPORT :: XML_WRITER_ABSTRACT, I2P, I4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I2P),               INTENT(IN)           :: X(1:)        !< X component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: Y(1:)        !< Y component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: Z(1:)        !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY3_RANK1_I2P_INTERFACE

  FUNCTION WRITE_DATAARRAY3_RANK1_I1P_INTERFACE(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I1P).
  IMPORT :: XML_WRITER_ABSTRACT, I1P, I4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I1P),               INTENT(IN)           :: X(1:)        !< X component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: Y(1:)        !< Y component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: Z(1:)        !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY3_RANK1_I1P_INTERFACE

  FUNCTION WRITE_DATAARRAY3_RANK3_R8P_INTERFACE(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R8P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P, R8P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  REAL(R8P),                  INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
  REAL(R8P),                  INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
  REAL(R8P),                  INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY3_RANK3_R8P_INTERFACE

  FUNCTION WRITE_DATAARRAY3_RANK3_R4P_INTERFACE(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (R4P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P, R4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  REAL(R4P),                  INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
  REAL(R4P),                  INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
  REAL(R4P),                  INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY3_RANK3_R4P_INTERFACE

  FUNCTION WRITE_DATAARRAY3_RANK3_I8P_INTERFACE(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I8P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P, I8P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I8P),               INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY3_RANK3_I8P_INTERFACE

  FUNCTION WRITE_DATAARRAY3_RANK3_I4P_INTERFACE(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I4P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I4P),               INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY3_RANK3_I4P_INTERFACE

  FUNCTION WRITE_DATAARRAY3_RANK3_I2P_INTERFACE(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I2P).
  IMPORT :: XML_WRITER_ABSTRACT, I2P, I4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I2P),               INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY3_RANK3_I2P_INTERFACE

  FUNCTION WRITE_DATAARRAY3_RANK3_I1P_INTERFACE(SELF, DATA_NAME, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (I1P).
  IMPORT :: XML_WRITER_ABSTRACT, I1P, I4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I1P),               INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY3_RANK3_I1P_INTERFACE

  FUNCTION WRITE_DATAARRAY6_RANK1_R8P_INTERFACE(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (R8P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P, R8P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  REAL(R8P),                  INTENT(IN)           :: U(1:)        !< U component of data variable.
  REAL(R8P),                  INTENT(IN)           :: V(1:)        !< V component of data variable.
  REAL(R8P),                  INTENT(IN)           :: W(1:)        !< W component of data variable.
  REAL(R8P),                  INTENT(IN)           :: X(1:)        !< X component of data variable.
  REAL(R8P),                  INTENT(IN)           :: Y(1:)        !< Y component of data variable.
  REAL(R8P),                  INTENT(IN)           :: Z(1:)        !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY6_RANK1_R8P_INTERFACE

  FUNCTION WRITE_DATAARRAY6_RANK1_R4P_INTERFACE(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (R4P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P, R4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  REAL(R4P),                  INTENT(IN)           :: U(1:)        !< U component of data variable.
  REAL(R4P),                  INTENT(IN)           :: V(1:)        !< V component of data variable.
  REAL(R4P),                  INTENT(IN)           :: W(1:)        !< W component of data variable.
  REAL(R4P),                  INTENT(IN)           :: X(1:)        !< X component of data variable.
  REAL(R4P),                  INTENT(IN)           :: Y(1:)        !< Y component of data variable.
  REAL(R4P),                  INTENT(IN)           :: Z(1:)        !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY6_RANK1_R4P_INTERFACE

  FUNCTION WRITE_DATAARRAY6_RANK1_I8P_INTERFACE(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I8P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P, I8P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I8P),               INTENT(IN)           :: U(1:)        !< U component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: V(1:)        !< V component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: W(1:)        !< W component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: X(1:)        !< X component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: Y(1:)        !< Y component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: Z(1:)        !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY6_RANK1_I8P_INTERFACE

  FUNCTION WRITE_DATAARRAY6_RANK1_I4P_INTERFACE(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I4P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I4P),               INTENT(IN)           :: U(1:)        !< U component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: V(1:)        !< V component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: W(1:)        !< W component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: X(1:)        !< X component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: Y(1:)        !< Y component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: Z(1:)        !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY6_RANK1_I4P_INTERFACE

  FUNCTION WRITE_DATAARRAY6_RANK1_I2P_INTERFACE(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I2P).
  IMPORT :: XML_WRITER_ABSTRACT, I2P, I4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I2P),               INTENT(IN)           :: U(1:)        !< U component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: V(1:)        !< V component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: W(1:)        !< W component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: X(1:)        !< X component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: Y(1:)        !< Y component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: Z(1:)        !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY6_RANK1_I2P_INTERFACE

  FUNCTION WRITE_DATAARRAY6_RANK1_I1P_INTERFACE(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I1P).
  IMPORT :: XML_WRITER_ABSTRACT, I1P, I4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I1P),               INTENT(IN)           :: U(1:)        !< U component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: V(1:)        !< V component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: W(1:)        !< W component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: X(1:)        !< X component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: Y(1:)        !< Y component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: Z(1:)        !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY6_RANK1_I1P_INTERFACE

  FUNCTION WRITE_DATAARRAY6_RANK3_R8P_INTERFACE(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (R8P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P, R8P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  REAL(R8P),                  INTENT(IN)           :: U(1:,1:,1:)  !< U component of data variable.
  REAL(R8P),                  INTENT(IN)           :: V(1:,1:,1:)  !< V component of data variable.
  REAL(R8P),                  INTENT(IN)           :: W(1:,1:,1:)  !< W component of data variable.
  REAL(R8P),                  INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
  REAL(R8P),                  INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
  REAL(R8P),                  INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY6_RANK3_R8P_INTERFACE

  FUNCTION WRITE_DATAARRAY6_RANK3_R4P_INTERFACE(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (R4P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P, R4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  REAL(R4P),                  INTENT(IN)           :: U(1:,1:,1:)  !< U component of data variable.
  REAL(R4P),                  INTENT(IN)           :: V(1:,1:,1:)  !< V component of data variable.
  REAL(R4P),                  INTENT(IN)           :: W(1:,1:,1:)  !< W component of data variable.
  REAL(R4P),                  INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
  REAL(R4P),                  INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
  REAL(R4P),                  INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY6_RANK3_R4P_INTERFACE

  FUNCTION WRITE_DATAARRAY6_RANK3_I8P_INTERFACE(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I8P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P, I8P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I8P),               INTENT(IN)           :: U(1:,1:,1:)  !< U component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: V(1:,1:,1:)  !< V component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: W(1:,1:,1:)  !< W component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
  INTEGER(I8P),               INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY6_RANK3_I8P_INTERFACE

  FUNCTION WRITE_DATAARRAY6_RANK3_I4P_INTERFACE(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I4P).
  IMPORT :: XML_WRITER_ABSTRACT, I4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I4P),               INTENT(IN)           :: U(1:,1:,1:)  !< U component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: V(1:,1:,1:)  !< V component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: W(1:,1:,1:)  !< W component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
  INTEGER(I4P),               INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY6_RANK3_I4P_INTERFACE

  FUNCTION WRITE_DATAARRAY6_RANK3_I2P_INTERFACE(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I2P).
  IMPORT :: XML_WRITER_ABSTRACT, I2P, I4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I2P),               INTENT(IN)           :: U(1:,1:,1:)  !< U component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: V(1:,1:,1:)  !< V component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: W(1:,1:,1:)  !< W component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
  INTEGER(I2P),               INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY6_RANK3_I2P_INTERFACE

  FUNCTION WRITE_DATAARRAY6_RANK3_I1P_INTERFACE(SELF, DATA_NAME, U, V, W, X, Y, Z, IS_TUPLES) RESULT(ERROR)
  !< Write `<DataArray... NumberOfComponents="6"...>...</DataArray>` tag (I1P).
  IMPORT :: XML_WRITER_ABSTRACT, I1P, I4P
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
  CHARACTER(*),               INTENT(IN)           :: DATA_NAME    !< Data name.
  INTEGER(I1P),               INTENT(IN)           :: U(1:,1:,1:)  !< U component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: V(1:,1:,1:)  !< V component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: W(1:,1:,1:)  !< W component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: X(1:,1:,1:)  !< X component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: Y(1:,1:,1:)  !< Y component of data variable.
  INTEGER(I1P),               INTENT(IN)           :: Z(1:,1:,1:)  !< Z component of data variable.
  LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES    !< Use "NumberOfTuples" instead of "NumberOFCOMPONENTS".
  INTEGER(I4P)                                     :: ERROR        !< Error status.
  ENDFUNCTION WRITE_DATAARRAY6_RANK3_I1P_INTERFACE

  SUBROUTINE WRITE_DATAARRAY_APPENDED_INTERFACE(SELF)
  !< Write `<AppendedData...>...</AppendedData>` tag.
  IMPORT :: XML_WRITER_ABSTRACT
  CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF !< Writer.
  ENDSUBROUTINE WRITE_DATAARRAY_APPENDED_INTERFACE
ENDINTERFACE
CONTAINS
   ! files methods
   SUBROUTINE CLOSE_XML_FILE(SELF)
   !< Close XML file.
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF !< Writer.

   IF (.NOT.SELF%IS_VOLATILE) CLOSE(UNIT=SELF%XML, IOSTAT=SELF%ERROR)
   ENDSUBROUTINE CLOSE_XML_FILE

   SUBROUTINE OPEN_XML_FILE(SELF, FILENAME)
   !< Open XML file.
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF     !< Writer.
   CHARACTER(*),               INTENT(IN)    :: FILENAME !< File name.

   IF (.NOT.SELF%IS_VOLATILE) THEN
      OPEN(NEWUNIT=SELF%XML,             &
           FILE=TRIM(ADJUSTL(FILENAME)), &
           FORM='UNFORMATTED',           &
           ACCESS='STREAM',              &
           ACTION='WRITE',               &
           STATUS='REPLACE',             &
           IOSTAT=SELF%ERROR)
   ELSE
      SELF%XML_VOLATILE = ''
   ENDIF
   ENDSUBROUTINE OPEN_XML_FILE

   ELEMENTAL SUBROUTINE FREE(SELF, ERROR)
   !< Free allocated memory.
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)         :: SELF  !< Writer.
   INTEGER(I4P),               INTENT(OUT), OPTIONAL :: ERROR !< Error status.

   CALL SELF%FORMAT_CH%FREE
   CALL SELF%TOPOLOGY%FREE
   SELF%INDENT=0_I4P
   SELF%IOFFSET=0_I8P
   SELF%XML=0_I4P
   SELF%VTM_BLOCK(1:2)=[-1_I4P, -1_I4P]
   SELF%ERROR=0_I4P
   CALL SELF%TAG%FREE
   SELF%IS_VOLATILE=.FALSE.
   CALL SELF%XML_VOLATILE%FREE
   ENDSUBROUTINE FREE

   PURE SUBROUTINE GET_XML_VOLATILE(SELF, XML_VOLATILE, ERROR)
   !< Return the eventual XML volatile string file.
   CLASS(XML_WRITER_ABSTRACT), INTENT(IN)               :: SELF         !< Writer.
   CHARACTER(LEN=:),           INTENT(OUT), ALLOCATABLE :: XML_VOLATILE !< XML volatile file.
   INTEGER(I4P),               INTENT(OUT), OPTIONAL    :: ERROR        !< Error status.

   IF (SELF%IS_VOLATILE) THEN
      XML_VOLATILE = SELF%XML_VOLATILE%RAW
   ENDIF
   ENDSUBROUTINE GET_XML_VOLATILE

   ! tag methods
   SUBROUTINE WRITE_END_TAG(SELF, NAME)
   !< Write `</tag_name>` end tag.
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF !< Writer.
   CHARACTER(*),               INTENT(IN)    :: NAME !< Tag name.

   SELF%INDENT = SELF%INDENT - 2
   SELF%TAG = XML_TAG(NAME=NAME, INDENT=SELF%INDENT)
   IF (.NOT.SELF%IS_VOLATILE) THEN
      CALL SELF%TAG%WRITE(UNIT=SELF%XML, IOSTAT=SELF%ERROR, IS_INDENTED=.TRUE., END_RECORD=END_REC, ONLY_END=.TRUE.)
   ELSE
      SELF%XML_VOLATILE = SELF%XML_VOLATILE//SELF%TAG%STRINGIFY(IS_INDENTED=.TRUE., ONLY_END=.TRUE.)//END_REC
   ENDIF
   ENDSUBROUTINE WRITE_END_TAG

   SUBROUTINE WRITE_HEADER_TAG(SELF)
   !< Write header tag.
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF   !< Writer.
   TYPE(STRING)                              :: BUFFER !< Buffer string.

   BUFFER = '<?xml version="1.0"?>'//END_REC
   IF (ENDIAN==ENDIANL) THEN
      BUFFER = BUFFER//'<VTKFile type="'//SELF%TOPOLOGY//'" version="1.0" byte_order="LittleEndian">'
   ELSE
      BUFFER = BUFFER//'<VTKFile type="'//SELF%TOPOLOGY//'" version="1.0" byte_order="BigEndian">'
   ENDIF
   IF (.NOT.SELF%IS_VOLATILE) THEN
      WRITE(UNIT=SELF%XML, IOSTAT=SELF%ERROR)BUFFER//END_REC
   ELSE
      SELF%XML_VOLATILE = SELF%XML_VOLATILE//BUFFER//END_REC
   ENDIF
   SELF%INDENT = 2
   ENDSUBROUTINE WRITE_HEADER_TAG

   SUBROUTINE WRITE_SELF_CLOSING_TAG(SELF, NAME, ATTRIBUTES)
   !< Write `<tag_name.../>` self closing tag.
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF       !< Writer.
   CHARACTER(*),               INTENT(IN)           :: NAME       !< Tag name.
   CHARACTER(*),               INTENT(IN), OPTIONAL :: ATTRIBUTES !< Tag attributes.

   SELF%TAG = XML_TAG(NAME=NAME, ATTRIBUTES_STREAM=ATTRIBUTES, SANITIZE_ATTRIBUTES_VALUE=.TRUE., INDENT=SELF%INDENT, &
                      IS_SELF_CLOSING=.TRUE.)
   IF (.NOT.SELF%IS_VOLATILE) THEN
      CALL SELF%TAG%WRITE(UNIT=SELF%XML, IOSTAT=SELF%ERROR, IS_INDENTED=.TRUE., END_RECORD=END_REC)
   ELSE
      SELF%XML_VOLATILE = SELF%XML_VOLATILE//SELF%TAG%STRINGIFY(IS_INDENTED=.TRUE.)//END_REC
   ENDIF
   ENDSUBROUTINE WRITE_SELF_CLOSING_TAG

   SUBROUTINE WRITE_START_TAG(SELF, NAME, ATTRIBUTES)
   !< Write `<tag_name...>` start tag.
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF       !< Writer.
   CHARACTER(*),               INTENT(IN)           :: NAME       !< Tag name.
   CHARACTER(*),               INTENT(IN), OPTIONAL :: ATTRIBUTES !< Tag attributes.

   SELF%TAG = XML_TAG(NAME=NAME, ATTRIBUTES_STREAM=ATTRIBUTES, SANITIZE_ATTRIBUTES_VALUE=.TRUE., INDENT=SELF%INDENT)
   IF (.NOT.SELF%IS_VOLATILE) THEN
      CALL SELF%TAG%WRITE(UNIT=SELF%XML, IOSTAT=SELF%ERROR, IS_INDENTED=.TRUE., END_RECORD=END_REC, ONLY_START=.TRUE.)
   ELSE
      SELF%XML_VOLATILE = SELF%XML_VOLATILE//SELF%TAG%STRINGIFY(IS_INDENTED=.TRUE., ONLY_START=.TRUE.)//END_REC
   ENDIF
   SELF%INDENT = SELF%INDENT + 2
   ENDSUBROUTINE WRITE_START_TAG

   SUBROUTINE WRITE_TAG(SELF, NAME, ATTRIBUTES, CONTENT)
   !< Write `<tag_name...>...</tag_name>` tag.
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF       !< Writer.
   CHARACTER(*),               INTENT(IN)           :: NAME       !< Tag name.
   CHARACTER(*),               INTENT(IN), OPTIONAL :: ATTRIBUTES !< Tag attributes.
   CHARACTER(*),               INTENT(IN), OPTIONAL :: CONTENT    !< Tag content.

   SELF%TAG = XML_TAG(NAME=NAME, ATTRIBUTES_STREAM=ATTRIBUTES, SANITIZE_ATTRIBUTES_VALUE=.TRUE., CONTENT=CONTENT, &
                      INDENT=SELF%INDENT)
   IF (.NOT.SELF%IS_VOLATILE) THEN
      CALL SELF%TAG%WRITE(UNIT=SELF%XML, IOSTAT=SELF%ERROR, IS_INDENTED=.TRUE., IS_CONTENT_INDENTED=.TRUE., END_RECORD=END_REC)
   ELSE
      SELF%XML_VOLATILE = SELF%XML_VOLATILE//SELF%TAG%STRINGIFY(IS_INDENTED=.TRUE., IS_CONTENT_INDENTED=.TRUE.)//END_REC
   ENDIF
   ENDSUBROUTINE WRITE_TAG

   SUBROUTINE WRITE_TOPOLOGY_TAG(SELF, NX1, NX2, NY1, NY2, NZ1, NZ2, MESH_KIND)
   !< Write XML topology tag.
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF      !< Writer.
   INTEGER(I4P),               INTENT(IN), OPTIONAL :: NX1       !< Initial node of x axis.
   INTEGER(I4P),               INTENT(IN), OPTIONAL :: NX2       !< Final node of x axis.
   INTEGER(I4P),               INTENT(IN), OPTIONAL :: NY1       !< Initial node of y axis.
   INTEGER(I4P),               INTENT(IN), OPTIONAL :: NY2       !< Final node of y axis.
   INTEGER(I4P),               INTENT(IN), OPTIONAL :: NZ1       !< Initial node of z axis.
   INTEGER(I4P),               INTENT(IN), OPTIONAL :: NZ2       !< Final node of z axis.
   CHARACTER(*),               INTENT(IN), OPTIONAL :: MESH_KIND !< Kind of mesh data: Float64, Float32, ecc.
   TYPE(STRING)                                     :: BUFFER    !< Buffer string.

   BUFFER = ''
   SELECT CASE(SELF%TOPOLOGY%CHARS())
   CASE('RectilinearGrid', 'StructuredGrid')
      BUFFER = 'WholeExtent="'//                             &
               TRIM(STR(N=NX1))//' '//TRIM(STR(N=NX2))//' '//&
               TRIM(STR(N=NY1))//' '//TRIM(STR(N=NY2))//' '//&
               TRIM(STR(N=NZ1))//' '//TRIM(STR(N=NZ2))//'"'
   CASE('PRectilinearGrid', 'PStructuredGrid')
      BUFFER = 'WholeExtent="'//                             &
               TRIM(STR(N=NX1))//' '//TRIM(STR(N=NX2))//' '//&
               TRIM(STR(N=NY1))//' '//TRIM(STR(N=NY2))//' '//&
               TRIM(STR(N=NZ1))//' '//TRIM(STR(N=NZ2))//'" GhostLevel="#"'
   CASE('PUnstructuredGrid')
      BUFFER = 'GhostLevel="0"'
   ENDSELECT
   CALL SELF%WRITE_START_TAG(NAME=SELF%TOPOLOGY%CHARS(), ATTRIBUTES=BUFFER%CHARS())
   ! parallel topologies peculiars
   SELECT CASE(SELF%TOPOLOGY%CHARS())
   CASE('PRectilinearGrid')
      IF (.NOT.PRESENT(MESH_KIND)) THEN
         SELF%ERROR = 1
         RETURN
      ENDIF
      CALL SELF%WRITE_START_TAG(NAME='PCoordinates')
      CALL SELF%WRITE_SELF_CLOSING_TAG(NAME='PDataArray', ATTRIBUTES='type="'//TRIM(MESH_KIND)//'"')
      CALL SELF%WRITE_SELF_CLOSING_TAG(NAME='PDataArray', ATTRIBUTES='type="'//TRIM(MESH_KIND)//'"')
      CALL SELF%WRITE_SELF_CLOSING_TAG(NAME='PDataArray', ATTRIBUTES='type="'//TRIM(MESH_KIND)//'"')
      CALL SELF%WRITE_END_TAG(NAME='PCoordinates')
   CASE('PStructuredGrid', 'PUnstructuredGrid')
      IF (.NOT.PRESENT(MESH_KIND)) THEN
         SELF%ERROR = 1
         RETURN
      ENDIF
      CALL SELF%WRITE_START_TAG(NAME='PPoints')
      CALL SELF%WRITE_SELF_CLOSING_TAG(NAME='PDataArray', &
                                       ATTRIBUTES='type="'//TRIM(MESH_KIND)//'" NumberOfComponents="3" Name="Points"')
      CALL SELF%WRITE_END_TAG(NAME='PPoints')
   ENDSELECT
   ENDSUBROUTINE WRITE_TOPOLOGY_TAG

   ! write_dataarray
   SUBROUTINE WRITE_DATAARRAY_TAG(SELF, DATA_TYPE, NUMBER_OF_COMPONENTS, DATA_NAME, DATA_CONTENT, IS_TUPLES)
   !< Write `<DataArray...>...</DataArray>` tag.
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF                 !< Writer.
   CHARACTER(*),               INTENT(IN)           :: DATA_TYPE            !< Type of dataarray.
   INTEGER(I4P),               INTENT(IN)           :: NUMBER_OF_COMPONENTS !< Number of dataarray components.
   CHARACTER(*),               INTENT(IN)           :: DATA_NAME            !< Data name.
   CHARACTER(*),               INTENT(IN), OPTIONAL :: DATA_CONTENT         !< Data content.
   LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES            !< Use "NumberOfTuples".
   TYPE(STRING)                                     :: TAG_ATTRIBUTES       !< Tag attributes.
   LOGICAL                                          :: IS_TUPLES_           !< Use "NumberOfTuples".

   IS_TUPLES_ = .FALSE.
   IF (PRESENT(IS_TUPLES)) IS_TUPLES_ = IS_TUPLES
   IF (IS_TUPLES_) THEN
      TAG_ATTRIBUTES = 'type="'//TRIM(ADJUSTL(DATA_TYPE))//             &
        '" NumberOfTuples="'//TRIM(STR(NUMBER_OF_COMPONENTS, .TRUE.))// &
        '" Name="'//TRIM(ADJUSTL(DATA_NAME))//                          &
        '" format="'//SELF%FORMAT_CH//'"'
   ELSE
      TAG_ATTRIBUTES = 'type="'//TRIM(ADJUSTL(DATA_TYPE))//                 &
        '" NumberOfComponents="'//TRIM(STR(NUMBER_OF_COMPONENTS, .TRUE.))// &
        '" Name="'//TRIM(ADJUSTL(DATA_NAME))//                              &
        '" format="'//SELF%FORMAT_CH//'"'
   ENDIF
   CALL SELF%WRITE_TAG(NAME='DataArray', ATTRIBUTES=TAG_ATTRIBUTES%CHARS(), CONTENT=DATA_CONTENT)
   ENDSUBROUTINE WRITE_DATAARRAY_TAG

   SUBROUTINE WRITE_DATAARRAY_TAG_APPENDED(SELF, DATA_TYPE, NUMBER_OF_COMPONENTS, DATA_NAME, IS_TUPLES)
   !< Write `<DataArray.../>` tag.
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF                 !< Writer.
   CHARACTER(*),               INTENT(IN)           :: DATA_TYPE            !< Type of dataarray.
   INTEGER(I4P),               INTENT(IN)           :: NUMBER_OF_COMPONENTS !< Number of dataarray components.
   CHARACTER(*),               INTENT(IN)           :: DATA_NAME            !< Data name.
   LOGICAL,                    INTENT(IN), OPTIONAL :: IS_TUPLES            !< Use "NumberOfTuples".
   TYPE(STRING)                                     :: TAG_ATTRIBUTES       !< Tag attributes.
   LOGICAL                                          :: IS_TUPLES_           !< Use "NumberOfTuples".

   IS_TUPLES_ = .FALSE.
   IF (PRESENT(IS_TUPLES)) IS_TUPLES_ = IS_TUPLES
   IF (IS_TUPLES_) THEN
      TAG_ATTRIBUTES =  'type="'//TRIM(ADJUSTL(DATA_TYPE))//            &
        '" NumberOfTuples="'//TRIM(STR(NUMBER_OF_COMPONENTS, .TRUE.))// &
        '" Name="'//TRIM(ADJUSTL(DATA_NAME))//                          &
        '" format="'//SELF%FORMAT_CH//                                  &
        '" offset="'//TRIM(STR(SELF%IOFFSET, .TRUE.))//'"'
   ELSE
      TAG_ATTRIBUTES = 'type="'//TRIM(ADJUSTL(DATA_TYPE))//                 &
        '" NumberOfComponents="'//TRIM(STR(NUMBER_OF_COMPONENTS, .TRUE.))// &
        '" Name="'//TRIM(ADJUSTL(DATA_NAME))//                              &
        '" format="'//SELF%FORMAT_CH//                                      &
        '" offset="'//TRIM(STR(SELF%IOFFSET, .TRUE.))//'"'
   ENDIF
   CALL SELF%WRITE_SELF_CLOSING_TAG(NAME='DataArray', ATTRIBUTES=TAG_ATTRIBUTES%CHARS())
   ENDSUBROUTINE WRITE_DATAARRAY_TAG_APPENDED

   FUNCTION WRITE_DATAARRAY_LOCATION_TAG(SELF, LOCATION, ACTION) RESULT(ERROR)
   !< Write `<[/]PointData>` or `<[/]CellData>` open/close tag.
   !<
   !< @note **must** be called before saving the data related to geometric mesh, this function initializes the
   !< saving of data variables indicating the *location* (node or cell centered) of variables that will be savED.
   !<
   !< @note A single file can contain both cell and node centered variables. In this case the VTK_DAT_XML funcTION MUST BE
   !< called two times, before saving cell-centered variables and before saving node-centered variables.
   !<
   !<### Examples of usage
   !<
   !<#### Opening node piece
   !<```fortran
   !< error = vtk%write_dataarray('node','OPeN')
   !<```
   !<
   !<#### Closing node piece
   !<```fortran
   !< error = vtk%write_dataarray('node','Close')
   !<```
   !<
   !<#### Opening cell piece
   !<```fortran
   !< error = vtk%write_dataarray('cell','OPEN')
   !<```
   !<
   !<#### Closing cell piece
   !<```fortran
   !< error = vtk%write_dataarray('cell','close')
   !<```
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF      !< Writer.
   CHARACTER(*),               INTENT(IN)    :: LOCATION  !< Location of variables: **cell** or **node** centeRED.
   CHARACTER(*),               INTENT(IN)    :: ACTION    !< Action: **open** or **close** tag.
   INTEGER(I4P)                              :: ERROR     !< Error status.
   TYPE(STRING)                              :: LOCATION_ !< Location string.
   TYPE(STRING)                              :: ACTION_   !< Action string.

   LOCATION_ = TRIM(ADJUSTL(LOCATION)) ; LOCATION_ = LOCATION_%UPPER()
   ACTION_ = TRIM(ADJUSTL(ACTION)) ; ACTION_ = ACTION_%UPPER()
   SELECT CASE(LOCATION_%CHARS())
   CASE('CELL')
      LOCATION_ = 'CellData'
   CASE('NODE')
      LOCATION_ = 'PointData'
   ENDSELECT
   SELECT CASE(SELF%TOPOLOGY%CHARS())
   CASE('PRectilinearGrid', 'PStructuredGrid', 'PUnstructuredGrid')
      LOCATION_ = 'P'//LOCATION_
   ENDSELECT
   SELECT CASE(ACTION_%CHARS())
   CASE('OPEN')
      CALL SELF%WRITE_START_TAG(NAME=LOCATION_%CHARS())
   CASE('CLOSE')
      CALL SELF%WRITE_END_TAG(NAME=LOCATION_%CHARS())
   ENDSELECT
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_DATAARRAY_LOCATION_TAG

   ! write_fielddata methods
   FUNCTION WRITE_FIELDDATA1_RANK0(SELF, DATA_NAME, X) RESULT(ERROR)
   !< Write `<DataArray... NumberOfTuples="..."...>...</DataArray>` tag (R8P).
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF      !< Writer.
   CHARACTER(*),               INTENT(IN)    :: DATA_NAME !< Data name.
   CLASS(*),                   INTENT(IN)    :: X         !< Data variable.
   INTEGER(I4P)                              :: ERROR     !< Error status.

   SELECT TYPE(X)
   TYPE IS(REAL(R8P))
      SELF%ERROR = SELF%WRITE_DATAARRAY(DATA_NAME=DATA_NAME, X=[X], IS_TUPLES=.TRUE.)
   TYPE IS(REAL(R4P))
      SELF%ERROR = SELF%WRITE_DATAARRAY(DATA_NAME=DATA_NAME, X=[X], IS_TUPLES=.TRUE.)
   TYPE IS(INTEGER(I8P))
      SELF%ERROR = SELF%WRITE_DATAARRAY(DATA_NAME=DATA_NAME, X=[X], IS_TUPLES=.TRUE.)
   TYPE IS(INTEGER(I4P))
      SELF%ERROR = SELF%WRITE_DATAARRAY(DATA_NAME=DATA_NAME, X=[X], IS_TUPLES=.TRUE.)
   TYPE IS(INTEGER(I2P))
      SELF%ERROR = SELF%WRITE_DATAARRAY(DATA_NAME=DATA_NAME, X=[X], IS_TUPLES=.TRUE.)
   TYPE IS(INTEGER(I1P))
      SELF%ERROR = SELF%WRITE_DATAARRAY(DATA_NAME=DATA_NAME, X=[X], IS_TUPLES=.TRUE.)
   ENDSELECT
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_FIELDDATA1_RANK0

   FUNCTION WRITE_FIELDDATA_TAG(SELF, ACTION) RESULT(ERROR)
   !< Write `<FieldData>`/`</FieldData>` start/end tag.
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF      !< Writer.
   CHARACTER(*),               INTENT(IN)    :: ACTION    !< Action: **open** or **close** tag.
   INTEGER(I4P)                              :: ERROR     !< Error status.
   TYPE(STRING)                              :: ACTION_   !< Action string.

   ACTION_ = TRIM(ADJUSTL(ACTION)) ; ACTION_ = ACTION_%UPPER()
   SELECT CASE(ACTION_%CHARS())
   CASE('OPEN')
      CALL SELF%WRITE_START_TAG(NAME='FieldData')
   CASE('CLOSE')
      CALL SELF%WRITE_END_TAG(NAME='FieldData')
   ENDSELECT
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_FIELDDATA_TAG

   ! write_piece methods
   FUNCTION WRITE_PIECE_START_TAG(SELF, NX1, NX2, NY1, NY2, NZ1, NZ2) RESULT(ERROR)
   !< Write `<Piece ...>` start tag.
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF           !< Writer.
   INTEGER(I4P),               INTENT(IN)    :: NX1            !< Initial node of x axis.
   INTEGER(I4P),               INTENT(IN)    :: NX2            !< Final node of x axis.
   INTEGER(I4P),               INTENT(IN)    :: NY1            !< Initial node of y axis.
   INTEGER(I4P),               INTENT(IN)    :: NY2            !< Final node of y axis.
   INTEGER(I4P),               INTENT(IN)    :: NZ1            !< Initial node of z axis.
   INTEGER(I4P),               INTENT(IN)    :: NZ2            !< Final node of z axis.
   INTEGER(I4P)                              :: ERROR          !< Error status.
   TYPE(STRING)                              :: TAG_ATTRIBUTES !< Tag attributes.

   TAG_ATTRIBUTES = 'Extent="'//TRIM(STR(N=NX1))//' '//TRIM(STR(N=NX2))//' '// &
                                TRIM(STR(N=NY1))//' '//TRIM(STR(N=NY2))//' '// &
                                TRIM(STR(N=NZ1))//' '//TRIM(STR(N=NZ2))//'"'
   CALL SELF%WRITE_START_TAG(NAME='Piece', ATTRIBUTES=TAG_ATTRIBUTES%CHARS())
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_PIECE_START_TAG

   FUNCTION WRITE_PIECE_START_TAG_UNST(SELF, NP, NC) RESULT(ERROR)
   !< Write `<Piece ...>` start tag for unstructured topology.
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF           !< Writer.
   INTEGER(I4P),               INTENT(IN)    :: NP             !< Number of points.
   INTEGER(I4P),               INTENT(IN)    :: NC             !< Number of cells.
   INTEGER(I4P)                              :: ERROR          !< Error status.
   TYPE(STRING)                              :: TAG_ATTRIBUTES !< Tag attributes.

   TAG_ATTRIBUTES = 'NumberOfPoints="'//TRIM(STR(N=NP))//'" NumberOfCells="'//TRIM(STR(N=NC))//'"'
   CALL SELF%WRITE_START_TAG(NAME='Piece', ATTRIBUTES=TAG_ATTRIBUTES%CHARS())
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_PIECE_START_TAG_UNST

   FUNCTION WRITE_PIECE_END_TAG(SELF) RESULT(ERROR)
   !< Write `</Piece>` end tag.
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF  !< Writer.
   INTEGER(I4P)                              :: ERROR !< Error status.

   CALL SELF%WRITE_END_TAG(NAME='Piece')
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_PIECE_END_TAG

   ! write_geo_rect methods
   FUNCTION WRITE_GEO_RECT_DATA3_RANK1_R8P(SELF, X, Y, Z) RESULT(ERROR)
   !< Write mesh with **RectilinearGrid** topology (data 3, rank 1, R8P).
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF  !< Writer.
   REAL(R8P),                  INTENT(IN)    :: X(1:) !< X coordinates.
   REAL(R8P),                  INTENT(IN)    :: Y(1:) !< Y coordinates.
   REAL(R8P),                  INTENT(IN)    :: Z(1:) !< Z coordinates.
   INTEGER(I4P)                              :: ERROR !< Error status.

   CALL SELF%WRITE_START_TAG(NAME='Coordinates')
   ERROR = SELF%WRITE_DATAARRAY(DATA_NAME='X', X=X)
   ERROR = SELF%WRITE_DATAARRAY(DATA_NAME='Y', X=Y)
   ERROR = SELF%WRITE_DATAARRAY(DATA_NAME='Z', X=Z)
   CALL SELF%WRITE_END_TAG(NAME='Coordinates')
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_GEO_RECT_DATA3_RANK1_R8P

   FUNCTION WRITE_GEO_RECT_DATA3_RANK1_R4P(SELF, X, Y, Z) RESULT(ERROR)
   !< Write mesh with **RectilinearGrid** topology (data 3, rank 1, R4P).
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF  !< Writer.
   REAL(R4P),                  INTENT(IN)    :: X(1:) !< X coordinates.
   REAL(R4P),                  INTENT(IN)    :: Y(1:) !< Y coordinates.
   REAL(R4P),                  INTENT(IN)    :: Z(1:) !< Z coordinates.
   INTEGER(I4P)                              :: ERROR !< Error status.

   CALL SELF%WRITE_START_TAG(NAME='Coordinates')
   ERROR = SELF%WRITE_DATAARRAY(DATA_NAME='X', X=X)
   ERROR = SELF%WRITE_DATAARRAY(DATA_NAME='Y', X=Y)
   ERROR = SELF%WRITE_DATAARRAY(DATA_NAME='Z', X=Z)
   CALL SELF%WRITE_END_TAG(NAME='Coordinates')
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_GEO_RECT_DATA3_RANK1_R4P

   ! write_geo_strg methods
   FUNCTION WRITE_GEO_STRG_DATA1_RANK2_R8P(SELF, XYZ) RESULT(ERROR)
   !< Write mesh with **StructuredGrid** topology (data 1, rank 2, R8P).
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF       !< Writer.
   REAL(R8P),                  INTENT(IN)    :: XYZ(1:,1:) !< X, y, z coordinates [1:3,1:n].
   INTEGER(I4P)                              :: ERROR      !< Error status.

   CALL SELF%WRITE_START_TAG(NAME='Points')
   ERROR = SELF%WRITE_DATAARRAY(DATA_NAME='Points', X=XYZ)
   CALL SELF%WRITE_END_TAG(NAME='Points')
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_GEO_STRG_DATA1_RANK2_R8P

   FUNCTION WRITE_GEO_STRG_DATA1_RANK2_R4P(SELF, XYZ) RESULT(ERROR)
   !< Write mesh with **StructuredGrid** topology (data 1, rank 2, R4P).
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF       !< Writer.
   REAL(R4P),                  INTENT(IN)    :: XYZ(1:,1:) !< X, y, z coordinates [1:3,:].
   INTEGER(I4P)                              :: ERROR      !< Error status.

   CALL SELF%WRITE_START_TAG(NAME='Points')
   ERROR = SELF%WRITE_DATAARRAY(DATA_NAME='Points', X=XYZ)
   CALL SELF%WRITE_END_TAG(NAME='Points')
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_GEO_STRG_DATA1_RANK2_R4P

   FUNCTION WRITE_GEO_STRG_DATA1_RANK4_R8P(SELF, XYZ) RESULT(ERROR)
   !< Write mesh with **StructuredGrid** topology (data 1, rank 4, R8P).
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF             !< Writer.
   REAL(R8P),                  INTENT(IN)    :: XYZ(1:,1:,1:,1:) !< X, y, z coordinates [1:3,:,:,:].
   INTEGER(I4P)                              :: ERROR            !< Error status.

   CALL SELF%WRITE_START_TAG(NAME='Points')
   ERROR = SELF%WRITE_DATAARRAY(DATA_NAME='Points', X=XYZ)
   CALL SELF%WRITE_END_TAG(NAME='Points')
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_GEO_STRG_DATA1_RANK4_R8P

   FUNCTION WRITE_GEO_STRG_DATA1_RANK4_R4P(SELF, XYZ) RESULT(ERROR)
   !< Write mesh with **StructuredGrid** topology (data 1, rank 4, R4P).
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF             !< Writer.
   REAL(R4P),                  INTENT(IN)    :: XYZ(1:,1:,1:,1:) !< X, y, z coordinates [1:3,:,:,:].
   INTEGER(I4P)                              :: ERROR            !< Error status.

   CALL SELF%WRITE_START_TAG(NAME='Points')
   ERROR = SELF%WRITE_DATAARRAY(DATA_NAME='Points', X=XYZ)
   CALL SELF%WRITE_END_TAG(NAME='Points')
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_GEO_STRG_DATA1_RANK4_R4P

   FUNCTION WRITE_GEO_STRG_DATA3_RANK1_R8P(SELF, N, X, Y, Z) RESULT(ERROR)
   !< Write mesh with **StructuredGrid** topology (data 3, rank 1, R8P).
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF  !< Writer.
   INTEGER(I4P),               INTENT(IN)    :: N     !< Number of nodes.
   REAL(R8P),                  INTENT(IN)    :: X(1:) !< X coordinates.
   REAL(R8P),                  INTENT(IN)    :: Y(1:) !< Y coordinates.
   REAL(R8P),                  INTENT(IN)    :: Z(1:) !< Z coordinates.
   INTEGER(I4P)                              :: ERROR !< Error status.

   IF ((N/=SIZE(X, DIM=1)).OR.(N/=SIZE(Y, DIM=1)).OR.(N/=SIZE(Z, DIM=1))) THEN
      SELF%ERROR = 1
      RETURN
   ENDIF
   CALL SELF%WRITE_START_TAG(NAME='Points')
   ERROR = SELF%WRITE_DATAARRAY(DATA_NAME='Points', X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_END_TAG(NAME='Points')
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_GEO_STRG_DATA3_RANK1_R8P

   FUNCTION WRITE_GEO_STRG_DATA3_RANK1_R4P(SELF, N, X, Y, Z) RESULT(ERROR)
   !< Write mesh with **StructuredGrid** topology (data 3, rank 1, R4P).
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF  !< Writer.
   INTEGER(I4P),               INTENT(IN)    :: N     !< Number of nodes.
   REAL(R4P),                  INTENT(IN)    :: X(1:) !< X coordinates.
   REAL(R4P),                  INTENT(IN)    :: Y(1:) !< Y coordinates.
   REAL(R4P),                  INTENT(IN)    :: Z(1:) !< Z coordinates.
   INTEGER(I4P)                              :: ERROR !< Error status.

   IF ((N/=SIZE(X, DIM=1)).OR.(N/=SIZE(Y, DIM=1)).OR.(N/=SIZE(Z, DIM=1))) THEN
      SELF%ERROR = 1
      RETURN
   ENDIF
   CALL SELF%WRITE_START_TAG(NAME='Points')
   ERROR = SELF%WRITE_DATAARRAY(DATA_NAME='Points', X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_END_TAG(NAME='Points')
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_GEO_STRG_DATA3_RANK1_R4P

   FUNCTION WRITE_GEO_STRG_DATA3_RANK3_R8P(SELF, N, X, Y, Z) RESULT(ERROR)
   !< Write mesh with **StructuredGrid** topology (data 3, rank 3, R8P).
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF        !< Writer.
   INTEGER(I4P),               INTENT(IN)    :: N           !< Number of nodes.
   REAL(R8P),                  INTENT(IN)    :: X(1:,1:,1:) !< X coordinates.
   REAL(R8P),                  INTENT(IN)    :: Y(1:,1:,1:) !< Y coordinates.
   REAL(R8P),                  INTENT(IN)    :: Z(1:,1:,1:) !< Z coordinates.
   INTEGER(I4P)                              :: ERROR       !< Error status.

   IF ((N/=SIZE(X, DIM=1)*SIZE(X, DIM=2)*SIZE(X, DIM=3)).OR.&
       (N/=SIZE(Y, DIM=1)*SIZE(Y, DIM=2)*SIZE(Y, DIM=3)).OR.&
       (N/=SIZE(Z, DIM=1)*SIZE(Z, DIM=2)*SIZE(Z, DIM=3))) THEN
      SELF%ERROR = 1
      RETURN
   ENDIF
   CALL SELF%WRITE_START_TAG(NAME='Points')
   ERROR = SELF%WRITE_DATAARRAY(DATA_NAME='Points', X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_END_TAG(NAME='Points')
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_GEO_STRG_DATA3_RANK3_R8P

   FUNCTION WRITE_GEO_STRG_DATA3_RANK3_R4P(SELF, N, X, Y, Z) RESULT(ERROR)
   !< Write mesh with **StructuredGrid** topology (data 3, rank 3, R4P).
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF        !< Writer.
   INTEGER(I4P),               INTENT(IN)    :: N           !< Number of nodes.
   REAL(R4P),                  INTENT(IN)    :: X(1:,1:,1:) !< X coordinates.
   REAL(R4P),                  INTENT(IN)    :: Y(1:,1:,1:) !< Y coordinates.
   REAL(R4P),                  INTENT(IN)    :: Z(1:,1:,1:) !< Z coordinates.
   INTEGER(I4P)                              :: ERROR       !< Error status.

   IF ((N/=SIZE(X, DIM=1)*SIZE(X, DIM=2)*SIZE(X, DIM=3)).OR.&
       (N/=SIZE(Y, DIM=1)*SIZE(Y, DIM=2)*SIZE(Y, DIM=3)).OR.&
       (N/=SIZE(Z, DIM=1)*SIZE(Z, DIM=2)*SIZE(Z, DIM=3))) THEN
      SELF%ERROR = 1
      RETURN
   ENDIF
   CALL SELF%WRITE_START_TAG(NAME='Points')
   ERROR = SELF%WRITE_DATAARRAY(DATA_NAME='Points', X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_END_TAG(NAME='Points')
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_GEO_STRG_DATA3_RANK3_R4P

   ! write_geo_unst methods
   FUNCTION WRITE_GEO_UNST_DATA1_RANK2_R8P(SELF, NP, NC, XYZ) RESULT(ERROR)
   !< Write mesh with **UnstructuredGrid** topology (data 1, rank 2, R8P).
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF       !< Writer.
   INTEGER(I4P),               INTENT(IN)    :: NP         !< Number of points.
   INTEGER(I4P),               INTENT(IN)    :: NC         !< Number of cells.
   REAL(R8P),                  INTENT(IN)    :: XYZ(1:,1:) !< X, y, z coordinates [1:3,:].
   INTEGER(I4P)                              :: ERROR      !< Error status.

   IF (NP/=SIZE(XYZ, DIM=2)) THEN
      SELF%ERROR = 1
      RETURN
   ENDIF
   CALL SELF%WRITE_START_TAG(NAME='Points')
   ERROR = SELF%WRITE_DATAARRAY(DATA_NAME='Points', X=XYZ)
   CALL SELF%WRITE_END_TAG(NAME='Points')
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_GEO_UNST_DATA1_RANK2_R8P

   FUNCTION WRITE_GEO_UNST_DATA1_RANK2_R4P(SELF, NP, NC, XYZ) RESULT(ERROR)
   !< Write mesh with **UnstructuredGrid** topology (data 1, rank 2, R4P).
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF       !< Writer.
   INTEGER(I4P),               INTENT(IN)    :: NP         !< Number of points.
   INTEGER(I4P),               INTENT(IN)    :: NC         !< Number of cells.
   REAL(R4P),                  INTENT(IN)    :: XYZ(1:,1:) !< X, y, z coordinates [1:3,:].
   INTEGER(I4P)                              :: ERROR      !< Error status.

   IF (NP/=SIZE(XYZ, DIM=2)) THEN
      SELF%ERROR = 1
      RETURN
   ENDIF
   CALL SELF%WRITE_START_TAG(NAME='Points')
   ERROR = SELF%WRITE_DATAARRAY(DATA_NAME='Points', X=XYZ)
   CALL SELF%WRITE_END_TAG(NAME='Points')
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_GEO_UNST_DATA1_RANK2_R4P

   FUNCTION WRITE_GEO_UNST_DATA3_RANK1_R8P(SELF, NP, NC, X, Y, Z) RESULT(ERROR)
   !< Write mesh with **UnstructuredGrid** topology (data 3, rank 1, R8P).
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF  !< Writer.
   INTEGER(I4P),               INTENT(IN)    :: NP    !< Number of points.
   INTEGER(I4P),               INTENT(IN)    :: NC    !< Number of cells.
   REAL(R8P),                  INTENT(IN)    :: X(1:) !< X coordinates.
   REAL(R8P),                  INTENT(IN)    :: Y(1:) !< Y coordinates.
   REAL(R8P),                  INTENT(IN)    :: Z(1:) !< Z coordinates.
   INTEGER(I4P)                              :: ERROR !< Error status.

   IF ((NP/=SIZE(X, DIM=1)).OR.(NP/=SIZE(Y, DIM=1)).OR.(NP/=SIZE(Z, DIM=1))) THEN
      SELF%ERROR = 1
      RETURN
   ENDIF
   CALL SELF%WRITE_START_TAG(NAME='Points')
   ERROR = SELF%WRITE_DATAARRAY(DATA_NAME='Points', X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_END_TAG(NAME='Points')
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_GEO_UNST_DATA3_RANK1_R8P

   FUNCTION WRITE_GEO_UNST_DATA3_RANK1_R4P(SELF, NP, NC, X, Y, Z) RESULT(ERROR)
   !< Write mesh with **UnstructuredGrid** topology (data 3, rank 1, R4P).
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF  !< Writer.
   INTEGER(I4P),               INTENT(IN)    :: NP    !< Number of points.
   INTEGER(I4P),               INTENT(IN)    :: NC    !< Number of cells.
   REAL(R4P),                  INTENT(IN)    :: X(1:) !< X coordinates.
   REAL(R4P),                  INTENT(IN)    :: Y(1:) !< Y coordinates.
   REAL(R4P),                  INTENT(IN)    :: Z(1:) !< Z coordinates.
   INTEGER(I4P)                              :: ERROR !< Error status.

   IF ((NP/=SIZE(X, DIM=1)).OR.(NP/=SIZE(Y, DIM=1)).OR.(NP/=SIZE(Z, DIM=1))) THEN
      SELF%ERROR = 1
      RETURN
   ENDIF
   CALL SELF%WRITE_START_TAG(NAME='Points')
   ERROR = SELF%WRITE_DATAARRAY(DATA_NAME='Points', X=X, Y=Y, Z=Z)
   CALL SELF%WRITE_END_TAG(NAME='Points')
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_GEO_UNST_DATA3_RANK1_R4P

   FUNCTION WRITE_CONNECTIVITY(SELF, NC, CONNECTIVITY, OFFSET, CELL_TYPE, FACE, FACEOFFSET) RESULT(ERROR)
   !< Write mesh connectivity.
   !<
   !< **Must** be used when unstructured grid is used, it saves the connectivity of the unstructured gird.
   !< @note The vector **connect** must follow the VTK-XML standard. It is passed as *assumed-shape array*
   !< because its dimensions is related to the mesh dimensions in a complex way. Its dimensions can be calculaTED BY THE FOLLOWING
   !< equation: \(dc = \sum\limits_{i = 1}^{NC} {nvertex_i }\).
   !< Note that this equation is different from the legacy one. The XML connectivity convention is quite diffeRENT FROM THE
   !< legacy standard.
   !< As an example suppose we have a mesh composed by 2 cells, one hexahedron (8 vertices) and one pyramid wiTH
   !< square basis (5 vertices) and suppose that the basis of pyramid is constitute by a face of the hexahedroN AND SO THE TWO CELLS
   !< share 4 vertices. The above equation gives \(dc=8+5=13\). The connectivity vector for this mesh can be:
   !<
   !<##### first cell
   !<+ connect(1)  = 0 identification flag of \(1^\circ\) vertex of first cell
   !<+ connect(2)  = 1 identification flag of \(2^\circ\) vertex of first cell
   !<+ connect(3)  = 2 identification flag of \(3^\circ\) vertex of first cell
   !<+ connect(4)  = 3 identification flag of \(4^\circ\) vertex of first cell
   !<+ connect(5)  = 4 identification flag of \(5^\circ\) vertex of first cell
   !<+ connect(6)  = 5 identification flag of \(6^\circ\) vertex of first cell
   !<+ connect(7)  = 6 identification flag of \(7^\circ\) vertex of first cell
   !<+ connect(8)  = 7 identification flag of \(8^\circ\) vertex of first cell
   !<
   !<##### second cell
   !<+ connect(9 ) = 0 identification flag of \(1^\circ\) vertex of second cell
   !<+ connect(10) = 1 identification flag of \(2^\circ\) vertex of second cell
   !<+ connect(11) = 2 identification flag of \(3^\circ\) vertex of second cell
   !<+ connect(12) = 3 identification flag of \(4^\circ\) vertex of second cell
   !<+ connect(13) = 8 identification flag of \(5^\circ\) vertex of second cell
   !<
   !< Therefore this connectivity vector convention is more simple than the legacy convention, now we must creATE ALSO THE
   !< *offset* vector that contains the data now missing in the *connect* vector. The offset
   !< vector for this mesh can be:
   !<
   !<##### first cell
   !<+ offset(1) = 8  => summ of nodes of \(1^\circ\) cell
   !<
   !<##### second cell
   !<+ offset(2) = 13 => summ of nodes of \(1^\circ\) and \(2^\circ\) cells
   !<
   !< The value of every cell-offset can be calculated by the following equation: \(offset_c=\sum\limits_{i=1}^{C}{NVERTEX_I}\)
   !< where \(offset_c\) is the value of \(c^{th}\) cell and \(nvertex_i\) is the number of vertices of \(i^{tH}\) CELL.
   !< The function VTK_CON_XML does not calculate the connectivity and offset vectors: it writes the connectivITY AND OFFSET
   !< vectors conforming the VTK-XML standard, but does not calculate them.
   !< The vector variable *cell\_type* must conform the VTK-XML standard (see the file VTK-Standard at the
   !< Kitware homepage) that is the same of the legacy standard. It contains the
   !< *type* of each cells. For the above example this vector is:
   !<
   !<##### first cell
   !<+ cell\_type(1) = 12 hexahedron type of first cell
   !<
   !<##### second cell
   !<+ cell\_type(2) = 14 pyramid type of second cell
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF             !< Writer.
   INTEGER(I4P),               INTENT(IN)    :: NC               !< Number of cells.
   INTEGER(I4P),               INTENT(IN)    :: CONNECTIVITY(1:) !< Mesh connectivity.
   INTEGER(I4P),               INTENT(IN)    :: OFFSET(1:)       !< Cell offset.
   INTEGER(I4P),   OPTIONAL,   INTENT(IN)    :: FACE(1:)         !< face composing the polyhedra.
   INTEGER(I4P),   OPTIONAL,   INTENT(IN)    :: FACEOFFSET(1:)   !< face offset.
   INTEGER(I1P),               INTENT(IN)    :: CELL_TYPE(1:)    !< VTK cell type.
   INTEGER(I4P)                              :: ERROR            !< Error status.

   CALL SELF%WRITE_START_TAG(NAME='Cells')
   ERROR = SELF%WRITE_DATAARRAY(DATA_NAME='connectivity', X=CONNECTIVITY)
   ERROR = SELF%WRITE_DATAARRAY(DATA_NAME='offsets', X=OFFSET)
   ERROR = SELF%WRITE_DATAARRAY(DATA_NAME='types', X=CELL_TYPE)
   CALL SELF%WRITE_END_TAG(NAME='Cells')

   !< Add faces and faceoffsets to the cell block for polyhedra. If the cell is not a polyhedron, its offset mUST BE SET TO -1.
   IF(PRESENT(FACE).AND. PRESENT(FACEOFFSET)) THEN
        ERROR = SELF%WRITE_DATAARRAY(DATA_NAME='faces', X=FACE)
        ERROR = SELF%WRITE_DATAARRAY(DATA_NAME='faceoffsets', X=FACEOFFSET)
   ENDIF
   ENDFUNCTION WRITE_CONNECTIVITY

   ! write_parallel methods
   FUNCTION WRITE_PARALLEL_OPEN_BLOCK(SELF, NAME) RESULT(ERROR)
   !< Write a block (open) container.
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF   !< Writer.
   CHARACTER(*),               INTENT(IN), OPTIONAL :: NAME   !< Block name.
   INTEGER(I4P)                                     :: ERROR  !< Error status.
   TYPE(STRING)                                     :: BUFFER !< Buffer string.

   SELF%VTM_BLOCK = SELF%VTM_BLOCK + 1
   IF (PRESENT(NAME)) THEN
      BUFFER = 'index="'//TRIM(STR((SELF%VTM_BLOCK(1) + SELF%VTM_BLOCK(2)),.TRUE.))//'" name="'//TRIM(ADJUSTL(NAME))//'"'
   ELSE
      BUFFER = 'index="'//TRIM(STR((SELF%VTM_BLOCK(1) + SELF%VTM_BLOCK(2)),.TRUE.))//'"'
   ENDIF
   CALL SELF%WRITE_START_TAG(NAME='Block', ATTRIBUTES=BUFFER%CHARS())
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_PARALLEL_OPEN_BLOCK

   FUNCTION WRITE_PARALLEL_CLOSE_BLOCK(SELF) RESULT(ERROR)
   !< Close a block container.
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT) :: SELF  !< Writer.
   INTEGER(I4P)                              :: ERROR !< Error status.

   SELF%VTM_BLOCK(2) = -1
   CALL SELF%WRITE_END_TAG(NAME='Block')
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_PARALLEL_CLOSE_BLOCK

   FUNCTION WRITE_PARALLEL_DATAARRAY(SELF, DATA_NAME, DATA_TYPE, NUMBER_OF_COMPONENTS) RESULT(ERROR)
   !< Write parallel (partitioned) VTK-XML dataarray info.
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF                 !< Writer.
   CHARACTER(*),               INTENT(IN)           :: DATA_NAME            !< Data name.
   CHARACTER(*),               INTENT(IN)           :: DATA_TYPE            !< Type of dataarray.
   INTEGER(I4P),               INTENT(IN), OPTIONAL :: NUMBER_OF_COMPONENTS !< Number of dataarray components.
   INTEGER(I4P)                                     :: ERROR                !< Error status.
   TYPE(STRING)                                     :: BUFFER               !< Buffer string.

   IF (PRESENT(NUMBER_OF_COMPONENTS)) THEN
      BUFFER = 'type="'//TRIM(ADJUSTL(DATA_TYPE))//'" Name="'//TRIM(ADJUSTL(DATA_NAME))//&
               '" NumberOfComponents="'//TRIM(STR(NUMBER_OF_COMPONENTS, .TRUE.))//'"'
   ELSE
      BUFFER = 'type="'//TRIM(ADJUSTL(DATA_TYPE))//'" Name="'//TRIM(ADJUSTL(DATA_NAME))//'"'
   ENDIF
   CALL SELF%WRITE_SELF_CLOSING_TAG(NAME='PDataArray', ATTRIBUTES=BUFFER%CHARS())
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_PARALLEL_DATAARRAY

   FUNCTION WRITE_PARALLEL_GEO(SELF, SOURCE, NX1, NX2, NY1, NY2, NZ1, NZ2) RESULT(ERROR)
   !< Write parallel (partitioned) VTK-XML geo source file.
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF   !< Writer.
   CHARACTER(*),               INTENT(IN)           :: SOURCE !< Source file name containing the piece data.
   INTEGER(I4P),               INTENT(IN), OPTIONAL :: NX1    !< Initial node of x axis.
   INTEGER(I4P),               INTENT(IN), OPTIONAL :: NX2    !< Final node of x axis.
   INTEGER(I4P),               INTENT(IN), OPTIONAL :: NY1    !< Initial node of y axis.
   INTEGER(I4P),               INTENT(IN), OPTIONAL :: NY2    !< Final node of y axis.
   INTEGER(I4P),               INTENT(IN), OPTIONAL :: NZ1    !< Initial node of z axis.
   INTEGER(I4P),               INTENT(IN), OPTIONAL :: NZ2    !< Final node of z axis.
   INTEGER(I4P)                                     :: ERROR  !< Error status.
   TYPE(STRING)                                     :: BUFFER !< Buffer string.

   SELECT CASE (SELF%TOPOLOGY%CHARS())
   CASE('PRectilinearGrid', 'PStructuredGrid')
      BUFFER = 'Extent="'// &
               TRIM(STR(N=NX1))//' '//TRIM(STR(N=NX2))//' '// &
               TRIM(STR(N=NY1))//' '//TRIM(STR(N=NY2))//' '// &
               TRIM(STR(N=NZ1))//' '//TRIM(STR(N=NZ2))//'" Source="'//TRIM(ADJUSTL(SOURCE))//'"'
   CASE('PUnstructuredGrid')
      BUFFER = 'Source="'//TRIM(ADJUSTL(SOURCE))//'"'
   ENDSELECT
   CALL SELF%WRITE_SELF_CLOSING_TAG(NAME='Piece', ATTRIBUTES=BUFFER%CHARS())
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_PARALLEL_GEO

   FUNCTION WRITE_PARALLEL_BLOCK_FILE(SELF, FILE_INDEX, FILENAME, NAME) RESULT(ERROR)
   !< Write single file that belong to the current block.
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF       !< Writer.
   INTEGER(I4P),               INTENT(IN)           :: FILE_INDEX !< Index of file in the list.
   CHARACTER(*),               INTENT(IN)           :: FILENAME   !< Wrapped file names.
   CHARACTER(*),               INTENT(IN), OPTIONAL :: NAME       !< Names attributed to wrapped file.
   INTEGER(I4P)                                     :: ERROR      !< Error status.

   IF (PRESENT(NAME)) THEN
      CALL SELF%WRITE_SELF_CLOSING_TAG(NAME='DataSet',                                      &
                                       ATTRIBUTES='index="'//TRIM(STR(FILE_INDEX, .TRUE.))//&
                                                 '" file="'//TRIM(ADJUSTL(FILENAME))//      &
                                                 '" name="'//TRIM(ADJUSTL(NAME))//'"')
   ELSE
      CALL SELF%WRITE_SELF_CLOSING_TAG(NAME='DataSet',                                       &
                                       ATTRIBUTES='index="'//TRIM(STR(FILE_INDEX, .TRUE.))// &
                                                 '" file="'//TRIM(ADJUSTL(FILENAME))//'"')
   ENDIF
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_PARALLEL_BLOCK_FILE

   FUNCTION WRITE_PARALLEL_BLOCK_FILES_ARRAY(SELF, FILENAMES, NAMES) RESULT(ERROR)
   !< Write list of files that belong to the current block (list passed as rank 1 array).
   !<
   !<#### Example of usage: 3 files blocks
   !<```fortran
   !< error = vtm%write_files_list_of_block(filenames=['file_1.vts','file_2.vts','file_3.vtu'])
   !<```
   !<
   !<#### Example of usage: 3 files blocks with custom name
   !<```fortran
   !< error = vtm%write_files_list_of_block(filenames=['file_1.vts','file_2.vts','file_3.vtu'],&
   !<                                       names=['block-bar','block-foo','block-baz'])
   !<```
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF         !< Writer.
   CHARACTER(*),               INTENT(IN)           :: FILENAMES(:) !< List of VTK-XML wrapped file names.
   CHARACTER(*),               INTENT(IN), OPTIONAL :: NAMES(:)     !< List names attributed to wrapped files.
   INTEGER(I4P)                                     :: ERROR        !< Error status.
   INTEGER(I4P)                                     :: F            !< File counter.

   IF (PRESENT(NAMES)) THEN
      IF (SIZE(NAMES, DIM=1)==SIZE(FILENAMES, DIM=1)) THEN
         DO F=1, SIZE(FILENAMES, DIM=1)
            CALL SELF%WRITE_SELF_CLOSING_TAG(NAME='DataSet',                                     &
                                             ATTRIBUTES='index="'//TRIM(STR(F-1, .TRUE.))//      &
                                                       '" file="'//TRIM(ADJUSTL(FILENAMES(F)))// &
                                                       '" name="'//TRIM(ADJUSTL(NAMES(F)))//'"')
         ENDDO
      ENDIF
   ELSE
      DO F=1,SIZE(FILENAMES, DIM=1)
         CALL SELF%WRITE_SELF_CLOSING_TAG(NAME='DataSet',                                &
                                          ATTRIBUTES='index="'//TRIM(STR(F-1, .TRUE.))// &
                                                    '" file="'//TRIM(ADJUSTL(FILENAMES(F)))//'"')
      ENDDO
   ENDIF
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_PARALLEL_BLOCK_FILES_ARRAY

   FUNCTION WRITE_PARALLEL_BLOCK_FILES_STRING(SELF, FILENAMES, NAMES, DELIMITER) RESULT(ERROR)
   !< Write list of files that belong to the current block (list passed as single string).
   !<
   !<#### Example of usage: 3 files blocks
   !<```fortran
   !< error = vtm%write_files_list_of_block(filenames='file_1.vts file_2.vts file_3.vtu')
   !<```
   !<
   !<#### Example of usage: 3 files blocks with custom name
   !<```fortran
   !< error = vtm%write_files_list_of_block(filenames='file_1.vts file_2.vts file_3.vtu',&
   !<                                       names='block-bar block-foo block-baz')
   !<```
   CLASS(XML_WRITER_ABSTRACT), INTENT(INOUT)        :: SELF          !< Writer.
   CHARACTER(*),               INTENT(IN)           :: FILENAMES     !< List of VTK-XML wrapped file names.
   CHARACTER(*),               INTENT(IN), OPTIONAL :: NAMES         !< List names attributed to wrapped files.
   CHARACTER(*),               INTENT(IN), OPTIONAL :: DELIMITER     !< Delimiter character.
   INTEGER(I4P)                                     :: ERROR         !< Error status.
   TYPE(STRING), ALLOCATABLE                        :: FILENAMES_(:) !< List of VTK-XML wrapped file names.
   TYPE(STRING), ALLOCATABLE                        :: NAMES_(:)     !< List names attributed to wrapped files.
   TYPE(STRING)                                     :: DELIMITER_    !< Delimiter character.
   TYPE(STRING)                                     :: BUFFER        !< A string buffer.
   INTEGER(I4P)                                     :: F             !< File counter.

   DELIMITER_ = ' ' ; IF (PRESENT(DELIMITER)) DELIMITER_ = DELIMITER
   BUFFER = FILENAMES
   CALL BUFFER%SPLIT(TOKENS=FILENAMES_, SEP=DELIMITER_%CHARS())
   IF (PRESENT(NAMES)) THEN
      BUFFER = NAMES
      CALL BUFFER%SPLIT(TOKENS=NAMES_, SEP=DELIMITER_%CHARS())
      IF (SIZE(NAMES_, DIM=1)==SIZE(FILENAMES_, DIM=1)) THEN
         DO F=1, SIZE(FILENAMES_, DIM=1)
            CALL SELF%WRITE_SELF_CLOSING_TAG(NAME='DataSet',                                      &
                                             ATTRIBUTES='index="'//TRIM(STR(F-1, .TRUE.))//       &
                                                       '" file="'//TRIM(ADJUSTL(FILENAMES_(F)))// &
                                                       '" name="'//TRIM(ADJUSTL(NAMES_(F)))//'"')
         ENDDO
      ENDIF
   ELSE
      DO F=1,SIZE(FILENAMES_, DIM=1)
         CALL SELF%WRITE_SELF_CLOSING_TAG(NAME='DataSet',                               &
                                          ATTRIBUTES='index="'//TRIM(STR(F-1,.TRUE.))// &
                                                    '" file="'//TRIM(ADJUSTL(FILENAMES_(F)))//'"')
      ENDDO
   ENDIF
   ERROR = SELF%ERROR
   ENDFUNCTION WRITE_PARALLEL_BLOCK_FILES_STRING
ENDMODULE ModLib_XMLWriterAbstract

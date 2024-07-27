!< VTM file class.
MODULE ModLib_VTMFile
!< VTM file class.
USE ModLib_BeFor64
USE ModLib_Penf
USE ModLib_Stringifor
USE ModLib_XMLWriterAbstract
USE ModLib_XMLWriterASCII

IMPLICIT NONE
PRIVATE
PUBLIC :: VTM_FILE

TYPE :: VTM_FILE
   !< VTM file class.
   CLASS(XML_WRITER_ABSTRACT), ALLOCATABLE, PUBLIC :: XML_WRITER      !< XML writer.
   INTEGER(I4P), ALLOCATABLE                       :: SCRATCH_UNIT(:) !< Scratch units for very large list of NAMED BLOCKS.
   CONTAINS
      ! public methods
      PROCEDURE, PASS(SELF) :: INITIALIZE          !< Initialize file.
      PROCEDURE, PASS(SELF) :: FINALIZE            !< Finalize file.
      GENERIC               :: WRITE_BLOCK =>      &
                               WRITE_BLOCK_ARRAY,  &
                               WRITE_BLOCK_STRING, &
                               WRITE_BLOCK_SCRATCH !< Write one block dataset.
      ! private methods
      PROCEDURE, PASS(SELF), PRIVATE :: WRITE_BLOCK_ARRAY  !< Write one block dataset (array input).
      PROCEDURE, PASS(SELF), PRIVATE :: WRITE_BLOCK_STRING !< Write one block dataset (string input).
      ! scratch files methods``
      PROCEDURE, PASS(SELF), PRIVATE :: PARSE_SCRATCH_FILES !< Parse scratch files.
      PROCEDURE, PASS(SELF), PRIVATE :: WRITE_BLOCK_SCRATCH !< Write one block dataset on scratch files.
ENDTYPE VTM_FILE
CONTAINS
  ! public methods
  FUNCTION INITIALIZE(SELF, FILENAME, SCRATCH_UNITS_NUMBER) RESULT(ERROR)
  !< Initialize file (writer).
  CLASS(VTM_FILE), INTENT(INOUT)        :: SELF                  !< VTM file.
  CHARACTER(*),    INTENT(IN)           :: FILENAME              !< File name of output VTM file.
  INTEGER(I4P),    INTENT(IN), OPTIONAL :: SCRATCH_UNITS_NUMBER  !< Number of scratch units for very large lisT OF NAMED BLOCKS.
  INTEGER(I4P)                          :: SCRATCH_UNITS_NUMBER_ !< Number of scratch units for very large lisT OF NAMED BLOCKS.
  INTEGER(I4P)                          :: ERROR                 !< Error status.

  IF (.NOT.IS_INITIALIZED) CALL PENF_INIT
  IF (.NOT.IS_B64_INITIALIZED) CALL B64_INIT
  SCRATCH_UNITS_NUMBER_ = 0_I4P ; IF (PRESENT(SCRATCH_UNITS_NUMBER)) SCRATCH_UNITS_NUMBER_ = SCRATCH_UNITS_NUMBER
  ERROR = SELF%FINALIZE()
  IF (ALLOCATED(SELF%XML_WRITER)) DEALLOCATE(SELF%XML_WRITER)
  ALLOCATE(XML_WRITER_ASCII_LOCAL :: SELF%XML_WRITER)
  ERROR = SELF%XML_WRITER%INITIALIZE(FORMAT='ascii', FILENAME=FILENAME, MESH_TOPOLOGY='vtkMultiBlockDataSet')
  IF (SCRATCH_UNITS_NUMBER_>0_I4P) ALLOCATE(SELF%SCRATCH_UNIT(SCRATCH_UNITS_NUMBER_))
  ENDFUNCTION INITIALIZE

  FUNCTION FINALIZE(SELF) RESULT(ERROR)
  !< Finalize file (writer).
  CLASS(VTM_FILE), INTENT(INOUT) :: SELF  !< VTM file.
  INTEGER(I4P)                   :: ERROR !< Error status.

  ERROR = 1
  IF (ALLOCATED(SELF%SCRATCH_UNIT)) THEN
     ERROR = SELF%PARSE_SCRATCH_FILES()
     DEALLOCATE(SELF%SCRATCH_UNIT)
  ENDIF
  IF (ALLOCATED(SELF%XML_WRITER)) ERROR = SELF%XML_WRITER%FINALIZE()
  ENDFUNCTION FINALIZE

  ! private methods
  FUNCTION WRITE_BLOCK_ARRAY(SELF, FILENAMES, NAMES, NAME) RESULT(ERROR)
  !< Write one block dataset (array input).
  !<
  !<#### Example of usage: 3 files blocks
  !<```fortran
  !< error = vtm%write_block(filenames=['file_1.vts', 'file_2.vts', 'file_3.vtu'], name='my_block')
  !<```
  !<
  !<#### Example of usage: 3 files blocks with custom name
  !<```fortran
  !< error = vtm%write_block(filenames=['file_1.vts', 'file_2.vts', 'file_3.vtu'], &
  !<                         names=['block-bar', 'block-foo', 'block-baz'], name='my_block')
  !<```
  CLASS(VTM_FILE), INTENT(INOUT)        :: SELF          !< VTM file.
  CHARACTER(*),    INTENT(IN)           :: FILENAMES(1:) !< File names of VTK files grouped into current block.
  CHARACTER(*),    INTENT(IN), OPTIONAL :: NAMES(1:)     !< Auxiliary names attributed to each files.
  CHARACTER(*),    INTENT(IN), OPTIONAL :: NAME          !< Block name
  INTEGER(I4P)                          :: ERROR         !< Error status.

  ERROR = SELF%XML_WRITER%WRITE_PARALLEL_OPEN_BLOCK(NAME=NAME)
  ERROR = SELF%XML_WRITER%WRITE_PARALLEL_BLOCK_FILES(FILENAMES=FILENAMES, NAMES=NAMES)
  ERROR = SELF%XML_WRITER%WRITE_PARALLEL_CLOSE_BLOCK()
  ENDFUNCTION WRITE_BLOCK_ARRAY

   FUNCTION WRITE_BLOCK_STRING(SELF, ACTION, FILENAMES, NAMES, NAME) RESULT(ERROR)
   !< Write one block dataset (string input).
   !<
   !<#### Example of usage: 3 files blocks
   !<```fortran
   !< error = vtm%write_block(filenames='file_1.vts file_2.vts file_3.vtu', name='my_block')
   !<```
   !<
   !<#### Example of usage: 3 files blocks with custom name
   !<```fortran
   !< error = vtm%write_block(filenames='file_1.vts file_2.vts file_3.vtu', names='block-bar block-foo block-bAZ', NAME='MY_BLOCK')
   !<```
   CLASS(VTM_FILE), INTENT(INOUT)        :: SELF      !< VTM file.
   CHARACTER(*),    INTENT(IN), OPTIONAL :: ACTION    !< Action: [open, close, write].
   CHARACTER(*),    INTENT(IN), OPTIONAL :: FILENAMES !< File names of VTK files grouped into current block.
   CHARACTER(*),    INTENT(IN), OPTIONAL :: NAMES     !< Auxiliary names attributed to each files.
   CHARACTER(*),    INTENT(IN), OPTIONAL :: NAME      !< Block name
   INTEGER(I4P)                          :: ERROR     !< Error status.
   TYPE(STRING)                          :: ACTION_   !< Action string.

   IF (PRESENT(ACTION)) THEN
      ACTION_ = TRIM(ADJUSTL(ACTION)) ; ACTION_ = ACTION_%UPPER()
      SELECT CASE(ACTION_%CHARS())
      CASE('OPEN')
         ERROR = SELF%XML_WRITER%WRITE_PARALLEL_OPEN_BLOCK(NAME=NAME)
      CASE('CLOSE')
         ERROR = SELF%XML_WRITER%WRITE_PARALLEL_CLOSE_BLOCK()
      CASE('WRITE')
         IF (PRESENT(FILENAMES)) ERROR = SELF%XML_WRITER%WRITE_PARALLEL_BLOCK_FILES(FILENAMES=FILENAMES, NAMES=NAMES)
      ENDSELECT
   ELSE
      ERROR = SELF%XML_WRITER%WRITE_PARALLEL_OPEN_BLOCK(NAME=NAME)
      ERROR = SELF%XML_WRITER%WRITE_PARALLEL_BLOCK_FILES(FILENAMES=FILENAMES, NAMES=NAMES)
      ERROR = SELF%XML_WRITER%WRITE_PARALLEL_CLOSE_BLOCK()
   ENDIF
   ENDFUNCTION WRITE_BLOCK_STRING

   ! scratch files methods
   FUNCTION PARSE_SCRATCH_FILES(SELF) RESULT(ERROR)
   !< Parse scratch files.
   CLASS(VTM_FILE), INTENT(INOUT) :: SELF     !< VTM file.
   INTEGER(I4P)                   :: ERROR    !< Error status.
   CHARACTER(9999)                :: FILENAME !< File name of VTK file grouped into current block.
   CHARACTER(9999)                :: NAME     !< Block name
   INTEGER(I4P)                   :: S, F     !< Counter.

   IF (ALLOCATED(SELF%SCRATCH_UNIT)) THEN
      DO S=1, SIZE(SELF%SCRATCH_UNIT, DIM=1)
         ! rewind scratch file
         REWIND(SELF%SCRATCH_UNIT(S))
         ! write group name
         F = 0_I4P
         READ(SELF%SCRATCH_UNIT(S), IOSTAT=ERROR, FMT=*) NAME
         ERROR = SELF%WRITE_BLOCK(ACTION='open', NAME=TRIM(ADJUSTL(NAME)))
         ! write group filenames
         PARSE_FILE_LOOP : DO
            READ(SELF%SCRATCH_UNIT(S), IOSTAT=ERROR, FMT=*) FILENAME
            IF (IS_IOSTAT_END(ERROR)) EXIT PARSE_FILE_LOOP
            ERROR = SELF%XML_WRITER%WRITE_PARALLEL_BLOCK_FILES(FILE_INDEX=F,                     &
                                                               FILENAME=TRIM(ADJUSTL(FILENAME)), &
                                                               NAME=TRIM(ADJUSTL(FILENAME)))
            F = F + 1_I4P
         ENDDO PARSE_FILE_LOOP
         ! close group
         ERROR = SELF%WRITE_BLOCK(ACTION='close')
         ! close scratch file
         CLOSE(SELF%SCRATCH_UNIT(S))
      ENDDO
   ENDIF
   ENDFUNCTION PARSE_SCRATCH_FILES

   FUNCTION WRITE_BLOCK_SCRATCH(SELF, SCRATCH, ACTION, FILENAME, NAME) RESULT(ERROR)
   !< Write one block dataset on scratch files.
   CLASS(VTM_FILE), INTENT(INOUT)        :: SELF     !< VTM file.
   INTEGER(I4P),    INTENT(IN)           :: SCRATCH  !< Scratch unit.
   CHARACTER(*),    INTENT(IN)           :: ACTION   !< Action: [open, write].
   CHARACTER(*),    INTENT(IN), OPTIONAL :: FILENAME !< File name of VTK file grouped into current block.
   CHARACTER(*),    INTENT(IN), OPTIONAL :: NAME     !< Block name
   INTEGER(I4P)                          :: ERROR    !< Error status.
   TYPE(STRING)                          :: ACTION_  !< Action string.
   TYPE(STRING)                          :: NAME_    !< Block name, local variable

   ACTION_ = TRIM(ADJUSTL(ACTION)) ; ACTION_ = ACTION_%UPPER()
   SELECT CASE(ACTION_%CHARS())
   CASE('OPEN')
     OPEN(NEWUNIT=SELF%SCRATCH_UNIT(SCRATCH), &
          FORM='FORMATTED',                   &
          ACTION='READWRITE',                 &
          STATUS='SCRATCH',                   &
          IOSTAT=ERROR)
      NAME_ = '' ; IF (PRESENT(NAME)) NAME_ = TRIM(ADJUSTL(NAME))
      WRITE(SELF%SCRATCH_UNIT(SCRATCH), IOSTAT=ERROR, FMT='(A)') NAME_%CHARS()
   CASE('WRITE')
      IF (PRESENT(FILENAME)) WRITE(SELF%SCRATCH_UNIT(SCRATCH), IOSTAT=ERROR, FMT='(A)') TRIM(FILENAME)
   ENDSELECT
   ENDFUNCTION WRITE_BLOCK_SCRATCH
ENDMODULE ModLib_VTMFile

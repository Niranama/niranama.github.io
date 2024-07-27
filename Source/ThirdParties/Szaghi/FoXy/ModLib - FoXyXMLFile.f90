!< FoXy XML file class.
MODULE ModLib_FoXyXMLFile
!< FoXy XML file class.
USE ModLib_FoXyXMLTag, ONLY : XML_TAG
USE ModLib_Penf

IMPLICIT NONE
PRIVATE

TYPE, PUBLIC:: XML_FILE
  !< XML file class.
  !<
  !< @todo The "delete" facility is incomplete: nested tags are not taken into account. Better support will wiTH THE
  !< "dom" facility.
  PRIVATE
  INTEGER(I4P)               :: NT = 0 !< Number of XML tags.
  TYPE(XML_TAG), ALLOCATABLE :: TAG(:) !< XML tags array.
  CONTAINS
    ! public methods
    PROCEDURE :: FREE       !< Free dynamic memory.
    PROCEDURE :: PARSE      !< Parse xml data from string or file.
    PROCEDURE :: CONTENT    !< Return tag content of tag named *name*.
    PROCEDURE :: STRINGIFY  !< Convert the whole file data into a string.
    PROCEDURE :: ADD_TAG    !< Add tag to XML file.
    PROCEDURE :: DELETE_TAG !< Add tag from XML file.
    FINAL     :: FINALIZE   !< Free dynamic memory when finalizing.
    ! private methods
    PROCEDURE, PRIVATE :: PARSE_FROM_STRING !< Parse xml data from string.
ENDTYPE XML_FILE
CONTAINS
  ! public methods
  ELEMENTAL SUBROUTINE FREE(SELF)
  !< Free dynamic memory.
  CLASS(XML_FILE), INTENT(INOUT) :: SELF !< XML file.

  IF (ALLOCATED(SELF%TAG)) THEN
    CALL SELF%TAG%FREE
    DEALLOCATE(SELF%TAG)
    SELF%NT = 0
  ENDIF
  ENDSUBROUTINE FREE

  SUBROUTINE FINALIZE(FILE)
  !< Free dynamic memory when finalizing.
  TYPE(XML_FILE), INTENT(INOUT) :: FILE !< XML file.

  CALL FILE%FREE
  ENDSUBROUTINE FINALIZE

  SUBROUTINE PARSE(SELF, STRING, FILENAME)
  !< Parse xml data from string or file.
  !<
  !< @note Self data are free before trying to parse new xml data: all previously parsed data are lost.
  CLASS(XML_FILE),        INTENT(INOUT) :: SELF     !< XML file.
  CHARACTER(*), OPTIONAL, INTENT(IN)    :: STRING   !< String containing xml data.
  CHARACTER(*), OPTIONAL, INTENT(IN)    :: FILENAME !< File name containing xml data.
  CHARACTER(LEN=:), ALLOCATABLE         :: SOURCE   !< String containing xml data.

  CALL SELF%FREE
  IF (PRESENT(STRING)) THEN
    CALL SELF%PARSE_FROM_STRING(SOURCE_STRING=STRING)
  ELSEIF (PRESENT(FILENAME)) THEN
    SOURCE = LOAD_FILE_AS_STREAM(FILENAME=FILENAME, FAST_READ=.TRUE.)
    CALL SELF%PARSE_FROM_STRING(SOURCE_STRING=SOURCE)
  ENDIF
  ENDSUBROUTINE PARSE

  PURE FUNCTION CONTENT(SELF, NAME)
  !< Return tag content of tag named *name*.
  !<
  !< @note If there is no value, the *tag_content* string is returned empty, but allocated.
  CLASS(XML_FILE), INTENT(IN)   :: SELF    !< XML file.
  CHARACTER(*),    INTENT(IN)   :: NAME    !< Tag name.
  CHARACTER(LEN=:), ALLOCATABLE :: CONTENT !< Tag content.
  INTEGER(I4P)                  :: T       !< Counter.

  IF (ALLOCATED(CONTENT)) DEALLOCATE(CONTENT)
  IF (SELF%NT>0) THEN
    DO T=1, SELF%NT
      CALL SELF%TAG(T)%GET_CONTENT(NAME=NAME, CONTENT=CONTENT)
      IF (ALLOCATED(CONTENT)) EXIT
    ENDDO
  ENDIF
  IF (.NOT.ALLOCATED(CONTENT)) CONTENT = ''
  ENDFUNCTION CONTENT

  PURE FUNCTION STRINGIFY(SELF) RESULT(STRING)
  !< Convert the whole file data into a string.
  CLASS(XML_FILE), INTENT(IN)   :: SELF       !< XML file.
  CHARACTER(LEN=:), ALLOCATABLE :: STRING     !< Output string containing the whole xml file.
  CHARACTER(LEN=:), ALLOCATABLE :: TAG_STRING !< Output string containing the current tag.
  INTEGER(I4P)                  :: T          !< Counter.

  STRING = ''
  IF (SELF%NT>0) THEN
    DO T=1, SELF%NT - 1
      TAG_STRING = SELF%TAG(T)%STRINGIFY()
      STRING = STRING//TAG_STRING//NEW_LINE('a')
    ENDDO
    TAG_STRING = SELF%TAG(SELF%NT)%STRINGIFY()
    STRING = STRING//TAG_STRING
  ENDIF
  ENDFUNCTION STRINGIFY

  ELEMENTAL SUBROUTINE ADD_TAG(SELF, TAG)
  !< Add tag to XML file.
  CLASS(XML_FILE), INTENT(INOUT) :: SELF       !< XML file.
  TYPE(XML_TAG),   INTENT(IN)    :: TAG        !< XML tag.
  TYPE(XML_TAG), ALLOCATABLE     :: TAG_NEW(:) !< New (extended) tags array.

  IF (SELF%NT>0_I4P) THEN
    ALLOCATE(TAG_NEW(1:SELF%NT + 1))
    TAG_NEW(1:SELF%NT) = SELF%TAG(1:SELF%NT)
    TAG_NEW(SELF%NT + 1) = TAG
  ELSE
    ALLOCATE(TAG_NEW(1:1))
    TAG_NEW(1) = TAG
  ENDIF
  CALL MOVE_ALLOC(FROM=TAG_NEW, TO=SELF%TAG)
  SELF%NT = SELF%NT + 1
  ENDSUBROUTINE ADD_TAG

  ELEMENTAL SUBROUTINE DELETE_TAG(SELF, NAME)
  !< Delete tag from XML file.
  CLASS(XML_FILE), INTENT(INOUT) :: SELF       !< XML file.
  CHARACTER(*),    INTENT(IN)    :: NAME       !< XML tag name.
  TYPE(XML_TAG), ALLOCATABLE     :: TAG_NEW(:) !< New (extended) tags array.
  INTEGER(I4P)                   :: T          !< Counter.

  IF (SELF%NT>0_I4P) THEN
    DO T=1, SELF%NT
      IF (NAME==SELF%TAG(T)%NAME()) THEN
        ALLOCATE(TAG_NEW(1:SELF%NT - 1))
        IF (T==1) THEN
          TAG_NEW(T:) = SELF%TAG(T+1:)
        ELSEIF (T==SELF%NT) THEN
          TAG_NEW(:T-1) = SELF%TAG(:T-1)
        ELSE
          TAG_NEW(:T-1) = SELF%TAG(:T-1)
          TAG_NEW(T:) = SELF%TAG(T+1:)
        ENDIF
        CALL MOVE_ALLOC(FROM=TAG_NEW, TO=SELF%TAG)
        SELF%NT = SELF%NT - 1
        EXIT
      ENDIF
    ENDDO
  ENDIF
  ENDSUBROUTINE DELETE_TAG

  ! private methods
  SUBROUTINE PARSE_FROM_STRING(SELF, SOURCE_STRING)
  !< Parse xml data from string.
  CLASS(XML_FILE), INTENT(INOUT) :: SELF          !< XML file.
  CHARACTER(*),    INTENT(IN)    :: SOURCE_STRING !< String containing xml data.
  TYPE(XML_TAG)                  :: TAG           !< Dummy xml tag.
  INTEGER(I4P)                   :: TSTART        !< Counter for tracking string parsing.
  INTEGER(I4P)                   :: TEND          !< Counter for tracking string parsing.

  TSTART = 1
  TEND = 0
  DO WHILE(TSTART<LEN(SOURCE_STRING))
    CALL TAG%FREE
    CALL TAG%PARSE(SOURCE=SOURCE_STRING(TSTART:), TEND=TEND)
    IF (TEND==0) EXIT
    IF (TAG%IS_PARSED()) CALL SELF%ADD_TAG(TAG)
    TSTART = TSTART + TEND
  ENDDO
  ENDSUBROUTINE PARSE_FROM_STRING

  ! non TBP
  FUNCTION LOAD_FILE_AS_STREAM(FILENAME, DELIMITER_START, DELIMITER_END, FAST_READ, IOSTAT, IOMSG) RESULT(STREAM)
  !< Load file contents and store as single characters stream.
  CHARACTER(*),           INTENT(IN)  :: FILENAME        !< File name.
  CHARACTER(*), OPTIONAL, INTENT(IN)  :: DELIMITER_START !< Delimiter from which start the stream.
  CHARACTER(*), OPTIONAL, INTENT(IN)  :: DELIMITER_END   !< Delimiter to which end the stream.
  LOGICAL,      OPTIONAL, INTENT(IN)  :: FAST_READ       !< Flag for activating efficient reading with one sinGLE READ.
  INTEGER(I4P), OPTIONAL, INTENT(OUT) :: IOSTAT          !< IO error.
  CHARACTER(*), OPTIONAL, INTENT(OUT) :: IOMSG           !< IO error message.
  CHARACTER(LEN=:), ALLOCATABLE       :: STREAM          !< Output string containing the file data as a single STREAM.
  LOGICAL                             :: IS_FILE         !< Flag for inquiring the presence of the file.
  INTEGER(I4P)                        :: UNIT            !< Unit file.
  INTEGER(I4P)                        :: IOSTATD         !< IO error.
  CHARACTER(500)                      :: IOMSGD          !< IO error message.
  CHARACTER(1)                        :: C1              !< Single character.
  CHARACTER(LEN=:), ALLOCATABLE       :: STRING          !< Dummy string.
  LOGICAL                             :: CSTART          !< Flag for stream capturing trigging.
  LOGICAL                             :: CEND            !< Flag for stream capturing trigging.
  LOGICAL                             :: FAST            !< Flag for activating efficient reading with one sinGLE READ.
  INTEGER(I4P)                        :: FILESIZE        !< Size of the file for fast reading.

  FAST = .FALSE. ; IF (PRESENT(FAST_READ)) FAST = FAST_READ
  ! inquire file existance
  INQUIRE(FILE=ADJUSTL(TRIM(FILENAME)), EXIST=IS_FILE, IOSTAT=IOSTATD, IOMSG=IOMSGD)
  IF (.NOT.IS_FILE) THEN
    IF (PRESENT(IOSTAT)) IOSTAT = IOSTATD
    IF (PRESENT(IOMSG )) IOMSG  = IOMSGD
    RETURN
  ENDIF
  ! open file
  OPEN(NEWUNIT=UNIT, FILE=ADJUSTL(TRIM(FILENAME)), ACCESS='STREAM', FORM='UNFORMATTED', IOSTAT=IOSTATD, IOMSG=IOMSGD)
  IF (IOSTATD/=0) THEN
    IF (PRESENT(IOSTAT)) IOSTAT = IOSTATD
    IF (PRESENT(IOMSG )) IOMSG  = IOMSGD
    RETURN
  ENDIF
  ! loadg data
  STREAM = ''
  IF (PRESENT(DELIMITER_START).AND.PRESENT(DELIMITER_END)) THEN
    ! load only data inside delimiter_start and delimiter_end
    STRING = ''
    MAIN_READ_LOOP: DO
      READ(UNIT=UNIT, IOSTAT=IOSTATD, IOMSG=IOMSGD, END=10)C1
      IF (C1==DELIMITER_START(1:1)) THEN
        CSTART = .TRUE.
        STRING = C1
        START_READ_LOOP: DO WHILE(LEN(STRING)<LEN(DELIMITER_START))
          READ(UNIT=UNIT, IOSTAT=IOSTATD, IOMSG=IOMSGD, END=10)C1
          STRING = STRING//C1
          IF (.NOT.(INDEX(STRING=DELIMITER_START, SUBSTRING=STRING)>0)) THEN
            CSTART = .FALSE.
            EXIT START_READ_LOOP
          ENDIF
        ENDDO START_READ_LOOP
        IF (CSTART) THEN
          CEND = .FALSE.
          STREAM = STRING
          DO WHILE(.NOT.CEND)
            READ(UNIT=UNIT, IOSTAT=IOSTATD, IOMSG=IOMSGD, END=10)C1
            IF (C1==DELIMITER_END(1:1)) THEN ! maybe the end
              STRING = C1
              END_READ_LOOP: DO WHILE(LEN(STRING)<LEN(DELIMITER_END))
                READ(UNIT=UNIT, IOSTAT=IOSTATD, IOMSG=IOMSGD, END=10)C1
                STRING = STRING//C1
                IF (.NOT.(INDEX(STRING=DELIMITER_END, SUBSTRING=STRING)>0)) THEN
                  STREAM = STREAM//STRING
                  EXIT END_READ_LOOP
                ELSEIF (LEN(STRING)==LEN(DELIMITER_END)) THEN
                  CEND = .TRUE.
                  STREAM = STREAM//STRING
                  EXIT MAIN_READ_LOOP
                ENDIF
              ENDDO END_READ_LOOP
            ELSE
              STREAM = STREAM//C1
            ENDIF
          ENDDO
        ENDIF
      ENDIF
    ENDDO MAIN_READ_LOOP
  ELSE
    ! load all data
    IF (FAST) THEN
      ! load fast
      INQUIRE(FILE=ADJUSTL(TRIM(FILENAME)), SIZE=FILESIZE, IOSTAT=IOSTATD, IOMSG=IOMSGD)
      IF (IOSTATD==0) THEN
        IF (ALLOCATED(STREAM)) DEALLOCATE(STREAM)
        ALLOCATE(CHARACTER(LEN=FILESIZE):: STREAM)
        READ(UNIT=UNIT, IOSTAT=IOSTATD, IOMSG=IOMSGD, END=10)STREAM
      ENDIF
    ELSE
      ! load slow, one character loop
      READ_LOOP: DO
        READ(UNIT=UNIT,IOSTAT=IOSTATD,IOMSG=IOMSGD,END=10)C1
        STREAM = STREAM//C1
      ENDDO READ_LOOP
    ENDIF
  ENDIF
  10 CLOSE(UNIT)
  IF (PRESENT(IOSTAT)) IOSTAT = IOSTATD
  IF (PRESENT(IOMSG))  IOMSG  = IOMSGD
  ENDFUNCTION LOAD_FILE_AS_STREAM
ENDMODULE ModLib_FoXyXMLFile

!< FoXy XML tag class.
MODULE ModLib_FoXyXMLTag
!< FoXy XML tag class.
USE ModLib_Penf
USE ModLib_Stringifor

IMPLICIT NONE
PRIVATE
PUBLIC :: XML_TAG

TYPE :: XML_TAG
  !< XML tag class.
  !<
  !< A valid XML tag must have the following syntax for a tag without a content (with only attributes):
  !<```xml
  !<   <Tag_Name att#1_Name="att#1_val" att#2_Name="att#2_val"... att#Nt_Name="att#Nt_val"/>
  !<```
  !< while a tag with a content must have the following syntax:
  !<```xml
  !<   <Tag_Name att#1_Name="att#1_val" att#2_Name="att#2_val"... att#Nt_Name="att#Nt_val">Tag_Content</Tag_NaME>
  !<```
  !<
  !< It is worth noting that the syntax is case sensitive and that the attributes are optional. Each attribute NAME MUST BE FOLLOWED
  !< by '="' without any additional white spaces and its value must be termined by '"'. Each attribute is sepaRATED BY ONE OR MORE
  !< white spaces.
  PRIVATE
  TYPE(STRING)              :: TAG_NAME                !< Tag name.
  TYPE(STRING)              :: TAG_CONTENT             !< Tag content.
  TYPE(STRING), ALLOCATABLE :: ATTRIBUTE(:,:)          !< Attributes names/values pairs, [1:2, 1:].
  INTEGER(I4P)              :: ATTRIBUTES_NUMBER=0     !< Number of defined attributes.
  INTEGER(I4P)              :: INDENT=0                !< Number of indent-white-spaces.
  LOGICAL                   :: IS_SELF_CLOSING=.FALSE. !< Self closing tag flag.
  CONTAINS
    ! public methods
    GENERIC               :: ADD_ATTRIBUTES =>        &
                             ADD_SINGLE_ATTRIBUTE,    &
                             ADD_MULTIPLE_ATTRIBUTES, &
                             ADD_STREAM_ATTRIBUTES       !< Add attributes name/value pairs.
    PROCEDURE, PASS(SELF) :: ATTRIBUTES                  !< Return attributes name/value pairs as string.
    PROCEDURE, PASS(SELF) :: GET_CONTENT                 !< Return tag content.
    GENERIC               :: DELETE_ATTRIBUTES =>     &
                             DELETE_SINGLE_ATTRIBUTE, &
                             DELETE_MULTIPLE_ATTRIBUTES  !< Delete attributes name/value pairs.
    PROCEDURE, PASS(SELF) :: DELETE_CONTENT              !< Delete tag conent.
    PROCEDURE, PASS(SELF) :: END_TAG                     !< Return `</tag_name>` end tag.
    PROCEDURE, PASS(SELF) :: FREE                        !< Free dynamic memory.
    PROCEDURE, PASS(SELF) :: IS_ATTRIBUTE_PRESENT        !< Return .true. it the queried attribute name is defINED.
    PROCEDURE, PASS(SELF) :: IS_PARSED                   !< Check is tag is correctly parsed, i.e. its *tag_naME* IS ALLOCATED.
    PROCEDURE, PASS(SELF) :: NAME                        !< Return tag name.
    PROCEDURE, PASS(SELF) :: PARSE                       !< Parse the tag contained into a source string.
    PROCEDURE, PASS(SELF) :: SELF_CLOSING_TAG            !< Return `<tag_name.../>` self closing tag.
    PROCEDURE, PASS(SELF) :: SET                         !< Set tag data.
    PROCEDURE, PASS(SELF) :: START_TAG                   !< Return `<tag_name...>` start tag.
    PROCEDURE, PASS(SELF) :: STRINGIFY                   !< Convert the whole tag into a string.
    PROCEDURE, PASS(SELF) :: WRITE => WRITE_TAG          !< Write tag to unit file.
    GENERIC               :: ASSIGNMENT(=) => ASSIGN_TAG !< Assignment operator overloading.
    ! private methods
    PROCEDURE, PASS(SELF), PRIVATE :: ADD_SINGLE_ATTRIBUTE       !< Add one attribute name/value pair.
    PROCEDURE, PASS(SELF), PRIVATE :: ADD_MULTIPLE_ATTRIBUTES    !< Add list of attributes name/value pairs.
    PROCEDURE, PASS(SELF), PRIVATE :: ADD_STREAM_ATTRIBUTES      !< Add list of attributes name/value pairs paSSED AS STREAM.
    PROCEDURE, PASS(SELF), PRIVATE :: ALLOC_ATTRIBUTES           !< Allocate (prepare for filling) dynamic memORY OF ATTRIBUTES.
    PROCEDURE, PASS(SELF), PRIVATE :: DELETE_SINGLE_ATTRIBUTE    !< Delete one attribute name/value pair.
    PROCEDURE, PASS(SELF), PRIVATE :: DELETE_MULTIPLE_ATTRIBUTES !< Delete list of attributes name/value pairs.
    PROCEDURE, PASS(SELF), PRIVATE :: GET                        !< Get the tag value and attributes from sourCE.
    PROCEDURE, PASS(SELF), PRIVATE :: GET_VALUE                  !< Get the tag value from source after tag_naME HAS BEEN SET.
    PROCEDURE, PASS(SELF), PRIVATE :: GET_ATTRIBUTES             !< Get the attributes values from source.
    PROCEDURE, PASS(SELF), PRIVATE :: PARSE_TAG_NAME             !< Parse the tag name contained into a string.
    PROCEDURE, PASS(SELF), PRIVATE :: PARSE_ATTRIBUTES_NAMES     !< Parse the tag attributes names contained iNTO A STRING.
    PROCEDURE, PASS(SELF), PRIVATE :: SEARCH                     !< Search tag named *tag_name* into a string.
    ! operators
    PROCEDURE, PASS(LHS), PRIVATE :: ASSIGN_TAG !< Assignment between two tags.
#if (__GNUC__ < 9)
    FINAL                         :: FINALIZE   !< Free dynamic memory when finalizing.
#endif
ENDTYPE XML_TAG

INTERFACE XML_TAG
  !< Overload *xml_tag* with creator procedures.
  MODULE PROCEDURE CREATE_TAG_FLAT, CREATE_TAG_NESTED
ENDINTERFACE
CONTAINS
  ! creator procedures overloading *xml_tag* name
  PURE FUNCTION CREATE_TAG_FLAT(NAME, ATTRIBUTE, ATTRIBUTES, ATTRIBUTES_STREAM, SANITIZE_ATTRIBUTES_VALUE, CONTENT, INDENT, &
                                IS_CONTENT_INDENTED, IS_SELF_CLOSING) RESULT(TAG)
  !< Return an instance of xml tag.
  !<
  !< Attributes are passed by array.
  CHARACTER(*), INTENT(IN)           :: NAME                      !< Tag name.
  CHARACTER(*), INTENT(IN), OPTIONAL :: ATTRIBUTE(1:)             !< Attribute name/value pair [1:2].
  CHARACTER(*), INTENT(IN), OPTIONAL :: ATTRIBUTES(1:,1:)         !< Attributes list of name/value pairs [1:2,1:].
  CHARACTER(*), INTENT(IN), OPTIONAL :: ATTRIBUTES_STREAM         !< Attributes list of name/value pairs as siNGLE STREAM.
  LOGICAL,      INTENT(IN), OPTIONAL :: SANITIZE_ATTRIBUTES_VALUE !< Sanitize attributes value.
  CHARACTER(*), INTENT(IN), OPTIONAL :: CONTENT                   !< Tag value.
  INTEGER(I4P), INTENT(IN), OPTIONAL :: INDENT                    !< Number of indent-white-spaces.
  LOGICAL,      INTENT(IN), OPTIONAL :: IS_CONTENT_INDENTED       !< Activate content indentation.
  LOGICAL,      INTENT(IN), OPTIONAL :: IS_SELF_CLOSING           !< The tag is self closing.
  TYPE(XML_TAG)                      :: TAG                       !< XML tag.

  CALL TAG%SET(NAME=NAME, ATTRIBUTE=ATTRIBUTE, ATTRIBUTES=ATTRIBUTES, ATTRIBUTES_STREAM=ATTRIBUTES_STREAM, &
               SANITIZE_ATTRIBUTES_VALUE=SANITIZE_ATTRIBUTES_VALUE, CONTENT=CONTENT,                       &
               INDENT=INDENT, IS_CONTENT_INDENTED=IS_CONTENT_INDENTED, IS_SELF_CLOSING=IS_SELF_CLOSING)
  ENDFUNCTION CREATE_TAG_FLAT

  PURE FUNCTION CREATE_TAG_NESTED(NAME, CONTENT, ATTRIBUTE, ATTRIBUTES, ATTRIBUTES_STREAM, SANITIZE_ATTRIBUTES_VALUE, INDENT, &
                                  IS_CONTENT_INDENTED) RESULT(TAG)
  !< Return an instance of xml tag with value being a nested tag.
  !<
  !< Attributes are passed by array.
  CHARACTER(*),  INTENT(IN)           :: NAME                      !< Tag name.
  TYPE(XML_TAG), INTENT(IN)           :: CONTENT                   !< Tag value as nested tag..
  CHARACTER(*),  INTENT(IN), OPTIONAL :: ATTRIBUTE(1:)             !< Attribute name/value pair [1:2].
  CHARACTER(*),  INTENT(IN), OPTIONAL :: ATTRIBUTES(1:,1:)         !< Attributes list of name/value pairs [1:2,1:].
  CHARACTER(*),  INTENT(IN), OPTIONAL :: ATTRIBUTES_STREAM         !< Attributes list of name/value pairs as sINGLE STREAM.
  LOGICAL,       INTENT(IN), OPTIONAL :: SANITIZE_ATTRIBUTES_VALUE !< Sanitize attributes value.
  INTEGER(I4P),  INTENT(IN), OPTIONAL :: INDENT                    !< Number of indent-white-spaces.
  LOGICAL,       INTENT(IN), OPTIONAL :: IS_CONTENT_INDENTED       !< Activate value indentation.
  TYPE(XML_TAG)                       :: TAG                       !< XML tag.

  CALL TAG%SET(NAME=NAME, ATTRIBUTE=ATTRIBUTE, ATTRIBUTES=ATTRIBUTES, CONTENT=CONTENT%STRINGIFY(),                      &
               SANITIZE_ATTRIBUTES_VALUE=SANITIZE_ATTRIBUTES_VALUE, ATTRIBUTES_STREAM=ATTRIBUTES_STREAM, INDENT=INDENT, &
               IS_CONTENT_INDENTED=IS_CONTENT_INDENTED)
  ENDFUNCTION CREATE_TAG_NESTED

  ! public methods
  PURE FUNCTION ATTRIBUTES(SELF) RESULT(ATT_)
  !< Return attributes name/value pairs as string.
  CLASS(XML_TAG), INTENT(IN)    :: SELF !< XML tag.
  CHARACTER(LEN=:), ALLOCATABLE :: ATT_ !< The attributes string.
  INTEGER(I4P)                  :: A    !< Counter.

  IF (SELF%ATTRIBUTES_NUMBER>0) THEN
    ATT_ = ''
    DO A=1, SELF%ATTRIBUTES_NUMBER
      ATT_ = ATT_//' '//SELF%ATTRIBUTE(1, A)//'="'//SELF%ATTRIBUTE(2, A)//'"'
    ENDDO
    ATT_ = TRIM(ADJUSTL(ATT_))
  ENDIF
  ENDFUNCTION ATTRIBUTES

  PURE SUBROUTINE GET_CONTENT(SELF, NAME, CONTENT)
  !< Return tag content of self (or its nested tags) if named *name*.
  !<
  !< @note If there is no value, the *content* string is returned deallocated.
  CLASS(XML_TAG),                INTENT(IN)  :: SELF    !< XML tag.
  CHARACTER(*),                  INTENT(IN)  :: NAME    !< Searched tag name.
  CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: CONTENT !< Tag content.
  TYPE(XML_TAG)                              :: TAG     !< Dummy XML tag.

  IF (ALLOCATED(CONTENT)) DEALLOCATE(CONTENT)
  IF (SELF%TAG_NAME%IS_ALLOCATED()) THEN
    IF (SELF%TAG_NAME==NAME) THEN
      IF (SELF%TAG_CONTENT%IS_ALLOCATED()) CONTENT = SELF%TAG_CONTENT%CHARS()
    ELSE
      IF (SELF%TAG_CONTENT%IS_ALLOCATED()) THEN
        CALL TAG%SEARCH(TAG_NAME=NAME, SOURCE=SELF%TAG_CONTENT%CHARS())
        IF (TAG%TAG_CONTENT%IS_ALLOCATED()) CONTENT = TAG%TAG_CONTENT%CHARS()
      ENDIF
    ENDIF
  ENDIF
  ENDSUBROUTINE GET_CONTENT

  PURE FUNCTION END_TAG(SELF, IS_INDENTED) RESULT(TAG_)
  !< Return `</tag_name>` end tag.
  CLASS(XML_TAG), INTENT(IN)           :: SELF        !< XML tag.
  LOGICAL,        INTENT(IN), OPTIONAL :: IS_INDENTED !< Activate content indentation.
  CHARACTER(LEN=:), ALLOCATABLE        :: TAG_        !< The end tag string.

  TAG_ = '</'//SELF%TAG_NAME//'>'
  IF (PRESENT(IS_INDENTED)) THEN
    IF (IS_INDENTED) TAG_ = REPEAT(' ', SELF%INDENT)//TAG_
  ENDIF
  ENDFUNCTION END_TAG

  ELEMENTAL SUBROUTINE FREE(SELF)
  !< Free dynamic memory.
  CLASS(XML_TAG), INTENT(INOUT) :: SELF !< XML tag.

  CALL SELF%TAG_NAME%FREE
  CALL SELF%TAG_CONTENT%FREE
  IF (ALLOCATED(SELF%ATTRIBUTE)) THEN
    CALL SELF%ATTRIBUTE%FREE
    DEALLOCATE(SELF%ATTRIBUTE)
  ENDIF
  SELF%ATTRIBUTES_NUMBER = 0
  SELF%INDENT = 0
  SELF%IS_SELF_CLOSING = .FALSE.
  ENDSUBROUTINE FREE

  ELEMENTAL FUNCTION IS_PARSED(SELF)
  !< Check is tag is correctly parsed, i.e. its *tag_name* is allocated.
  CLASS(XML_TAG), INTENT(IN) :: SELF      !< XML tag.
  LOGICAL                    :: IS_PARSED !< Result of check.

  IS_PARSED = SELF%TAG_NAME%IS_ALLOCATED()
  ENDFUNCTION IS_PARSED

  PURE FUNCTION NAME(SELF)
  !< Return tag name.
  CLASS(XML_TAG), INTENT(IN)    :: SELF !< XML tag.
  CHARACTER(LEN=:), ALLOCATABLE :: NAME !< XML tag name.

  NAME = SELF%TAG_NAME%CHARS()
  ENDFUNCTION NAME

  ELEMENTAL SUBROUTINE PARSE(SELF, SOURCE, TSTART, TEND)
  !< Parse the tag contained into a source string.
  !<
  !< It is assumed that the first tag contained into the source string is parsed, the others eventually presenT ARE OMITTED.
  !< Valid syntax are:
  !< + `<tag_name att1="att1 val" att2="att2 val"...>...</tag_name>`
  !< + `<tag_name att1="att1 val" att2="att2 val".../>`
  !< @note Inside the attributes value the symbols `<` and `>` are not allowed.
  CLASS(XML_TAG),         INTENT(INOUT) :: SELF      !< XML tag.
  CHARACTER(*),           INTENT(IN)    :: SOURCE    !< String containing the input.
  INTEGER(I4P), OPTIONAL, INTENT(OUT)   :: TSTART    !< Starting index of tag inside the string.
  INTEGER(I4P), OPTIONAL, INTENT(OUT)   :: TEND      !< Ending index of tag inside the string.
  INTEGER(I4P)                          :: TSTARTD   !< Starting index of tag inside the string.
  INTEGER(I4P)                          :: TENDD     !< Ending index of tag inside the string.

  TSTARTD = 0
  TENDD   = 0
  CALL SELF%PARSE_TAG_NAME(SOURCE=SOURCE, TSTART=TSTARTD, TEND=TENDD)
  IF (SELF%TAG_NAME%IS_ALLOCATED()) THEN
    IF (INDEX(STRING=SOURCE(TSTARTD:TENDD), SUBSTRING='=')>0) CALL SELF%PARSE_ATTRIBUTES_NAMES(SOURCE=SOURCE(TSTARTD:TENDD))
    IF (INDEX(STRING=SOURCE, SUBSTRING='</'//SELF%TAG_NAME//'>')>0) &
      TENDD = INDEX(STRING=SOURCE, SUBSTRING='</'//SELF%TAG_NAME//'>') + LEN('</'//SELF%TAG_NAME//'>') - 1
    CALL SELF%GET(SOURCE=SOURCE(TSTARTD:TENDD))
  ENDIF
  IF (PRESENT(TSTART)) TSTART = TSTARTD
  IF (PRESENT(TEND  )) TEND   = TENDD
  ENDSUBROUTINE PARSE

  PURE SUBROUTINE SET(SELF, NAME, ATTRIBUTE, ATTRIBUTES, ATTRIBUTES_STREAM, SANITIZE_ATTRIBUTES_VALUE, CONTENT, &
                      INDENT, IS_CONTENT_INDENTED, IS_SELF_CLOSING)
  !< Set tag data.
  CLASS(XML_TAG), INTENT(INOUT)        :: SELF                      !< XML tag.
  CHARACTER(*),   INTENT(IN), OPTIONAL :: NAME                      !< Tag name.
  CHARACTER(*),   INTENT(IN), OPTIONAL :: ATTRIBUTE(1:)             !< Attribute name/value pair [1:2].
  CHARACTER(*),   INTENT(IN), OPTIONAL :: ATTRIBUTES(1:,1:)         !< Attributes list of name/value pairs [1:2,1:].
  CHARACTER(*),   INTENT(IN), OPTIONAL :: ATTRIBUTES_STREAM         !< Attributes list of name/value pairs as SINGLE STREAM.
  LOGICAL,        INTENT(IN), OPTIONAL :: SANITIZE_ATTRIBUTES_VALUE !< Sanitize attributes value.
  CHARACTER(*),   INTENT(IN), OPTIONAL :: CONTENT                   !< Tag value.
  INTEGER(I4P),   INTENT(IN), OPTIONAL :: INDENT                    !< Number of indent-white-spaces.
  LOGICAL,        INTENT(IN), OPTIONAL :: IS_CONTENT_INDENTED       !< Activate value indentation.
  LOGICAL,        INTENT(IN), OPTIONAL :: IS_SELF_CLOSING           !< The tag is self closing.
  LOGICAL                              :: IS_CONTENT_INDENTED_      !< Activate value indentation.

  IF (PRESENT(NAME)) SELF%TAG_NAME = NAME
  IF (PRESENT(ATTRIBUTE)) CALL SELF%ADD_SINGLE_ATTRIBUTE(ATTRIBUTE=ATTRIBUTE, SANITIZE_VALUE=SANITIZE_ATTRIBUTES_VALUE)
  IF (PRESENT(ATTRIBUTES)) CALL SELF%ADD_MULTIPLE_ATTRIBUTES(ATTRIBUTES=ATTRIBUTES, SANITIZE_VALUES=SANITIZE_ATTRIBUTES_VALUE)
  IF (PRESENT(ATTRIBUTES_STREAM)) CALL SELF%ADD_STREAM_ATTRIBUTES(ATTRIBUTES_STREAM=ATTRIBUTES_STREAM, &
                                                                  SANITIZE_VALUES=SANITIZE_ATTRIBUTES_VALUE)
  IF (PRESENT(INDENT)) SELF%INDENT = INDENT
  IF (PRESENT(CONTENT)) THEN
    IS_CONTENT_INDENTED_ = .FALSE. ; IF (PRESENT(IS_CONTENT_INDENTED)) IS_CONTENT_INDENTED_ = IS_CONTENT_INDENTED
    IF (IS_CONTENT_INDENTED_) THEN
      SELF%TAG_CONTENT = NEW_LINE('a')//REPEAT(' ', SELF%INDENT+2)//CONTENT//NEW_LINE('a')
    ELSE
      SELF%TAG_CONTENT = CONTENT
    ENDIF
  ENDIF
  IF (PRESENT(IS_SELF_CLOSING)) SELF%IS_SELF_CLOSING = IS_SELF_CLOSING
  ENDSUBROUTINE SET

  PURE FUNCTION SELF_CLOSING_TAG(SELF, IS_INDENTED) RESULT(TAG_)
  !< Return `<tag_name.../>` self closing tag.
  CLASS(XML_TAG), INTENT(IN)           :: SELF        !< XML tag.
  LOGICAL,        INTENT(IN), OPTIONAL :: IS_INDENTED !< Flag to check if tag is indented.
  CHARACTER(LEN=:), ALLOCATABLE        :: TAG_        !< The self closing tag string.

  TAG_ = '<'//SELF%TAG_NAME
  IF (SELF%ATTRIBUTES_NUMBER>0) TAG_ = TAG_//' '//SELF%ATTRIBUTES()
  TAG_ = TAG_//'/>'
  IF (PRESENT(IS_INDENTED)) THEN
    IF (IS_INDENTED) TAG_ = REPEAT(' ', SELF%INDENT)//TAG_
  ENDIF
  ENDFUNCTION SELF_CLOSING_TAG

  PURE FUNCTION START_TAG(SELF, IS_INDENTED) RESULT(TAG_)
  !< Return `<tag_name...>` start tag.
  CLASS(XML_TAG), INTENT(IN)           :: SELF        !< XML tag.
  LOGICAL,        INTENT(IN), OPTIONAL :: IS_INDENTED !< Flag to check if tag is indented.
  CHARACTER(LEN=:), ALLOCATABLE        :: TAG_        !< The start tag string.

  TAG_ = '<'//SELF%TAG_NAME
  IF (SELF%ATTRIBUTES_NUMBER>0) TAG_ = TAG_//' '//SELF%ATTRIBUTES()
  TAG_ = TAG_//'>'
  IF (PRESENT(IS_INDENTED)) THEN
    IF (IS_INDENTED) TAG_ = REPEAT(' ', SELF%INDENT)//TAG_
  ENDIF
  ENDFUNCTION START_TAG

  PURE FUNCTION STRINGIFY(SELF, IS_INDENTED, IS_CONTENT_INDENTED, ONLY_START, ONLY_CONTENT, ONLY_END) RESULT(STRINGED)
  !< Convert the whole tag into a string.
  CLASS(XML_TAG), INTENT(IN)           :: SELF                 !< XML tag.
  LOGICAL,        INTENT(IN), OPTIONAL :: IS_INDENTED          !< Activate content indentation.
  LOGICAL,        INTENT(IN), OPTIONAL :: IS_CONTENT_INDENTED  !< Activate content indentation.
  LOGICAL,        INTENT(IN), OPTIONAL :: ONLY_START           !< Write only start tag.
  LOGICAL,        INTENT(IN), OPTIONAL :: ONLY_CONTENT         !< Write only content.
  LOGICAL,        INTENT(IN), OPTIONAL :: ONLY_END             !< Write only end tag.
  CHARACTER(LEN=:), ALLOCATABLE        :: STRINGED             !< Output string containing the whole tag.
  LOGICAL                              :: IS_CONTENT_INDENTED_ !< Activate content indentation.
  LOGICAL                              :: ONLY_START_          !< Write only start tag.
  LOGICAL                              :: ONLY_CONTENT_        !< Write only content.
  LOGICAL                              :: ONLY_END_            !< Write only end tag.

  IS_CONTENT_INDENTED_ = .FALSE. ; IF (PRESENT(IS_CONTENT_INDENTED)) IS_CONTENT_INDENTED_ = IS_CONTENT_INDENTED
  ONLY_START_ = .FALSE. ; IF (PRESENT(ONLY_START)) ONLY_START_ = ONLY_START
  ONLY_CONTENT_ = .FALSE. ; IF (PRESENT(ONLY_CONTENT)) ONLY_CONTENT_ = ONLY_CONTENT
  ONLY_END_ = .FALSE. ; IF (PRESENT(ONLY_END)) ONLY_END_ = ONLY_END
  IF (ONLY_START_) THEN
    STRINGED = SELF%START_TAG(IS_INDENTED=IS_INDENTED)
  ELSEIF (ONLY_CONTENT_) THEN
    IF (SELF%TAG_CONTENT%IS_ALLOCATED()) THEN
      IF (IS_CONTENT_INDENTED_) THEN
        STRINGED = REPEAT(' ', SELF%INDENT+2)//SELF%TAG_CONTENT
      ELSE
        STRINGED = SELF%TAG_CONTENT%CHARS()
      ENDIF
    ENDIF
  ELSEIF (ONLY_END_) THEN
    STRINGED = SELF%END_TAG(IS_INDENTED=IS_INDENTED)
  ELSE
    STRINGED = ''
    IF (SELF%TAG_NAME%IS_ALLOCATED()) THEN
      IF (SELF%IS_SELF_CLOSING) THEN
        STRINGED = SELF%SELF_CLOSING_TAG(IS_INDENTED=IS_INDENTED)
      ELSE
        STRINGED = SELF%START_TAG(IS_INDENTED=IS_INDENTED)
        IF (SELF%TAG_CONTENT%IS_ALLOCATED()) THEN
          IF (IS_CONTENT_INDENTED_) THEN
            STRINGED = STRINGED//NEW_LINE('a')//REPEAT(' ', SELF%INDENT+2)//&
                       SELF%TAG_CONTENT//NEW_LINE('a')//REPEAT(' ', SELF%INDENT)
          ELSE
            STRINGED = STRINGED//SELF%TAG_CONTENT
          ENDIF
        ENDIF
        STRINGED = STRINGED//SELF%END_TAG()
      ENDIF
    ENDIF
  ENDIF
  ENDFUNCTION STRINGIFY

  SUBROUTINE WRITE_TAG(SELF, UNIT, IS_INDENTED, IS_CONTENT_INDENTED, FORM, END_RECORD, ONLY_START, ONLY_CONTENT, ONLY_END, &
                       IOSTAT, IOMSG)
  !< Write tag to unit file.
  CLASS(XML_TAG), INTENT(IN)            :: SELF                !< XML tag.
  INTEGER(I4P),   INTENT(IN)            :: UNIT                !< File unit.
  LOGICAL,        INTENT(IN),  OPTIONAL :: IS_INDENTED         !< Activate content indentation.
  LOGICAL,        INTENT(IN),  OPTIONAL :: IS_CONTENT_INDENTED !< Activate content indentation.
  CHARACTER(*),   INTENT(IN),  OPTIONAL :: FORM                !< Format.
  CHARACTER(*),   INTENT(IN),  OPTIONAL :: END_RECORD          !< Ending record.
  LOGICAL,        INTENT(IN),  OPTIONAL :: ONLY_START          !< Write only start tag.
  LOGICAL,        INTENT(IN),  OPTIONAL :: ONLY_CONTENT        !< Write only content.
  LOGICAL,        INTENT(IN),  OPTIONAL :: ONLY_END            !< Write only end tag.
  INTEGER(I4P),   INTENT(OUT), OPTIONAL :: IOSTAT              !< IO status.
  CHARACTER(*),   INTENT(OUT), OPTIONAL :: IOMSG               !< IO message.
  TYPE(STRING)                          :: FORM_               !< Format.
  TYPE(STRING)                          :: END_RECORD_         !< Ending record.
  INTEGER(I4P)                          :: IOSTAT_             !< IO status.
  CHARACTER(500)                        :: IOMSG_              !< IO message.

  FORM_ = 'UNFORMATTED'
  IF (PRESENT(FORM)) THEN
    FORM_ = FORM
    FORM_ = FORM_%UPPER()
  ENDIF
  END_RECORD_ = '' ; IF (PRESENT(END_RECORD)) END_RECORD_ = END_RECORD
  SELECT CASE(FORM_%CHARS())
  CASE('UNFORMATTED')
    WRITE(UNIT=UNIT, IOSTAT=IOSTAT_, IOMSG=IOMSG_)SELF%STRINGIFY(IS_INDENTED=IS_INDENTED,                 &
                                                                 IS_CONTENT_INDENTED=IS_CONTENT_INDENTED, &
                                                                 ONLY_START=ONLY_START,                   &
                                                                 ONLY_CONTENT=ONLY_CONTENT,               &
                                                                 ONLY_END=ONLY_END)//END_RECORD_
  CASE('FORMATTED')
    WRITE(UNIT=UNIT, FMT='(A)', IOSTAT=IOSTAT_, IOMSG=IOMSG_)SELF%STRINGIFY(IS_INDENTED=IS_INDENTED,                 &
                                                                            IS_CONTENT_INDENTED=IS_CONTENT_INDENTED, &
                                                                            ONLY_START=ONLY_START,                   &
                                                                            ONLY_CONTENT=ONLY_CONTENT,               &
                                                                            ONLY_END=ONLY_END)//END_RECORD_
  ENDSELECT
  IF (PRESENT(IOSTAT)) IOSTAT = IOSTAT_
  IF (PRESENT(IOMSG)) IOMSG = IOMSG_
  ENDSUBROUTINE WRITE_TAG

  ! private methods
  PURE FUNCTION IS_ATTRIBUTE_PRESENT(SELF, NAME) RESULT(IS_PRESENT)
  !< Return .true. it the queried attribute name is defined, .false. otherwise.
  CLASS(XML_TAG), INTENT(IN) :: SELF       !< XML tag.
  CHARACTER(*),   INTENT(IN) :: NAME       !< Attribute name.
  LOGICAL                    :: IS_PRESENT !< Inquire result.
  INTEGER(I4P)               :: A          !< Counter.

  IS_PRESENT = .FALSE.
  IF (SELF%ATTRIBUTES_NUMBER>0) THEN
    DO A=1, SELF%ATTRIBUTES_NUMBER
      IF (SELF%ATTRIBUTE(1, A)==NAME) THEN
        IS_PRESENT = .TRUE.
        EXIT
      ENDIF
    ENDDO
  ENDIF
  ENDFUNCTION IS_ATTRIBUTE_PRESENT

  PURE SUBROUTINE ADD_SINGLE_ATTRIBUTE(SELF, ATTRIBUTE, SANITIZE_VALUE)
  !< Add one attribute name/value pair.
  !<
  !< @note Leading and trailing white spaces are trimmed out by attribute's name.
  CLASS(XML_TAG), INTENT(INOUT)        :: SELF               !< XML tag.
  CHARACTER(*),   INTENT(IN)           :: ATTRIBUTE(1:)      !< Attribute name/value pair [1:2].
  LOGICAL,        INTENT(IN), OPTIONAL :: SANITIZE_VALUE     !< Sanitize attribute value.
  TYPE(STRING), ALLOCATABLE            :: NEW_ATTRIBUTE(:,:) !< Temporary storage for attributes.
  LOGICAL                              :: SANITIZE_VALUE_    !< Sanitize attribute value.
  LOGICAL                              :: IS_UPDATED         !< Flag to check if the attribute has been updeteD.
  INTEGER(I4P)                         :: A                  !< Counter.

  SANITIZE_VALUE_ = .FALSE. ; IF (PRESENT(SANITIZE_VALUE)) SANITIZE_VALUE_ = SANITIZE_VALUE
  IF (SELF%ATTRIBUTES_NUMBER>0) THEN
    IS_UPDATED = .FALSE.
    UPDATE_IF_ALREADY_PRESENT: DO A=1, SELF%ATTRIBUTES_NUMBER
      IF (SELF%ATTRIBUTE(1, A)==ATTRIBUTE(1)) THEN
        IF (SANITIZE_VALUE_) THEN
          SELF%ATTRIBUTE(2, A) = TRIM(ADJUSTL(ATTRIBUTE(2)))
        ELSE
          SELF%ATTRIBUTE(2, A) = ATTRIBUTE(2)
        ENDIF
        IS_UPDATED = .TRUE.
        EXIT UPDATE_IF_ALREADY_PRESENT
      ENDIF
    ENDDO UPDATE_IF_ALREADY_PRESENT
    IF (.NOT.IS_UPDATED) THEN
      ALLOCATE(NEW_ATTRIBUTE(1:2, 1:SELF%ATTRIBUTES_NUMBER+1))
      NEW_ATTRIBUTE(1:2, 1:SELF%ATTRIBUTES_NUMBER) = SELF%ATTRIBUTE
      NEW_ATTRIBUTE(1, SELF%ATTRIBUTES_NUMBER+1) = TRIM(ADJUSTL(ATTRIBUTE(1)))
      IF (SANITIZE_VALUE_) THEN
        NEW_ATTRIBUTE(2, SELF%ATTRIBUTES_NUMBER+1) = TRIM(ADJUSTL(ATTRIBUTE(2)))
      ELSE
        NEW_ATTRIBUTE(2, SELF%ATTRIBUTES_NUMBER+1) = ATTRIBUTE(2)
      ENDIF
      CALL MOVE_ALLOC(FROM=NEW_ATTRIBUTE, TO=SELF%ATTRIBUTE)
      SELF%ATTRIBUTES_NUMBER = SELF%ATTRIBUTES_NUMBER + 1
    ENDIF
  ELSE
    CALL SELF%ALLOC_ATTRIBUTES(NA=1)
    SELF%ATTRIBUTE(1, 1) = TRIM(ADJUSTL(ATTRIBUTE(1)))
    IF (SANITIZE_VALUE_) THEN
      SELF%ATTRIBUTE(2, 1) = TRIM(ADJUSTL(ATTRIBUTE(2)))
    ELSE
      SELF%ATTRIBUTE(2, 1) = ATTRIBUTE(2)
    ENDIF
  ENDIF
  ENDSUBROUTINE ADD_SINGLE_ATTRIBUTE

  PURE SUBROUTINE ADD_MULTIPLE_ATTRIBUTES(SELF, ATTRIBUTES, SANITIZE_VALUES)
  !< Add list of attributes name/value pairs.
  CLASS(XML_TAG), INTENT(INOUT)        :: SELF              !< XML tag.
  CHARACTER(*),   INTENT(IN)           :: ATTRIBUTES(1:,1:) !< Attribute name/value pair list [1:2,1:].
  LOGICAL,        INTENT(IN), OPTIONAL :: SANITIZE_VALUES   !< Sanitize attribute values.
  INTEGER(I4P)                         :: A                 !< Counter.

  DO A=1, SIZE(ATTRIBUTES, DIM=2)
    ! not efficient: many reallocation, but safe
    CALL SELF%ADD_SINGLE_ATTRIBUTE(ATTRIBUTE=ATTRIBUTES(1:,A), SANITIZE_VALUE=SANITIZE_VALUES)
  ENDDO
  ENDSUBROUTINE ADD_MULTIPLE_ATTRIBUTES

  PURE SUBROUTINE ADD_STREAM_ATTRIBUTES(SELF, ATTRIBUTES_STREAM, SANITIZE_VALUES)
  !< Add list of attributes name/value pairs passed as stream.
  !<
  !< @note The character `=` cannot compare into the attributes names of values.
  CLASS(XML_TAG), INTENT(INOUT)        :: SELF              !< XML tag.
  CHARACTER(*),   INTENT(IN)           :: ATTRIBUTES_STREAM !< Attribute name/value pair list passed as stream.
  LOGICAL,        INTENT(IN), OPTIONAL :: SANITIZE_VALUES   !< Sanitize attribute values.
  TYPE(STRING)                         :: ATTRIBUTES_STRING !< Attribute name/value pair list as string.
  TYPE(STRING)                         :: TOKENS(1:3)       !< Attributes tokenized by `=`.
  TYPE(STRING)                         :: ATTRIBUTE(1:2)    !< Attribute name/value pair.
  LOGICAL                              :: CONTINUE_TO_PARSE !< Sentinel to stop attributes stream parsing.
  INTEGER(I4P)                         :: MAX_CHARS         !< Counter.

  ATTRIBUTES_STRING = ATTRIBUTES_STREAM
  CONTINUE_TO_PARSE = .TRUE.
  DO WHILE(CONTINUE_TO_PARSE)
    TOKENS = ATTRIBUTES_STRING%PARTITION(SEP='=')
    ATTRIBUTE(1) = TRIM(ADJUSTL(TOKENS(1)))
    IF (ATTRIBUTE(1)/='') THEN
      TOKENS(3) = TOKENS(3)%SLICE(ISTART=TOKENS(3)%INDEX('"')+1, IEND=TOKENS(3)%LEN())
      ATTRIBUTE(2) = TOKENS(3)%SLICE(ISTART=1, IEND=TOKENS(3)%INDEX('"')-1)
      TOKENS(3) = TOKENS(3)%SLICE(ISTART=TOKENS(3)%INDEX('"')+1, IEND=TOKENS(3)%LEN())
      MAX_CHARS = MAX(ATTRIBUTE(1)%LEN(), ATTRIBUTE(2)%LEN())
      ATTRIBUTE(1) = ATTRIBUTE(1)%FILL(WIDTH=MAX_CHARS, RIGHT=.TRUE., FILLING_CHAR=' ')
      ATTRIBUTE(2) = ATTRIBUTE(2)%FILL(WIDTH=MAX_CHARS, RIGHT=.TRUE., FILLING_CHAR=' ')
      CALL SELF%ADD_SINGLE_ATTRIBUTE(ATTRIBUTE=[ATTRIBUTE(1)//'', ATTRIBUTE(2)//''], SANITIZE_VALUE=SANITIZE_VALUES)
      IF (TOKENS(3)%INDEX('=')>0) THEN
        ATTRIBUTES_STRING = TOKENS(3)
      ELSE
        CONTINUE_TO_PARSE = .FALSE.
      ENDIF
    ELSE
      CONTINUE_TO_PARSE = .FALSE.
    ENDIF
  ENDDO
  ENDSUBROUTINE ADD_STREAM_ATTRIBUTES

  ELEMENTAL SUBROUTINE ALLOC_ATTRIBUTES(SELF, NA)
  !< Allocate (prepare for filling) dynamic memory of attributes.
  CLASS(XML_TAG),    INTENT(INOUT) :: SELF     !< XML tag.
  INTEGER(I4P),      INTENT(IN)    :: NA       !< Number of attributes.

  IF (ALLOCATED(SELF%ATTRIBUTE)) THEN
    CALL SELF%ATTRIBUTE%FREE
    DEALLOCATE(SELF%ATTRIBUTE)
  ENDIF
  ALLOCATE(SELF%ATTRIBUTE(1:2, 1:NA))
  SELF%ATTRIBUTES_NUMBER = NA
  ENDSUBROUTINE ALLOC_ATTRIBUTES

  PURE SUBROUTINE DELETE_CONTENT(SELF)
  !< Delete tag content.
  CLASS(XML_TAG), INTENT(INOUT) :: SELF !< XML tag.

  CALL SELF%TAG_CONTENT%FREE
  ENDSUBROUTINE DELETE_CONTENT

  PURE SUBROUTINE DELETE_SINGLE_ATTRIBUTE(SELF, NAME)
  !< Delete one attribute name/value pair.
  CLASS(XML_TAG), INTENT(INOUT) :: SELF               !< XML tag.
  CHARACTER(*),   INTENT(IN)    :: NAME               !< Attribute name.
  TYPE(STRING), ALLOCATABLE     :: NEW_ATTRIBUTE(:,:) !< Temporary storage for attributes.
  INTEGER(I4P)                  :: A                  !< Counter.

  IF (SELF%ATTRIBUTES_NUMBER>0) THEN
    SEARCH_TAG: DO A=1, SELF%ATTRIBUTES_NUMBER
      IF (SELF%ATTRIBUTE(1, A)==NAME) THEN
        IF (SELF%ATTRIBUTES_NUMBER>1) THEN
          ALLOCATE(NEW_ATTRIBUTE(1:2, 1:SELF%ATTRIBUTES_NUMBER-1))
          IF (A==1) THEN
            NEW_ATTRIBUTE(:, A:) = SELF%ATTRIBUTE(:, A+1:)
          ELSEIF (A==SELF%ATTRIBUTES_NUMBER) THEN
            NEW_ATTRIBUTE(:, :A-1) = SELF%ATTRIBUTE(:, :A-1)
          ELSE
            NEW_ATTRIBUTE(:, :A-1) = SELF%ATTRIBUTE(:, :A-1)
            NEW_ATTRIBUTE(:, A:) = SELF%ATTRIBUTE(:, A+1:)
          ENDIF
          CALL MOVE_ALLOC(FROM=NEW_ATTRIBUTE, TO=SELF%ATTRIBUTE)
        ELSE
          CALL SELF%ATTRIBUTE%FREE
          DEALLOCATE(SELF%ATTRIBUTE)
        ENDIF
        SELF%ATTRIBUTES_NUMBER = SELF%ATTRIBUTES_NUMBER - 1
        EXIT SEARCH_TAG
      ENDIF
    ENDDO SEARCH_TAG
  ENDIF
  ENDSUBROUTINE DELETE_SINGLE_ATTRIBUTE

  PURE SUBROUTINE DELETE_MULTIPLE_ATTRIBUTES(SELF, NAME)
  !< Delete list of attributes name/value pairs.
  CLASS(XML_TAG), INTENT(INOUT) :: SELF     !< XML tag.
  CHARACTER(*),   INTENT(IN)    :: NAME(1:) !< Attributes names.
  INTEGER(I4P)                  :: A        !< Counter.

  DO A=1, SIZE(NAME, DIM=1)
    CALL SELF%DELETE_SINGLE_ATTRIBUTE(NAME=NAME(A))
  ENDDO
  ENDSUBROUTINE DELETE_MULTIPLE_ATTRIBUTES

  ELEMENTAL SUBROUTINE GET(SELF, SOURCE)
  !< Get the tag content and attributes from source after tag_name and attributes names have been set.
  CLASS(XML_TAG), INTENT(INOUT) :: SELF   !< XML tag.
  CHARACTER(*),   INTENT(IN)    :: SOURCE !< String containing data.

  CALL SELF%GET_VALUE(SOURCE=SOURCE)
  CALL SELF%GET_ATTRIBUTES(SOURCE=SOURCE)
  ! call self%get_nested()
  ENDSUBROUTINE GET

  ELEMENTAL SUBROUTINE GET_ATTRIBUTES(SELF, SOURCE)
  !< Get the attributes values from source after tag_name and attributes names have been set.
  CLASS(XML_TAG), INTENT(INOUT) :: SELF   !< XML tag.
  CHARACTER(*),   INTENT(IN)    :: SOURCE !< String containing data.
  INTEGER                       :: A      !< Counter.
  INTEGER                       :: C1     !< Counter.
  INTEGER                       :: C2     !< Counter.

  IF (INDEX(STRING=SOURCE, SUBSTRING='<'//SELF%TAG_NAME)>0) THEN
    IF (SELF%ATTRIBUTES_NUMBER>0) THEN ! parsing attributes
      DO A=1, SELF%ATTRIBUTES_NUMBER
        C1 = INDEX(STRING=SOURCE, SUBSTRING=SELF%ATTRIBUTE(1, A)//'="') + SELF%ATTRIBUTE(1, A)%LEN() + 2
        IF (C1>SELF%ATTRIBUTE(1, A)%LEN() + 2) THEN
          C2 = INDEX(STRING=SOURCE(C1:), SUBSTRING='"')
          IF (C2>0) THEN
            SELF%ATTRIBUTE(2, A) = SOURCE(C1:C1+C2-2)
          ELSE
            CALL SELF%ATTRIBUTE(2, A)%FREE
          ENDIF
        ELSE
          CALL SELF%ATTRIBUTE(2, A)%FREE
        ENDIF
      ENDDO
    ENDIF
  ENDIF
  ENDSUBROUTINE GET_ATTRIBUTES

  ELEMENTAL SUBROUTINE GET_VALUE(SELF, SOURCE)
  !< Get the tag value from source after tag_name has been set.
  CLASS(XML_TAG), INTENT(INOUT) :: SELF   !< XML tag.
  CHARACTER(*),   INTENT(IN)    :: SOURCE !< String containing data.
  INTEGER                       :: C1     !< Counter.
  INTEGER                       :: C2     !< Counter.

  CALL SELF%TAG_CONTENT%FREE
  SELF%IS_SELF_CLOSING = .FALSE.
  IF (INDEX(STRING=SOURCE, SUBSTRING='<'//SELF%TAG_NAME)>0) THEN
    C2 = INDEX(STRING=SOURCE, SUBSTRING='</'//SELF%TAG_NAME//'>')
    IF (C2>0) THEN ! parsing tag value
      C1 = INDEX(STRING=SOURCE, SUBSTRING='>')
      IF (C1+1<C2-1) SELF%TAG_CONTENT = SOURCE(C1+1:C2-1)
    ELSE
      SELF%IS_SELF_CLOSING = .TRUE.
    ENDIF
  ENDIF
  ENDSUBROUTINE GET_VALUE

  ELEMENTAL SUBROUTINE PARSE_ATTRIBUTES_NAMES(SELF, SOURCE)
  !< Parse the tag attributes names contained into a string.
  !<
  !< Valid syntax is:
  !< + `att1="att1 val" att2="att2 val"...`
  !< @note Inside the attributes value the symbols `<` and `>` are not allowed.
  CLASS(XML_TAG), INTENT(INOUT) :: SELF   !< XML tag.
  CHARACTER(*),   INTENT(IN)    :: SOURCE !< String containing the input.
  CHARACTER(LEN=:), ALLOCATABLE :: ATT    !< Dummy string for parsing file.
  INTEGER(I4P)                  :: C      !< Counter.
  INTEGER(I4P)                  :: S      !< Counter.
  INTEGER(I4P)                  :: A      !< Counter.
  INTEGER(I4P)                  :: NA     !< Counter.

  NA = 0
  C = 1
  ATT_COUNT: DO WHILE(C<=LEN(SOURCE))
    IF (SOURCE(C:C)=='=') NA = NA + 1
    C = C + 1
  ENDDO ATT_COUNT
  IF (NA>0) THEN
    CALL SELF%ALLOC_ATTRIBUTES(NA=NA)
    C = INDEX(STRING=SOURCE, SUBSTRING=' ')
    ATT = SOURCE(C:)
    C = 1
    A = 1
    ATT_SEARCH: DO WHILE(C<=LEN(ATT))
      IF (ATT(C:C)=='=') THEN
        S = MAX(0, INDEX(STRING=ATT, SUBSTRING=' '))
        SELF%ATTRIBUTE(1, A) = TRIM(ADJUSTL(ATT(S+1:C-1)))
        ATT = ATT(C+1:)
        C = 1
        A = A + 1
      ENDIF
      C = C + 1
    ENDDO ATT_SEARCH
  ENDIF
  ENDSUBROUTINE PARSE_ATTRIBUTES_NAMES

  ELEMENTAL SUBROUTINE PARSE_TAG_NAME(SELF, SOURCE, TSTART, TEND)
  !< Parse the tag name contained into a string.
  !<
  !< It is assumed that the first tag contained into the source is parsed, the others eventually present are oMITTED.
  !< Valid syntax are:
  !< + `<tag_name att1="att1 val" att2="att2 val"...>...</tag_name>`
  !< + `<tag_name att1="att1 val" att2="att2 val".../>`
  !< @note Inside the attributes value the symbols `<` and `>` are not allowed.
  CLASS(XML_TAG),         INTENT(INOUT) :: SELF    !< XML tag.
  CHARACTER(*),           INTENT(IN)    :: SOURCE  !< String containing the input.
  INTEGER(I4P), OPTIONAL, INTENT(OUT)   :: TSTART  !< Starting index of tag inside the source.
  INTEGER(I4P), OPTIONAL, INTENT(OUT)   :: TEND    !< Ending index of tag inside the source.
  INTEGER(I4P)                          :: TSTARTD !< Starting index of tag inside the source.
  INTEGER(I4P)                          :: TENDD   !< Ending index of tag inside the source.
  CHARACTER(LEN=1)                      :: C1      !< Dummy string for parsing file.
  CHARACTER(LEN=:), ALLOCATABLE         :: C2      !< Dummy string for parsing file.
  INTEGER(I4P)                          :: C       !< Counter.
  INTEGER(I4P)                          :: S       !< Counter.

  TSTARTD = 0
  TENDD   = 0
  C = 1
  TAG_SEARCH: DO WHILE(C<=LEN(SOURCE))
    C1 = SOURCE(C:C)
    IF (C1=='<') THEN
      TSTARTD = C
      C2 = C1
      TAG_NAME: DO WHILE(C<LEN(SOURCE))
        C = C + 1 ; C1 = SOURCE(C:C)
        C2 = C2//C1
        IF (C1=='>') THEN
          TENDD = C
          EXIT TAG_NAME
        ENDIF
      ENDDO TAG_NAME
      S = INDEX(STRING=C2, SUBSTRING=' ')
      IF (S>0) THEN ! there are attributes
        SELF%TAG_NAME = C2(2:S-1)
      ELSE
        IF (INDEX(STRING=C2, SUBSTRING='/>')>0) THEN ! self closing tag
          SELF%TAG_NAME = C2(2:LEN(C2)-2)
        ELSE
          SELF%TAG_NAME = C2(2:LEN(C2)-1)
        ENDIF
      ENDIF
      EXIT TAG_SEARCH
    ENDIF
    C = C + 1
  ENDDO TAG_SEARCH
  IF (PRESENT(TSTART)) TSTART = TSTARTD
  IF (PRESENT(TEND  )) TEND   = TENDD
  ENDSUBROUTINE PARSE_TAG_NAME

  ELEMENTAL SUBROUTINE SEARCH(SELF, TAG_NAME, SOURCE, TSTART, TEND)
  !< Search tag named *tag_name* into a string and, in case it is found, store into self.
  !<
  !< @note If *tag_name* is not found, self is returned empty.
  CLASS(XML_TAG),         INTENT(INOUT) :: SELF     !< XML tag.
  CHARACTER(*),           INTENT(IN)    :: TAG_NAME !< Searched tag name.
  CHARACTER(*),           INTENT(IN)    :: SOURCE   !< String containing the input.
  INTEGER(I4P), OPTIONAL, INTENT(OUT)   :: TSTART   !< Starting index of tag inside the source.
  INTEGER(I4P), OPTIONAL, INTENT(OUT)   :: TEND     !< Ending index of tag inside the source.
  TYPE(XML_TAG)                         :: TAG      !< Dummy XML tag.
  INTEGER(I4P)                          :: TSTART_  !< Starting index of tag inside the source, local variable.
  INTEGER(I4P)                          :: TEND_    !< Ending index of tag inside the source, local variable.
  LOGICAL                               :: FOUND    !< Flag for inquiring search result.
  INTEGER(I4P)                          :: TSTART_C !< Starting index of tag inside the current slice of sourcE.
  INTEGER(I4P)                          :: TEND_C   !< Starting index of tag inside the current slice of sourcE.
  INTEGER(I4P)                          :: I

  CALL SELF%FREE
  SELF%TAG_NAME = TAG_NAME
  TSTART_ = 1
  TEND_   = 0
  FOUND = .FALSE.
  TSTART_C = 0
  TEND_C = 0
  TAG_SEARCH: DO
    CALL TAG%PARSE(SOURCE=SOURCE(TEND_ + 1:), TSTART=TSTART_C, TEND=TEND_C)
    TSTART_ = TSTART_ + TEND_
    TEND_ = TEND_ + TEND_C
    IF (TSTART_C==0.AND.TEND_C==0) THEN
      EXIT TAG_SEARCH ! no tag found
    ELSE
      IF (TAG%TAG_NAME%IS_ALLOCATED()) THEN
        IF (TAG%TAG_NAME==SELF%TAG_NAME) THEN
          FOUND = .TRUE.
        ENDIF
      ENDIF
    ENDIF
    IF (FOUND) EXIT TAG_SEARCH
  ENDDO TAG_SEARCH
  IF (FOUND) THEN
    SELF = TAG
  ELSE
    CALL SELF%FREE
  ENDIF
  IF (PRESENT(TSTART)) TSTART = TSTART_
  IF (PRESENT(TEND  )) TEND   = TEND_
  ENDSUBROUTINE SEARCH

  ! assignment (=)
  ELEMENTAL SUBROUTINE ASSIGN_TAG(LHS, RHS)
  !< Assignment between two tags.
  CLASS(XML_TAG), INTENT(INOUT) :: LHS !< Left hand side.
  TYPE(XML_TAG),  INTENT(IN)    :: RHS !< Right hand side.
  INTEGER(I4P)                  :: A   !< Counter.

  CALL LHS%FREE
  IF (RHS%TAG_NAME%IS_ALLOCATED()) LHS%TAG_NAME = RHS%TAG_NAME
  IF (RHS%TAG_CONTENT%IS_ALLOCATED()) LHS%TAG_CONTENT = RHS%TAG_CONTENT
  IF (RHS%ATTRIBUTES_NUMBER>0) THEN
    ALLOCATE(LHS%ATTRIBUTE(1:2, 1:RHS%ATTRIBUTES_NUMBER))
    DO A=1, RHS%ATTRIBUTES_NUMBER
      LHS%ATTRIBUTE(1:2, A) = RHS%ATTRIBUTE(1:2, A)
    ENDDO
  ENDIF
  LHS%ATTRIBUTES_NUMBER = RHS%ATTRIBUTES_NUMBER
  LHS%INDENT = RHS%INDENT
  LHS%IS_SELF_CLOSING = RHS%IS_SELF_CLOSING
  ENDSUBROUTINE ASSIGN_TAG

  ! finalize
  ELEMENTAL SUBROUTINE FINALIZE(TAG)
  !< Free dynamic memory when finalizing.
  TYPE(XML_TAG), INTENT(INOUT) :: TAG !< XML tag.

  CALL TAG%FREE
  ENDSUBROUTINE FINALIZE
ENDMODULE ModLib_FoXyXMLTag

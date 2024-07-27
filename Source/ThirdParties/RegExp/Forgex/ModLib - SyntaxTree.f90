MODULE ModLib_SyntaxTree
   USE, INTRINSIC :: ISO_FORTRAN_ENV, STDERR=>ERROR_UNIT
   USE :: ModLib_Segment
   IMPLICIT NONE
   PRIVATE

   PUBLIC :: TREE_T
   PUBLIC :: BUILD_SYNTAX_TREE
   PUBLIC :: TAPE_T
   PUBLIC :: DEALLOCATE_TREE
   PUBLIC :: PRINT_TREE
   PUBLIC :: TK_CHAR, TK_UNION, TK_LPAR, TK_RPAR, TK_BACKSLASH, TK_QUESTION
   PUBLIC :: TK_STAR, TK_PLUS, TK_LSBRACKET, TK_RSBRACKET, TK_LCURLYBRACE
   PUBLIC :: TK_RCURLYBRACE, TK_DOT, TK_HYPHEN, TK_CARET, TK_DOLLAR, TK_END
   PUBLIC :: OP_CHAR, OP_CONCAT, OP_UNION, OP_CLOSURE, OP_EMPTY

   CHARACTER(UTF8_CHAR_SIZE), PARAMETER, PUBLIC :: EMPTY = CHAR(0)

     ! These enums will be rewritten in Fortran 2023's enumerator feature.
   ENUM, BIND(C)
      ENUMERATOR :: TK_CHAR = 0
      ENUMERATOR :: TK_UNION        ! 1
      ENUMERATOR :: TK_LPAR         ! 2
      ENUMERATOR :: TK_RPAR         ! 3
      ENUMERATOR :: TK_BACKSLASH    ! 4
      ENUMERATOR :: TK_QUESTION     ! 5
      ENUMERATOR :: TK_STAR         ! 6
      ENUMERATOR :: TK_PLUS         ! 7
      ENUMERATOR :: TK_LSBRACKET    ! 8  left square bracket
      ENUMERATOR :: TK_RSBRACKET    ! 9  right square bracket
      ENUMERATOR :: TK_LCURLYBRACE  ! 10 left curly brace
      ENUMERATOR :: TK_RCURLYBRACE  ! 11 right curly brace
      ENUMERATOR :: TK_DOT          ! 12
      ENUMERATOR :: TK_HYPHEN       ! 13
      ENUMERATOR :: TK_CARET        ! 14
      ENUMERATOR :: TK_DOLLAR       ! 15
      ENUMERATOR :: TK_END          ! 16
   END ENUM

   ENUM, BIND(C)
      ENUMERATOR :: OP_CHAR = 0
      ENUMERATOR :: OP_CONCAT
      ENUMERATOR :: OP_UNION
      ENUMERATOR :: OP_CLOSURE
      ENUMERATOR :: OP_EMPTY
   END ENUM
   
   INTEGER(INT32), PARAMETER :: TREE_MAX_SIZE = 1024

   CHARACTER(1), PARAMETER, PRIVATE :: ESCAPE_T = 't'
   CHARACTER(1), PARAMETER, PRIVATE :: ESCAPE_N = 'n'
   CHARACTER(1), PARAMETER, PRIVATE :: ESCAPE_R = 'r'
   CHARACTER(1), PARAMETER, PRIVATE :: ESCAPE_D = 'd'
   CHARACTER(1), PARAMETER, PRIVATE :: ESCAPE_W = 'w'
   CHARACTER(1), PARAMETER, PRIVATE :: ESCAPE_S = 's'
   CHARACTER(1), PARAMETER, PRIVATE :: ESCAPE_D_CAPITAL = 'D'
   CHARACTER(1), PARAMETER, PRIVATE :: ESCAPE_W_CAPITAL = 'W'
   CHARACTER(1), PARAMETER, PRIVATE :: ESCAPE_S_CAPITAL = 'S'
   CHARACTER(1), PARAMETER, PRIVATE :: HAT = '^'
   CHARACTER(1), PARAMETER, PRIVATE :: HYPHEN = '-'
   CHARACTER(1), PARAMETER, PRIVATE :: CARET = '^'
   CHARACTER(1), PARAMETER, PRIVATE :: DOLLAR = '$'

   TYPE :: ALLOCATED_LIST_T
      TYPE(TREE_T), POINTER :: NODE
   END TYPE

   TYPE :: TREE_T
      INTEGER(INT32) :: OP
      TYPE(SEGMENT_T), ALLOCATABLE :: C(:)
      TYPE(TREE_T), POINTER :: LEFT => NULL()
      TYPE(TREE_T), POINTER :: RIGHT => NULL()
   END TYPE

   TYPE :: TAPE_T
      CHARACTER(:), ALLOCATABLE :: STR
      INTEGER(INT32) :: CURRENT_TOKEN
      CHARACTER(UTF8_CHAR_SIZE) :: TOKEN_CHAR = EMPTY
      INTEGER(INT32) :: IDX = 1
   CONTAINS
      PROCEDURE :: GET_TOKEN
   END TYPE

   INTEGER :: TREE_NODE_COUNT = 0

   TYPE(ALLOCATED_LIST_T) :: ARRAY(TREE_MAX_SIZE)

CONTAINS

   FUNCTION BUILD_SYNTAX_TREE(TAPE, STR) RESULT(ROOT)
      IMPLICIT NONE
      CHARACTER(*), INTENT(IN) :: STR
      TYPE(TAPE_T), INTENT(INOUT) :: TAPE
      TYPE(TREE_T), POINTER :: ROOT

      ROOT => NULL()

      TAPE%IDX = 1

      CALL INITIALIZE_PARSER(TAPE, STR)

      ROOT => REGEX(TAPE)

      IF (TAPE%CURRENT_TOKEN /= TK_END) THEN
         WRITE(STDERR, *) "The pattern contains extra character at the end."
      END IF

   END FUNCTION BUILD_SYNTAX_TREE


   SUBROUTINE DEALLOCATE_TREE()
      IMPLICIT NONE
      INTEGER :: I, MAX

      MAX = TREE_NODE_COUNT

      DO I = 1, MAX
         IF (ASSOCIATED(ARRAY(I)%NODE)) THEN
            DEALLOCATE(ARRAY(I)%NODE)
            TREE_NODE_COUNT = TREE_NODE_COUNT - 1
         END IF
      END DO

   END SUBROUTINE DEALLOCATE_TREE



   SUBROUTINE PRINT_TREE(TREE)
      IMPLICIT NONE
      TYPE(TREE_T), INTENT(IN) :: TREE

      WRITE(STDERR, '(a)') "--- PRINT TREE ---"
      CALL PRINT_TREE_INTERNAL(TREE)
      WRITE(STDERR, '(a)') ''
   END SUBROUTINE PRINT_TREE


   SUBROUTINE INITIALIZE_PARSER(TAPE, STR)
      IMPLICIT NONE
      TYPE(TAPE_T), INTENT(INOUT) :: TAPE
      CHARACTER(*), INTENT(IN) :: STR

      TAPE%STR = STR

      CALL GET_TOKEN(TAPE)
   END SUBROUTINE INITIALIZE_PARSER


   SUBROUTINE GET_TOKEN(SELF, CLASS)
      USE :: ModLib_UTF8
      IMPLICIT NONE
      CLASS(TAPE_T) :: SELF
      LOGICAL, OPTIONAL, INTENT(IN) :: CLASS

      LOGICAL :: CLASS_FLAG

      INTEGER(INT32) :: I, NEXTI
      CHARACTER(UTF8_CHAR_SIZE) :: C

      CLASS_FLAG = .FALSE.
      IF (PRESENT(CLASS)) CLASS_FLAG = CLASS

      I = SELF%IDX

      IF (I > LEN(SELF%STR)) THEN
         SELF%CURRENT_TOKEN = TK_END
         SELF%TOKEN_CHAR = ''
      ELSE
         NEXTI = IDXUTF8(SELF%STR, I) + 1

         C = SELF%STR(I:NEXTI-1)

         IF (CLASS_FLAG) THEN

            SELECT CASE (TRIM(C))
            CASE (']')
               SELF%CURRENT_TOKEN = TK_RSBRACKET
            CASE ('-')
               SELF%CURRENT_TOKEN = TK_HYPHEN
               SELF%TOKEN_CHAR = C
            CASE DEFAULT
               SELF%CURRENT_TOKEN = TK_CHAR
               SELF%TOKEN_CHAR = C
            END SELECT

         ELSE

            SELECT CASE (TRIM(C))
            CASE ('|')
               SELF%CURRENT_TOKEN = TK_UNION
            CASE ('(')
               SELF%CURRENT_TOKEN = TK_LPAR
            CASE (')')
               SELF%CURRENT_TOKEN = TK_RPAR
            CASE ('*')
               SELF%CURRENT_TOKEN = TK_STAR
            CASE ('+')
               SELF%CURRENT_TOKEN = TK_PLUS
            CASE ('?')
               SELF%CURRENT_TOKEN = TK_QUESTION
            CASE ('\')
               SELF%CURRENT_TOKEN = TK_BACKSLASH

               I = NEXTI
               NEXTI = IDXUTF8(SELF%STR, I) + 1

               C = SELF%STR(I:NEXTI-1)
               SELF%TOKEN_CHAR = C
            CASE  ('[')
               SELF%CURRENT_TOKEN = TK_LSBRACKET
            CASE (']')
               SELF%CURRENT_TOKEN = TK_RSBRACKET
            CASE ('{')
               SELF%CURRENT_TOKEN = TK_LCURLYBRACE
            CASE ('}')
               SELF%CURRENT_TOKEN = TK_RCURLYBRACE
            CASE ('.')
               SELF%CURRENT_TOKEN = TK_DOT
            CASE ('^')
               SELF%CURRENT_TOKEN = TK_CARET
            CASE ('$')
               SELF%CURRENT_TOKEN = TK_DOLLAR
            CASE DEFAULT
               SELF%CURRENT_TOKEN = TK_CHAR
               SELF%TOKEN_CHAR = C
            END SELECT
         END IF

         SELF%IDX = NEXTI
      END IF

   END SUBROUTINE GET_TOKEN

!=====================================================================!

   FUNCTION MAKE_TREE_NODE(OP, LEFT, RIGHT) RESULT(NODE)
      IMPLICIT NONE
      INTEGER(INT32), INTENT(IN) :: OP
      TYPE(TREE_T), POINTER, INTENT(IN) :: LEFT, RIGHT
      TYPE(TREE_T), POINTER :: NODE

      NODE => NULL()

      ALLOCATE(NODE)

      NODE%OP = OP
      NODE%LEFT => LEFT
      NODE%RIGHT => RIGHT

      TREE_NODE_COUNT = TREE_NODE_COUNT + 1

      ARRAY(TREE_NODE_COUNT)%NODE => NODE
   END FUNCTION


   FUNCTION MAKE_ATOM (SEGMENT) RESULT(NODE)
      IMPLICIT NONE
      TYPE(SEGMENT_T), INTENT(IN) :: SEGMENT
      TYPE(TREE_T), POINTER :: NODE

      NODE => NULL()
      ALLOCATE(NODE)
      ALLOCATE(NODE%C(1))

      NODE%OP = OP_CHAR
      NODE%C = SEGMENT

      TREE_NODE_COUNT = TREE_NODE_COUNT + 1
      ARRAY(TREE_NODE_COUNT)%NODE => NODE
   END FUNCTION MAKE_ATOM

!=====================================================================!

   FUNCTION REGEX(TAPE) RESULT(TREE)
      IMPLICIT NONE
      TYPE(TAPE_T), INTENT(INOUT) :: TAPE
      TYPE(TREE_T), POINTER :: TREE

      TREE => NULL()

      TREE => TERM(TAPE)
      DO WHILE (TAPE%CURRENT_TOKEN == TK_UNION)
         CALL TAPE%GET_TOKEN()
         TREE => MAKE_TREE_NODE(OP_UNION, TREE, TERM(TAPE))
      END DO

   END FUNCTION REGEX


   FUNCTION TERM(TAPE) RESULT(TREE)
      IMPLICIT NONE
      TYPE(TAPE_T), INTENT(INOUT) :: TAPE
      TYPE(TREE_T), POINTER :: TREE

      TREE => NULL()

      IF ( TAPE%CURRENT_TOKEN == TK_UNION &
           .OR. TAPE%CURRENT_TOKEN == TK_RPAR &
           .OR. TAPE%CURRENT_TOKEN == TK_END) THEN
         TREE => MAKE_TREE_NODE(OP_EMPTY, NULL(), NULL())
      ELSE
         TREE => POSTFIX_OP(TAPE)
         DO WHILE (TAPE%CURRENT_TOKEN /= TK_UNION &
                   .AND. TAPE%CURRENT_TOKEN /= TK_RPAR &
                   .AND. TAPE%CURRENT_TOKEN /= TK_END )
            TREE => MAKE_TREE_NODE(OP_CONCAT, TREE, POSTFIX_OP(TAPE))
         END DO
      END IF
   END FUNCTION TERM


   FUNCTION POSTFIX_OP(TAPE) RESULT(TREE)
      IMPLICIT NONE
      TYPE(TAPE_T), INTENT(INOUT) :: TAPE
      TYPE(TREE_T), POINTER :: TREE

      TREE => NULL()

      TREE => PRIMARY(TAPE)

      SELECT CASE (TAPE%CURRENT_TOKEN)
      CASE (TK_STAR)
         TREE => MAKE_TREE_NODE(OP_CLOSURE, TREE, NULL())
         CALL TAPE%GET_TOKEN()

      CASE (TK_PLUS)
         TREE => MAKE_TREE_NODE(OP_CONCAT, TREE, MAKE_TREE_NODE(OP_CLOSURE, TREE, NULL()))
         CALL TAPE%GET_TOKEN()

      CASE (TK_QUESTION)
         TREE => MAKE_TREE_NODE(OP_UNION, TREE, MAKE_TREE_NODE(OP_EMPTY, TREE, NULL()))
         CALL TAPE%GET_TOKEN()

      CASE (TK_LCURLYBRACE)
         TREE => RANGE_MIN_MAX(TAPE, TREE)
         CALL TAPE%GET_TOKEN()
      END SELECT

   END FUNCTION POSTFIX_OP


   FUNCTION PRIMARY (TAPE) RESULT(TREE)
      IMPLICIT NONE
      TYPE(TAPE_T), INTENT(INOUT) :: TAPE
      TYPE(TREE_T), POINTER :: TREE

      TYPE(SEGMENT_T) :: SEG

      TREE => NULL()

      SELECT CASE (TAPE%CURRENT_TOKEN)
      CASE (TK_CHAR)
         SEG = SEGMENT_T(ICHAR_UTF8(TAPE%TOKEN_CHAR), ICHAR_UTF8(TAPE%TOKEN_CHAR))
         TREE => MAKE_ATOM(SEG)
         CALL TAPE%GET_TOKEN()

      CASE (TK_LPAR)
         CALL TAPE%GET_TOKEN()
         TREE => REGEX(TAPE)
         IF (TAPE%CURRENT_TOKEN /= TK_RPAR) THEN
            WRITE(STDERR, *) "Close parenthesis is expected."
         END IF
         CALL TAPE%GET_TOKEN()

      CASE (TK_LSBRACKET)
         CALL TAPE%GET_TOKEN(CLASS=.TRUE.)
         TREE => CHAR_CLASS(TAPE)
         IF (TAPE%CURRENT_TOKEN /= TK_RSBRACKET) THEN
            WRITE(STDERR, *) "Close square bracket is expected."
         END IF
         CALL TAPE%GET_TOKEN()

      CASE (TK_DOT)
         TREE => MAKE_ATOM(SEG_ANY)
         CALL TAPE%GET_TOKEN()

      CASE (TK_BACKSLASH)
         TREE => SHORTHAND(TAPE)
         CALL TAPE%GET_TOKEN()

      CASE (TK_CARET)
         TREE => MAKE_TREE_CRLF()
         CALL TAPE%GET_TOKEN()
      CASE (TK_DOLLAR)
         TREE => MAKE_TREE_CRLF()
         CALL TAPE%GET_TOKEN()

      CASE DEFAULT
         WRITE(STDERR, *) "Pattern includes some syntax error."
      END SELECT

   END FUNCTION PRIMARY


   FUNCTION RANGE_MIN_MAX(TAPE, PTR) RESULT(TREE)
      IMPLICIT NONE
      TYPE(TAPE_T), INTENT(INOUT) :: TAPE
      TYPE(TREE_T), POINTER, INTENT(IN) :: PTR
      TYPE(TREE_T), POINTER :: TREE

      CHARACTER(:), ALLOCATABLE :: BUF
      INTEGER(INT32) :: ARG(2), IOS, MIN, MAX, COUNT

      BUF = ''
      ARG(:) = 0
      TREE => NULL()
      MAX = 0
      MIN = 0

      CALL TAPE%GET_TOKEN()

      DO WHILE (TAPE%CURRENT_TOKEN /= TK_RCURLYBRACE)
         BUF = BUF//TRIM(TAPE%TOKEN_CHAR)
         CALL TAPE%GET_TOKEN()

         IF (TAPE%CURRENT_TOKEN == TK_END) THEN
            WRITE(STDERR, *) "range_min_max: Close curly brace is expected."
            EXIT
         END IF
      END DO

      READ(BUF, *, IOSTAT=IOS) ARG(:)

      BUF = ADJUSTL(BUF)


      IF (ARG(1) == 0) THEN   ! {,max}, {0,max}
         MIN = 0
         MAX = ARG(2)
      ELSE IF (ARG(2) == 0) THEN ! {min,}, {num}
         IF (BUF(LEN_TRIM(BUF):LEN_TRIM(BUF)) == ',') THEN
            MIN = ARG(1)
            MAX = 0
         ELSE
            MIN = ARG(1)
            MAX = ARG(1)
         END IF

      ELSE
         MIN = ARG(1)
         MAX = ARG(2)
      END IF

      IF (MAX == 0) THEN

         IF (MIN == 0) THEN
            TREE => MAKE_TREE_NODE(OP_CLOSURE, PTR, NULL())
            RETURN
         END IF

         IF (MIN >= 1) THEN
            TREE => MAKE_TREE_NODE(OP_UNION, PTR, MAKE_TREE_NODE(OP_EMPTY, TREE, NULL()))
            TREE => MAKE_TREE_NODE(OP_CONCAT, PTR, TREE)
         END IF

         IF (MIN > 1) THEN
            COUNT = 1
            DO WHILE (COUNT < MIN)
               TREE => MAKE_TREE_NODE(OP_CONCAT, PTR, TREE)
               COUNT = COUNT + 1
            END DO
         END IF

         RETURN


      ELSE IF (MAX == 1) THEN

         IF (MIN == 0) THEN
            TREE => MAKE_TREE_NODE(OP_UNION, PTR, MAKE_TREE_NODE(OP_EMPTY, PTR, NULL()))
            RETURN
         END IF

         IF (MIN >= 1) THEN
            TREE => PTR
            RETURN
         END IF


      ELSE ! (max > 1)

         IF (MIN == 0) THEN
            COUNT = 1
            TREE => PTR
            DO WHILE (COUNT < MAX)
               TREE => MAKE_TREE_NODE(OP_UNION, TREE, MAKE_TREE_NODE(OP_EMPTY, TREE, NULL()))
               TREE => MAKE_TREE_NODE(OP_CONCAT, PTR, TREE)
               COUNT = COUNT + 1
            END DO

            TREE => MAKE_TREE_NODE(OP_UNION, TREE, MAKE_TREE_NODE(OP_EMPTY, TREE, NULL()))

            RETURN
         END IF

         IF (MIN == 1) THEN
            COUNT = 1
            TREE => PTR

            DO WHILE (COUNT < MAX-1)
               TREE => MAKE_TREE_NODE(OP_UNION, TREE, MAKE_TREE_NODE(OP_EMPTY, TREE, NULL()))
               TREE => MAKE_TREE_NODE(OP_CONCAT, PTR, TREE)
               COUNT = COUNT + 1
            END DO

            TREE => MAKE_TREE_NODE(OP_UNION, TREE, MAKE_TREE_NODE(OP_EMPTY, TREE, NULL()))
            TREE => MAKE_TREE_NODE(OP_CONCAT, PTR, TREE)
            RETURN

         END IF

         IF (MIN > 1) THEN
            COUNT = MIN + 1

            TREE => PTR

            DO WHILE (COUNT < MAX+1)
               TREE => MAKE_TREE_NODE(OP_UNION, TREE, MAKE_TREE_NODE(OP_EMPTY, TREE, NULL()))
               TREE => MAKE_TREE_NODE(OP_CONCAT, PTR, TREE)
               COUNT = COUNT + 1
            END DO

            COUNT = 1
            DO WHILE (COUNT < MIN)
               TREE => MAKE_TREE_NODE(OP_CONCAT, TREE, PTR)
               COUNT = COUNT + 1
            END DO

         END IF
      END IF

   END FUNCTION RANGE_MIN_MAX


   FUNCTION CHAR_CLASS(TAPE) RESULT(TREE)
      IMPLICIT NONE
      TYPE(TAPE_T), INTENT(INOUT) :: TAPE
      TYPE(TREE_T), POINTER :: TREE
      TYPE(SEGMENT_T), ALLOCATABLE :: SEGLIST(:)

      CHARACTER(:), ALLOCATABLE :: BUF
      INTEGER :: SIZ, I, INEXT, IEND, J
      LOGICAL :: INVERTED

      TREE => NULL()

      BUF = ''
      DO WHILE (TAPE%CURRENT_TOKEN /= TK_RSBRACKET)
         IEND = IDXUTF8(TAPE%TOKEN_CHAR, 1)
         BUF = BUF//TAPE%TOKEN_CHAR(1:IEND)
         CALL TAPE%GET_TOKEN(CLASS = .TRUE.)
      END DO

      INVERTED = .FALSE.
      ! is there '^' at first?
      IF (BUF(1:1) == HAT) THEN
         INVERTED = .TRUE.
         BUF = BUF(2:LEN(BUF))
      END IF

      SIZ = LEN_UTF8(BUF)

      SIZ = SIZ - 2*COUNT_TOKEN(BUF(2:LEN_TRIM(BUF)-1), HYPHEN)

      IF (BUF(LEN_TRIM(BUF):LEN_TRIM(BUF)) == HYPHEN) SIZ = SIZ -1

      ALLOCATE(SEGLIST(SIZ))


      IEND = LEN(BUF)
      I = 1
      J = 1
      BUF = BUF//CHAR(0) !空文字を末尾に追加する。

      DO WHILE (I <= IEND)

         INEXT = IDXUTF8(BUF, I) + 1

         ! 次の文字がハイフンでないならば、
         IF (BUF(INEXT:INEXT) /= HYPHEN) THEN
            SEGLIST(J)%MIN = ICHAR_UTF8(BUF(I:INEXT-1))
            SEGLIST(J)%MAX = ICHAR_UTF8(BUF(I:INEXT-1))
            J = J + 1

         ELSE
            SEGLIST(J)%MIN = ICHAR_UTF8(BUF(I:INEXT-1))

            ! 2文字すすめる
            I = INEXT +1
            INEXT = IDXUTF8(BUF, I) + 1

            SEGLIST(J)%MAX = ICHAR_UTF8(BUF(I:INEXT-1))
            J = J + 1
         END IF

         ! 先頭の文字がハイフンならば
         IF (J == 1 .AND. BUF(1:1) == HYPHEN) THEN
            SEGLIST(1)%MIN = ICHAR_UTF8(HYPHEN)
            SEGLIST(1)%MAX = ICHAR_UTF8(HYPHEN)
            J = J + 1
            CYCLE
         END IF

         IF (I == IEND .AND. BUF(IEND:IEND) == HYPHEN) THEN
            SEGLIST(SIZ)%MAX = UTF8_CODE_MAX
            EXIT
         END IF

         I = INEXT
      END DO

      IF (INVERTED) THEN
         CALL INVERT_SEGMENT_LIST(SEGLIST)
      END IF

      ALLOCATE(TREE)
      ALLOCATE(TREE%C(SIZE(SEGLIST, DIM=1)))

      TREE%C(:) = SEGLIST(:)
      TREE%OP = OP_CHAR

      TREE_NODE_COUNT = TREE_NODE_COUNT + 1
      ARRAY(TREE_NODE_COUNT)%NODE => TREE

   END FUNCTION CHAR_CLASS


   FUNCTION MAKE_TREE_CRLF() RESULT(TREE)
      IMPLICIT NONE
      TYPE(TREE_T), POINTER :: TREE
      TYPE(TREE_T), POINTER :: CR, LF

      TREE => NULL()
      CR => NULL()
      LF => NULL()

      ALLOCATE(CR)
      ALLOCATE(CR%C(1))
      CR%C(1) = SEG_CR
      CR%OP = OP_CHAR

      TREE_NODE_COUNT = TREE_NODE_COUNT + 1
      ARRAY(TREE_NODE_COUNT)%NODE => CR

      ALLOCATE(LF)
      ALLOCATE(LF%C(1))

      LF%C(1) = SEG_LF
      LF%OP = OP_CHAR

      TREE_NODE_COUNT = TREE_NODE_COUNT + 1
      ARRAY(TREE_NODE_COUNT)%NODE => LF

      TREE => MAKE_TREE_NODE(OP_UNION, LF, MAKE_TREE_NODE(OP_CONCAT, CR, LF))
   END FUNCTION MAKE_TREE_CRLF


   FUNCTION SHORTHAND(TAPE) RESULT(TREE)
      IMPLICIT NONE
      TYPE(TAPE_T), INTENT(INOUT) :: TAPE
      TYPE(TREE_T), POINTER :: TREE, LEFT, RIGHT

      TYPE(SEGMENT_T), ALLOCATABLE :: SEGLIST(:)
      TYPE(SEGMENT_T) :: SEG

      TREE => NULL()
      LEFT => NULL()
      RIGHT => NULL()

      SELECT CASE (TRIM(TAPE%TOKEN_CHAR))
      CASE (ESCAPE_T)
         TREE => MAKE_ATOM(SEG_TAB)
         RETURN

      CASE (ESCAPE_N)
         TREE => MAKE_TREE_CRLF()
         RETURN

      CASE (ESCAPE_R)
         TREE => MAKE_ATOM(SEG_CR)
         RETURN

      CASE (ESCAPE_D)
         TREE => MAKE_ATOM(SEG_DIGIT)
         RETURN

      CASE (ESCAPE_D_CAPITAL)
         ALLOCATE(SEGLIST(1))
         SEGLIST(1) = SEG_DIGIT
         CALL INVERT_SEGMENT_LIST(SEGLIST)

      CASE (ESCAPE_W)
         ALLOCATE(SEGLIST(4))
         SEGLIST(1) = SEG_LOWERCASE
         SEGLIST(2) = SEG_UPPERCASE
         SEGLIST(3) = SEG_DIGIT
         SEGLIST(4) = SEG_UNDERSCORE

      CASE (ESCAPE_W_CAPITAL)
         ALLOCATE(SEGLIST(4))
         SEGLIST(1) = SEG_LOWERCASE
         SEGLIST(2) = SEG_UPPERCASE
         SEGLIST(3) = SEG_DIGIT
         SEGLIST(4) = SEG_UNDERSCORE
         CALL INVERT_SEGMENT_LIST(SEGLIST)

      CASE (ESCAPE_S)
         ALLOCATE(SEGLIST(6))
         SEGLIST(1) = SEG_SPACE
         SEGLIST(2) = SEG_TAB
         SEGLIST(3) = SEG_CR
         SEGLIST(4) = SEG_LF
         SEGLIST(5) = SEG_FF
         SEGLIST(6) = SEG_ZENKAKU_SPACE

      CASE (ESCAPE_S_CAPITAL)
         ALLOCATE(SEGLIST(6))
         SEGLIST(1) = SEG_SPACE
         SEGLIST(2) = SEG_TAB
         SEGLIST(3) = SEG_CR
         SEGLIST(4) = SEG_LF
         SEGLIST(5) = SEG_FF
         SEGLIST(6) = SEG_ZENKAKU_SPACE
         CALL INVERT_SEGMENT_LIST(SEGLIST)

      CASE DEFAULT
         SEG = SEGMENT_T(ICHAR_UTF8(TAPE%TOKEN_CHAR), ICHAR_UTF8(TAPE%TOKEN_CHAR))
         TREE => MAKE_ATOM(SEG)
         RETURN
      END SELECT

      ALLOCATE(TREE)
      ALLOCATE(TREE%C(SIZE(SEGLIST, DIM=1)))

      TREE%C(:) = SEGLIST(:)
      TREE%OP = OP_CHAR

      TREE_NODE_COUNT = TREE_NODE_COUNT +1
      ARRAY(TREE_NODE_COUNT)%NODE => TREE

      DEALLOCATE(SEGLIST)

   END FUNCTION SHORTHAND


   SUBROUTINE INVERT_SEGMENT_LIST(LIST)
      IMPLICIT NONE
      TYPE(SEGMENT_T), INTENT(INOUT), ALLOCATABLE :: LIST(:)

      LOGICAL, ALLOCATABLE :: UNICODE(:)
      LOGICAL, ALLOCATABLE :: INVERTED(:)

      INTEGER :: I, J, COUNT

      ALLOCATE(UNICODE(UTF8_CODE_MIN:UTF8_CODE_MAX))
      ALLOCATE(INVERTED((UTF8_CODE_MIN-1):(UTF8_CODE_MAX+1)))
      UNICODE(:) = .FALSE.
      INVERTED(:) = .FALSE.

      DO I = UTF8_CODE_MIN, UTF8_CODE_MAX
         DO J = 1, SIZE(LIST, DIM=1)
            UNICODE(I) = UNICODE(I) .OR. (LIST(J)%MIN <= I .AND. I <= LIST(J)%MAX)
         END DO
      END DO

      INVERTED(UTF8_CODE_MIN-1) = .FALSE.
      INVERTED(UTF8_CODE_MAX+1) = .FALSE.
      INVERTED(UTF8_CODE_MIN:UTF8_CODE_MAX) = .NOT. UNICODE(UTF8_CODE_MIN:UTF8_CODE_MAX)

      COUNT = 0
      DO I = UTF8_CODE_MIN, UTF8_CODE_MAX
         IF (.NOT. INVERTED(I-1) .AND. INVERTED(I)) COUNT = COUNT + 1
      END DO

      DEALLOCATE(LIST)
      ALLOCATE(LIST(COUNT))

      COUNT = 1
      DO I = UTF8_CODE_MIN, UTF8_CODE_MAX+1
         IF (.NOT. INVERTED(I-1) .AND. INVERTED(I)) THEN
            LIST(COUNT)%MIN = I
         END IF

         IF (INVERTED(I-1) .AND. .NOT. INVERTED(I)) THEN
            LIST(COUNT)%MAX = I-1
            COUNT = COUNT + 1
         END IF
      END DO

   END SUBROUTINE INVERT_SEGMENT_LIST

!=====================================================================!

   RECURSIVE SUBROUTINE PRINT_TREE_INTERNAL(TREE)
      IMPLICIT NONE
      TYPE(TREE_T), INTENT(IN) :: TREE

      SELECT CASE (TREE%OP)
      CASE (OP_CHAR)
         WRITE(STDERR, '(a)', ADVANCE='no') TRIM(PRINT_CLASS_SIMPLIFY(TREE))
      CASE (OP_CONCAT)
         WRITE(STDERR, '(a)', ADVANCE='no') "(concatenate "
         CALL PRINT_TREE_INTERNAL(TREE%LEFT)
         WRITE(STDERR, '(a)', ADVANCE='no') ' '
         CALL PRINT_TREE_INTERNAL(TREE%RIGHT)
         WRITE(STDERR, '(a)', ADVANCE='no') ')'

      CASE (OP_UNION)
         WRITE(STDERR, '(a)', ADVANCE='no') "(or "
         CALL PRINT_TREE_INTERNAL(TREE%LEFT)
         WRITE(STDERR, '(a)', ADVANCE='no') ' '
         CALL PRINT_TREE_INTERNAL(TREE%RIGHT)
         WRITE(STDERR, '(a)', ADVANCE='no') ')'

      CASE (OP_CLOSURE)
         WRITE(STDERR, '(a)', ADVANCE='no') "(closure"
         CALL PRINT_TREE_INTERNAL(TREE%LEFT)
         WRITE(STDERR, '(a)', ADVANCE='no') ')'

      CASE (OP_EMPTY)
         WRITE(STDERR, '(a)', ADVANCE='no') 'EMPTY'

      CASE DEFAULT
         WRITE(STDERR, '(a)') "This will not occur in 'print_tree'."
         ERROR STOP
      END SELECT
   END SUBROUTINE PRINT_TREE_INTERNAL


   FUNCTION PRINT_CLASS_SIMPLIFY (P) RESULT(STR)
      IMPLICIT NONE
      TYPE(TREE_T), INTENT(IN) :: P
      CHARACTER(:), ALLOCATABLE :: STR

      INTEGER(INT32) :: SIZ, J
      CHARACTER(:),ALLOCATABLE :: BUF

      STR = ''
      SIZ = SIZE(P%C, DIM=1)

      IF (SIZ == 0) RETURN

      IF (P%C(1) == SEG_LF) THEN
         STR = '<LF>'
         RETURN

      ELSE IF (P%C(1) == SEG_CR) THEN
         STR = '<CR>'
         RETURN

      ELSE IF (SIZ == 1 .AND. P%C(1)%MIN == P%C(1)%MAX) THEN
         STR = '"'//CHAR_UTF8(P%C(1)%MIN)//'"'
         RETURN

      ELSE IF (SIZ == 1 .AND. P%C(1) == SEG_ANY) THEN
         STR = '<ANY>'
         RETURN
      END IF

      BUF = '[ '
      DO J = 1, SIZ

         IF (P%C(J) == SEG_LF) THEN
            BUF = BUF//'<LF>; '

         ELSE IF (P%C(J) == SEG_TAB) THEN
            BUF = BUF//'<TAB>; '

         ELSE IF (P%C(J) == SEG_CR) THEN
            BUF = BUF//'<CR>; '

         ELSE IF (P%C(J) == SEG_FF) THEN
            BUF = BUF//'<FF>; '

         ELSE IF (P%C(J) == SEG_SPACE) THEN
            BUF = BUF//'<SPACE>; '

         ELSE IF (P%C(J) == SEG_ZENKAKU_SPACE) THEN
            BUF = BUF//'<ZENKAKU SPACE>; '

         ELSE IF (P%C(J)%MAX == UTF8_CODE_MAX) THEN
            BUF = BUF//'"'//CHAR_UTF8(P%C(J)%MIN)//'"-"'//"<U+1FFFFF>"//'; '

         ELSE
            BUF = BUF//'"'//CHAR_UTF8(P%C(J)%MIN)//'"-"'//CHAR_UTF8(P%C(J)%MAX)//'"; '
         END IF
      END DO

      BUF = TRIM(BUF)//']'

      STR = TRIM(BUF)

   END FUNCTION PRINT_CLASS_SIMPLIFY


END MODULE ModLib_SyntaxTree

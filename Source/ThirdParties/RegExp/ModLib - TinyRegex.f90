! *************************************************************************************************
!                                    ____  ___________________  __
!                                   / __ \/ ____/ ____/ ____/ |/ /
!                                  / /_/ / __/ / / __/ __/  |   /
!                                 / _, _/ /___/ /_/ / /___ /   |
!                                /_/ |_/_____/\____/_____//_/|_|
!
! MIT License
!
! (C) Federico Perini, 2022
!     A Fortran port of the tiny-regex library.
!
!     https://github.com/kokke/tiny-regex-c
!     Code inspired by Rob Pike's regex code described in:
!     http://www.cs.princeton.edu/courses/archive/spr09/cos333/beautiful.html
!
! *************************************************************************************************
MODULE ModLib_TinyRegex
    USE ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT
    IMPLICIT NONE
    PRIVATE

    PUBLIC :: PARSE_PATTERN
    PUBLIC :: CHECK_PATTERN
    PUBLIC :: REGEX

    ! Character kind
    INTEGER, PARAMETER, PUBLIC :: RCK = SELECTED_CHAR_KIND("ascii")

    LOGICAL, PARAMETER, PUBLIC :: RE_DOT_MATCHES_NEWLINE = .TRUE. ! Define .false. if you DON'T want '.' to maTCH '\R' + '\N'
    INTEGER, PARAMETER, PUBLIC :: MAX_REGEXP_OBJECTS = 512        ! Max number of regex symbols in expression.
    INTEGER, PARAMETER, PUBLIC :: MAX_CHAR_CLASS_LEN = 1024       ! Max length of character-class buffer in.

    ! Turn on verbosity for debugging
    LOGICAL, PARAMETER :: DEBUG = .FALSE.

    ! Supported patterns
    INTEGER, PARAMETER :: UNUSED         = 0
    INTEGER, PARAMETER :: DOT            = 1   ! '.'        Dot, matches any character
    INTEGER, PARAMETER :: BEGIN_WITH     = 2   ! '^'        Start anchor, matches beginning of string
    INTEGER, PARAMETER :: END_WITH       = 3   ! '$'        End anchor, matches end of string
    INTEGER, PARAMETER :: QUESTIONMARK   = 4   ! '?'        Question, match zero or one (non-greedy)
    INTEGER, PARAMETER :: STAR           = 5   ! '*'        Asterisk, match zero or more (greedy)
    INTEGER, PARAMETER :: PLUS           = 6   ! '+'        Plus, match one or more (greedy)
    INTEGER, PARAMETER :: ATCHAR         = 7   ! '[a-zA-Z]' Character ranges, the character set of the ranges { A-Z | A-Z }
    INTEGER, PARAMETER :: AT_CHAR_CLASS  = 8   ! '[abc]'    Character class, match if one of {'a', 'b', 'c'}
    INTEGER, PARAMETER :: INV_CHAR_CLASS = 9   ! '[^abc]'   Inverted class, match if NOT one of {'a', 'b', 'c'} -- NOTE: FEATURE IS CURRENTLY BROKEN!
    INTEGER, PARAMETER :: DIGIT          = 10  ! '\d'       Digits, [0-9]
    INTEGER, PARAMETER :: NOT_DIGIT      = 11  ! '\D'       Non-digits
    INTEGER, PARAMETER :: ALPHA          = 12  ! '\w'       Alphanumeric, [a-zA-Z0-9_]
    INTEGER, PARAMETER :: NOT_ALPHA      = 13  ! '\W'       Non-alphanumeric
    INTEGER, PARAMETER :: WHITESPACE     = 14  ! '\s'       Whitespace, \t \f \r \n \v and spaces
    INTEGER, PARAMETER :: NOT_WHITESPACE = 15  ! '\S'       Non-whitespace

    CHARACTER(KIND=RCK,LEN=*), PARAMETER :: TYPES(*) = [ CHARACTER(LEN=14) :: "UNUSED", "DOT", "BEGIN", "END", "QUESTIONMARK", &
        "STAR", "PLUS", "CHAR", "CHAR_CLASS", "INV_CHAR_CLASS", "DIGIT", "NOT_DIGIT", "ALPHA", "NOT_ALPHA", &
        "WHITESPACE", "NOT_WHITESPACE", "BRANCH" ]

    ! Characters
    CHARACTER(KIND=RCK,LEN=*), PARAMETER :: LOWERCASE="abcdefghijklmnopqrstuvwxyz"
    CHARACTER(KIND=RCK,LEN=*), PARAMETER :: UPPERCASE="ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    CHARACTER(KIND=RCK), PARAMETER, PUBLIC :: UNDERSCORE = "_"
    CHARACTER(KIND=RCK), PARAMETER, PUBLIC :: SPACE      = " "
    CHARACTER(KIND=RCK), PARAMETER, PUBLIC :: DASH       = "-"
    CHARACTER(KIND=RCK), PARAMETER, PUBLIC :: CNULL      = ACHAR( 0,KIND=RCK)  ! \0 or null character
    CHARACTER(KIND=RCK), PARAMETER, PUBLIC :: NEWLINE    = ACHAR(10,KIND=RCK)  ! \n or line feed
    CHARACTER(KIND=RCK), PARAMETER, PUBLIC :: BACKSPCE   = ACHAR( 8,KIND=RCK)  ! \b or backspace character
    CHARACTER(KIND=RCK), PARAMETER, PUBLIC :: TAB        = ACHAR( 9,KIND=RCK)  ! \t or tabulation character

    ! Regex pattern element
    TYPE, PUBLIC :: REGEX_TOKEN

        INTEGER :: TYPE = UNUSED

        ! Single or multi-character pattern
        CHARACTER(KIND=RCK,LEN=:), ALLOCATABLE :: CCL
        CONTAINS

          PROCEDURE :: PRINT => PRINT_PATTERN
          PROCEDURE :: DESTROY => PAT_DESTROY
          PROCEDURE :: MATCH => PAT_MATCH

    END TYPE REGEX_TOKEN

    TYPE, PUBLIC :: REGEX_PATTERN

        INTEGER :: N = 0

        TYPE(REGEX_TOKEN), DIMENSION(MAX_REGEXP_OBJECTS) :: PATTERN

        CONTAINS

           PROCEDURE :: NEW     => NEW_FROM_PATTERN
           PROCEDURE :: WRITE   => WRITE_PATTERN
           PROCEDURE :: NRULES
           PROCEDURE :: DESTROY
           FINAL     :: FINALIZE

    END TYPE REGEX_PATTERN

    ! Public interface
    INTERFACE REGEX
        MODULE PROCEDURE RE_MATCH
        MODULE PROCEDURE RE_MATCH_NOBACK
        MODULE PROCEDURE RE_MATCH_NOLENGTH
        MODULE PROCEDURE RE_MATCH_NOLENGTH_NOBACK
        MODULE PROCEDURE RE_MATCHP
        MODULE PROCEDURE RE_MATCHP_NOBACK
        MODULE PROCEDURE RE_MATCHP_NOLENGTH
        MODULE PROCEDURE RE_MATCHP_NOLENGTH_NOBACK
    END INTERFACE REGEX

    ! Override default constructor for ifort bug
    INTERFACE REGEX_TOKEN
        MODULE PROCEDURE PAT_FROM_CHAR
    END INTERFACE REGEX_TOKEN


    CONTAINS

    ! Construct a regex pattern from a single character
    ELEMENTAL TYPE(REGEX_TOKEN) FUNCTION PAT_FROM_CHAR(TYPE,CCL) RESULT(THIS)
       INTEGER, INTENT(IN) :: TYPE
       CHARACTER(KIND=RCK), INTENT(IN) :: CCL
       CALL PAT_DESTROY(THIS)
       THIS%TYPE = TYPE
       ALLOCATE(CHARACTER(LEN=1,KIND=RCK) :: THIS%CCL)
       THIS%CCL(1:1) = CCL
    END FUNCTION PAT_FROM_CHAR

    ! Check that a pattern matches the expected result
    LOGICAL FUNCTION CHECK_PATTERN(STRING,PATTERN,EXPECTED) RESULT(SUCCESS)
       CHARACTER(LEN=*,KIND=RCK), INTENT(IN) :: STRING
       CHARACTER(LEN=*,KIND=RCK), INTENT(IN) :: PATTERN
       CHARACTER(LEN=*,KIND=RCK), INTENT(IN) :: EXPECTED

       INTEGER :: IDX,LENGTH

       IDX = REGEX(STRING,PATTERN,LENGTH)

       IF (IDX>0) THEN
           SUCCESS = LENGTH==LEN(EXPECTED)
           IF (SUCCESS) SUCCESS = STRING(IDX:IDX+LENGTH-1)==EXPECTED
       ELSE
           SUCCESS = LEN(EXPECTED)<=0
       END IF

       IF (DEBUG .AND. .NOT.SUCCESS) THEN
         PRINT "('[regex] test FAILED: text=',a,' pattern=',a,' index=',i0,' len=',i0)", &
                                               STRING,PATTERN,IDX,LENGTH
         STOP 1
       ENDIF

    END FUNCTION CHECK_PATTERN

    ! Clean up a pattern
    ELEMENTAL SUBROUTINE PAT_DESTROY(THIS)
       CLASS(REGEX_TOKEN), INTENT(INOUT) :: THIS
       INTEGER :: IERR
       THIS%TYPE = UNUSED
       DEALLOCATE(THIS%CCL,STAT=IERR)
    END SUBROUTINE PAT_DESTROY

    ! Number of rules in the current pattern
    ELEMENTAL INTEGER FUNCTION NRULES(THIS)
       CLASS(REGEX_PATTERN), INTENT(IN) :: THIS
       INTEGER :: I
       NRULES = 0
       DO I=1,MAX_REGEXP_OBJECTS
          IF (THIS%PATTERN(I)%TYPE==UNUSED) RETURN
          NRULES = NRULES + 1
       END DO
    END FUNCTION NRULES

    SUBROUTINE WRITE_PATTERN(THIS,IUNIT)
        CLASS(REGEX_PATTERN), INTENT(IN) :: THIS
        INTEGER, OPTIONAL, INTENT(IN) :: IUNIT

        INTEGER :: I,U

        IF (PRESENT(IUNIT)) THEN
            U = IUNIT
        ELSE
            U = OUTPUT_UNIT
        END IF

        DO I=1,THIS%NRULES()
           WRITE(U,'(a)') THIS%PATTERN(I)%PRINT()
        END DO

    END SUBROUTINE WRITE_PATTERN

    ELEMENTAL SUBROUTINE DESTROY(THIS)
        CLASS(REGEX_PATTERN), INTENT(INOUT) :: THIS
        INTEGER :: I
        DO I=1,MAX_REGEXP_OBJECTS
            CALL THIS%PATTERN(I)%DESTROY()
        END DO
    END SUBROUTINE DESTROY

    SUBROUTINE FINALIZE(THIS)
        TYPE(REGEX_PATTERN), INTENT(INOUT) :: THIS
        INTEGER :: I
        DO I=1,MAX_REGEXP_OBJECTS
            CALL THIS%PATTERN(I)%DESTROY()
        END DO
    END SUBROUTINE FINALIZE

    ! Check that a character matches a dot ("any character") pattern
    ELEMENTAL LOGICAL FUNCTION MATCHDOT(C)
       CHARACTER(KIND=RCK), INTENT(IN) :: C
       IF (RE_DOT_MATCHES_NEWLINE) THEN
          MATCHDOT = .TRUE.
       ELSE
          MATCHDOT = C/=NEWLINE .AND. C/=BACKSPCE
       END IF
    END FUNCTION MATCHDOT

    ELEMENTAL LOGICAL FUNCTION ISMETACHAR(C)
       CHARACTER(KIND=RCK), INTENT(IN) :: C
       ISMETACHAR = INDEX("sSwWdD",C)>0
    END FUNCTION ISMETACHAR

    PURE LOGICAL FUNCTION MATCHMETACHAR(C, STR)
       CHARACTER(KIND=RCK), INTENT(IN) :: C
       CHARACTER(KIND=RCK,LEN=*), INTENT(IN) :: STR

       SELECT CASE (STR(1:1))
          CASE ('d');   MATCHMETACHAR =      ISDIGIT(C)
          CASE ('D');   MATCHMETACHAR = .NOT.ISDIGIT(C)
          CASE ('w');   MATCHMETACHAR =      ISALPHANUM(C)
          CASE ('W');   MATCHMETACHAR = .NOT.ISALPHANUM(C)
          CASE ('s');   MATCHMETACHAR =      ISSPACE(C)
          CASE ('S');   MATCHMETACHAR = .NOT.ISSPACE(C)
          CASE DEFAULT; MATCHMETACHAR = C==STR(1:1)
       END SELECT
    END FUNCTION MATCHMETACHAR

    ELEMENTAL LOGICAL FUNCTION ISDIGIT(C)
       CHARACTER(KIND=RCK), INTENT(IN) :: C
       ISDIGIT = INDEX("1234567890",C)>0
    END FUNCTION ISDIGIT

    ELEMENTAL LOGICAL FUNCTION ISALPHA(C)
       CHARACTER(KIND=RCK), INTENT(IN) :: C
       ISALPHA = INDEX(LOWERCASE,C)>0 .OR. INDEX(UPPERCASE,C)>0
    END FUNCTION ISALPHA

    ELEMENTAL LOGICAL FUNCTION ISALPHANUM(C)
       CHARACTER(KIND=RCK), INTENT(IN) :: C
       ISALPHANUM = ISALPHA(C) .OR. ISDIGIT(C) .OR. C==UNDERSCORE
    END FUNCTION ISALPHANUM

    ELEMENTAL LOGICAL FUNCTION ISSPACE(C)
       CHARACTER(KIND=RCK), INTENT(IN) :: C
       ISSPACE = C==SPACE
    END FUNCTION ISSPACE

    ! Match range of the tye 0-9  or 5-7 etc.
    ELEMENTAL LOGICAL FUNCTION MATCHRANGE(C,STR)
       CHARACTER(KIND=RCK), INTENT(IN) :: C
       CHARACTER(KIND=RCK,LEN=*), INTENT(IN) :: STR ! the range pattern

       MATCHRANGE = LEN(STR)>=3; IF (.NOT.MATCHRANGE) RETURN
       MATCHRANGE = C /= DASH &
                    .AND. STR(1:1) /= DASH &
                    .AND. STR(2:2) == DASH &
                    .AND. IACHAR(C)>=IACHAR(STR(1:1)) &
                    .AND. IACHAR(C)<=IACHAR(STR(3:3))    ! Range (number/letters) is in increasing order

    END FUNCTION MATCHRANGE

    LOGICAL FUNCTION MATCHCHARCLASS(C,STR) RESULT(MATCH)
       CHARACTER(KIND=RCK), INTENT(IN) :: C         ! The current character
       CHARACTER(KIND=RCK,LEN=*), INTENT(IN) :: STR ! The charclass contents

       INTEGER :: I

       MATCH = .FALSE.
       I = 0

       ! All characters in the charclass contents
       LOOP: DO WHILE (I<LEN(STR))

          I = I+1

          ! We're in a range: must check this further
          IF (MATCHRANGE(C,STR(I:))) THEN
            MATCH = .TRUE.
            RETURN

          ! Escaped character? look what's next
          ELSEIF (STR(I:I) == '\') THEN

             I = I+1

             ! Valid escaped sequence

             IF (MATCHMETACHAR(C,STR(I:))) THEN
                MATCH = .TRUE.
                RETURN
             ELSEIF (C==STR(I:I) .AND. (.NOT.ISMETACHAR(C))) THEN
                MATCH = .TRUE.
                RETURN
             ENDIF

          ELSEIF (C==STR(I:I)) THEN

             ! Character match
             IF (C==DASH) THEN

                ! Dash is a single character only if it does not have characters before/after
                MATCH = I==1 .OR. I+1>LEN(STR)

             ELSE
                MATCH = .TRUE.
             END IF
             RETURN
          END IF

       END DO LOOP

       IF (DEBUG) PRINT *, 'charclass: no match on i=',I,' str=',TRIM(STR),' c=',C

    END FUNCTION MATCHCHARCLASS

    RECURSIVE LOGICAL FUNCTION MATCHQUESTION(P, PATTERN, TEXT, MATCHLENGTH)
       TYPE(REGEX_TOKEN), INTENT(IN) :: P, PATTERN(:)
       CHARACTER(LEN=*,KIND=RCK), INTENT(IN) :: TEXT
       INTEGER, INTENT(INOUT) :: MATCHLENGTH

       MATCHQUESTION = .FALSE.

       IF (P%TYPE == UNUSED) THEN
          MATCHQUESTION = .TRUE.
          RETURN
       ELSEIF (MATCHPATTERN(PATTERN, TEXT, MATCHLENGTH)) THEN
          MATCHQUESTION = .TRUE.
          RETURN
       ELSEIF (LEN(TEXT)>0) THEN
          IF (PAT_MATCH(P,TEXT) .AND. LEN(TEXT)>1) THEN
             IF (MATCHPATTERN(PATTERN,TEXT(2:),MATCHLENGTH)) THEN
                MATCHLENGTH  = MATCHLENGTH+1
                MATCHQUESTION = .TRUE.
                RETURN
             ENDIF
          END IF
       END IF

    END FUNCTION MATCHQUESTION

    RECURSIVE LOGICAL FUNCTION MATCHSTAR(P, PATTERN, TEXT, IT0, MATCHLENGTH)
       TYPE(REGEX_TOKEN), INTENT(IN) :: P, PATTERN(:)
       CHARACTER(LEN=*,KIND=RCK), INTENT(IN) :: TEXT
       INTEGER, INTENT(IN)    :: IT0 ! starting point
       INTEGER, INTENT(INOUT) :: MATCHLENGTH

       INTEGER :: PRELEN,IT

       IF (DEBUG) PRINT *, 'match star, length=',MATCHLENGTH,' it0=',IT0,' lenm=',LEN(TEXT)

       IF (LEN(TEXT)<=0) THEN
          MATCHSTAR = .FALSE.
          RETURN
       END IF

       ! Save input variables
       PRELEN   = MATCHLENGTH
       IT = IT0

       DO WHILE (IT>0 .AND. IT<=LEN(TEXT))
          IF (.NOT.PAT_MATCH(P, TEXT(IT:))) EXIT
          IT          = IT+1
          MATCHLENGTH = MATCHLENGTH+1
       END DO

       DO WHILE (IT>=IT0)
         MATCHSTAR = MATCHPATTERN(PATTERN, TEXT(IT:), MATCHLENGTH)
         IT          = IT-1
         IF (MATCHSTAR) RETURN
         MATCHLENGTH = MATCHLENGTH-1
       END DO

       MATCHLENGTH = PRELEN
       MATCHSTAR   = .FALSE.

    END FUNCTION MATCHSTAR

    RECURSIVE LOGICAL FUNCTION MATCHPLUS(P, PATTERN, TEXT, IT0, MATCHLENGTH)
       TYPE(REGEX_TOKEN), INTENT(IN) :: P, PATTERN(:)
       CHARACTER(LEN=*,KIND=RCK), INTENT(IN) :: TEXT
       INTEGER, INTENT(IN) :: IT0
       INTEGER, INTENT(INOUT) :: MATCHLENGTH

       INTEGER :: IT

       IF (DEBUG) PRINT *, 'matching PLUS pattern'

       IT = IT0
       DO WHILE (IT>0 .AND. IT<=LEN(TEXT))
          IF (.NOT. PAT_MATCH(P, TEXT(IT:))) EXIT
          IT = IT+1
          MATCHLENGTH = MATCHLENGTH+1
       END DO

       DO WHILE (IT>IT0)
          MATCHPLUS = MATCHPATTERN(PATTERN, TEXT(IT:), MATCHLENGTH)
          IT = IT-1
          IF (MATCHPLUS) RETURN
          MATCHLENGTH = MATCHLENGTH-1
       END DO

       MATCHPLUS = .FALSE.

    END FUNCTION MATCHPLUS

    ! Find matches of the given pattern in the string
    INTEGER FUNCTION RE_MATCH(STRING, PATTERN, LENGTH, BACK) RESULT(INDEX)
       CHARACTER(*,KIND=RCK), INTENT(IN) :: PATTERN
       CHARACTER(*,KIND=RCK), INTENT(IN) :: STRING
       INTEGER, INTENT(OUT) :: LENGTH
       LOGICAL, INTENT(IN)  :: BACK
       TYPE (REGEX_PATTERN) :: COMMAND

       COMMAND = PARSE_PATTERN(PATTERN)
       INDEX = RE_MATCHP(STRING,COMMAND,LENGTH,BACK)

    END FUNCTION RE_MATCH

    ! Find matches of the given pattern in the string
    INTEGER FUNCTION RE_MATCH_NOBACK(STRING, PATTERN, LENGTH) RESULT(INDEX)
       CHARACTER(*,KIND=RCK), INTENT(IN) :: PATTERN
       CHARACTER(*,KIND=RCK), INTENT(IN) :: STRING
       INTEGER, INTENT(OUT) :: LENGTH
       TYPE (REGEX_PATTERN) :: COMMAND

       COMMAND = PARSE_PATTERN(PATTERN)
       INDEX = RE_MATCHP(STRING,COMMAND,LENGTH,.FALSE.)

    END FUNCTION RE_MATCH_NOBACK

    ! Find matches of the given pattern in the string
    INTEGER FUNCTION RE_MATCH_NOLENGTH(STRING, PATTERN, BACK) RESULT(INDEX)
       CHARACTER(*,KIND=RCK), INTENT(IN) :: PATTERN
       CHARACTER(*,KIND=RCK), INTENT(IN) :: STRING
       LOGICAL              , INTENT(IN) :: BACK

       TYPE (REGEX_PATTERN) :: COMMAND
       INTEGER :: LENGTH

       COMMAND = PARSE_PATTERN(PATTERN)
       INDEX = RE_MATCHP(STRING,COMMAND,LENGTH,BACK)

    END FUNCTION RE_MATCH_NOLENGTH

    ! Find matches of the given pattern in the string
    INTEGER FUNCTION RE_MATCH_NOLENGTH_NOBACK(STRING, PATTERN) RESULT(INDEX)
       CHARACTER(*,KIND=RCK), INTENT(IN) :: PATTERN
       CHARACTER(*,KIND=RCK), INTENT(IN) :: STRING

       TYPE (REGEX_PATTERN) :: COMMAND
       INTEGER :: LENGTH

       COMMAND = PARSE_PATTERN(PATTERN)
       INDEX = RE_MATCHP(STRING,COMMAND,LENGTH,.FALSE.)

    END FUNCTION RE_MATCH_NOLENGTH_NOBACK

    TYPE(REGEX_PATTERN) FUNCTION PARSE_PATTERN(PATTERN) RESULT(THIS)
       CHARACTER(*,KIND=RCK), INTENT(IN) :: PATTERN

       CALL NEW_FROM_PATTERN(THIS,PATTERN)

    END FUNCTION PARSE_PATTERN

    SUBROUTINE NEW_FROM_PATTERN(THIS,PATTERN)
       CLASS(REGEX_PATTERN), INTENT(INOUT) :: THIS
       CHARACTER(*,KIND=RCK), INTENT(IN) :: PATTERN

       ! Local variables
       CHARACTER(LEN=MAX_CHAR_CLASS_LEN,KIND=RCK) :: CCL_BUF ! size of buffer for chars in all char-classes in THE EXPRESSION. */
       INTEGER :: LOC,I,J,LENP,LENC
       CHARACTER(KIND=RCK) :: C

       ! Initialize class
       CALL THIS%DESTROY()
       CCL_BUF = REPEAT(SPACE,MAX_CHAR_CLASS_LEN)

       IF (DEBUG) PRINT "('[regex] parsing pattern: <',a,'>')", TRIM(PATTERN)

       I = 1 ! index in pattern
       J = 1 ! index in re-compiled
       LENP = LEN_TRIM(PATTERN)

       ! Move along the pattern string
       TO_THE_MOON: DO WHILE (I<=LENP)

         C = PATTERN(I:I)
         IF (DEBUG) PRINT "('[regex] at location ',i0,': <',a,'>')", I, C

         SELECT CASE (C)

            ! Meta-characters are single-character patterns
            CASE ('^'); THIS%PATTERN(J) = REGEX_TOKEN(BEGIN_WITH,C)
            CASE ('$'); THIS%PATTERN(J) = REGEX_TOKEN(END_WITH,C)
            CASE ('.'); THIS%PATTERN(J) = REGEX_TOKEN(DOT,C)
            CASE ('*'); THIS%PATTERN(J) = REGEX_TOKEN(STAR,C)
            CASE ('+'); THIS%PATTERN(J) = REGEX_TOKEN(PLUS,C)
            CASE ('?'); THIS%PATTERN(J) = REGEX_TOKEN(QUESTIONMARK,C)

            ! Escaped character-classes (\s, \w, ...)
            CASE ('\');

                ! Parse an escaped character class
                IF (I<LENP) THEN

                    ! There's something next: check it
                    I = I+1;

                    SELECT CASE (PATTERN(I:I))
                       CASE ('d'); THIS%PATTERN(J) = REGEX_TOKEN(DIGIT,'d')
                       CASE ('D'); THIS%PATTERN(J) = REGEX_TOKEN(NOT_DIGIT,'D')
                       CASE ('w'); THIS%PATTERN(J) = REGEX_TOKEN(ALPHA,'w')
                       CASE ('W'); THIS%PATTERN(J) = REGEX_TOKEN(NOT_ALPHA,'W')
                       CASE ('s'); THIS%PATTERN(J) = REGEX_TOKEN(WHITESPACE,'s')
                       CASE ('S'); THIS%PATTERN(J) = REGEX_TOKEN(NOT_WHITESPACE,'S')
                       CASE DEFAULT;
                            ! Escaped character: "." or "$"
                            THIS%PATTERN(J) = REGEX_TOKEN(ATCHAR,PATTERN(I:I))
                    END SELECT

                ELSE

                    ! This is the first character of a sequence *and* the end of rht pattern. store as CHAR
                    THIS%PATTERN(J) = REGEX_TOKEN(ATCHAR,C)

                ENDIF

            ! Character class
            CASE ('[')

                LOC = 1

                ! First, check if this class is negated ("^")
                IF (PATTERN(I+1:I+1)=='^') THEN
                    THIS%PATTERN(J)%TYPE = INV_CHAR_CLASS

                    I = I+1 ! Increment i to avoid including "^" in the char-buffer

                    ! incomplete pattern
                    IF (I>=LENP) THEN
                        CALL THIS%DESTROY()
                        RETURN
                    END IF

                ELSE
                    THIS%PATTERN(J)%TYPE = AT_CHAR_CLASS
                END IF

                ! Remove any escape characters
                LOC = INDEX(PATTERN(I+1:),']')
                LENC = LOC-1
                IF (LOC>0) THEN
                    CCL_BUF = PATTERN(I+1:I+LOC-1)
                    I = I+LOC
                    IF (DEBUG) PRINT "('[regex] at end of multi-character pattern: ',a)", TRIM(CCL_BUF)
                ELSE
                    ! Incomplete [] pattern
                    CALL THIS%DESTROY()
                    RETURN
                END IF

                ! If there is any escape character(s), just check that the next is nonempty
                LOC = INDEX(CCL_BUF,'\')
                IF (LOC>0) THEN
                    IF (LOC>=LEN(CCL_BUF)) THEN
                        ! stop 'incomplete escaped character inside [] pattern'
                        CALL THIS%DESTROY()
                        RETURN
                    END IF
                    IF (CCL_BUF(LOC+1:LOC+1)==SPACE) THEN
                        ! stop 'empty escaped character inside [] pattern'
                        CALL THIS%DESTROY()
                        RETURN
                    END IF
                END IF

                ! Ensure there are no spaces

                ALLOCATE(CHARACTER(LEN=LENC,KIND=RCK) :: THIS%PATTERN(J)%CCL)
                THIS%PATTERN(J)%CCL = CCL_BUF(:LENC)

         CASE DEFAULT

             ! Single character
             THIS%PATTERN(J) = REGEX_TOKEN(ATCHAR,C)

         END SELECT

         IF (DEBUG) PRINT "('[regex] added pattern ',i0,': ',a)",J,THIS%PATTERN(J)%PRINT()

         ! A pattern was added: move to next
         I = I+1
         J = J+1
         IF (J>MAX_REGEXP_OBJECTS) STOP 'max regexp reached!'

       END DO TO_THE_MOON

       ! Save number of patterns
       THIS%N = J-1
       RETURN

    END SUBROUTINE NEW_FROM_PATTERN

    FUNCTION PRINT_PATTERN(PATTERN) RESULT(MSG)
        CLASS(REGEX_TOKEN), INTENT(IN) :: PATTERN
        CHARACTER(:,KIND=RCK), ALLOCATABLE :: MSG

        CHARACTER(LEN=MAX_CHAR_CLASS_LEN,KIND=RCK) :: BUFFER
        INTEGER :: LT

        WRITE(BUFFER,1) TRIM(TYPES(PATTERN%TYPE+1)),TRIM(PATTERN%CCL)

        LT = LEN_TRIM(BUFFER)
        ALLOCATE(CHARACTER(LEN=LT,KIND=RCK) :: MSG)
        IF (LT>0) MSG(1:LT) = BUFFER(1:LT)

        1 FORMAT('type=',A,:,1X,'char=',A)

    END FUNCTION PRINT_PATTERN

    ! Match a single pattern at the g
    RECURSIVE LOGICAL FUNCTION PAT_MATCH(P, C) RESULT(MATCH)
       CLASS(REGEX_TOKEN), INTENT(IN) :: P
       CHARACTER(KIND=RCK), INTENT(IN) :: C

       SELECT CASE (P%TYPE)
          CASE (DOT);            MATCH = MATCHDOT(C)
          CASE (AT_CHAR_CLASS);  MATCH = MATCHCHARCLASS(C,P%CCL)
          CASE (INV_CHAR_CLASS); MATCH = .NOT.MATCHCHARCLASS(C,P%CCL)
          CASE (DIGIT);          MATCH = ISDIGIT(C)
          CASE (NOT_DIGIT);      MATCH = .NOT.ISDIGIT(C)
          CASE (ALPHA);          MATCH = ISALPHANUM(C)
          CASE (NOT_ALPHA);      MATCH = .NOT.ISALPHANUM(C)
          CASE (WHITESPACE);     MATCH = ISSPACE(C)
          CASE (NOT_WHITESPACE); MATCH = .NOT.ISSPACE(C)
          CASE DEFAULT;          MATCH = C==P%CCL(1:1)
       END SELECT

       IF (DEBUG) PRINT "('[regex] current pattern=',a,' at char=',a,' match? ',l1)", P%PRINT(),C,MATCH

    END FUNCTION PAT_MATCH

    INTEGER FUNCTION RE_MATCHP_NOLENGTH(STRING, PATTERN, BACK) RESULT(INDEX)
       TYPE(REGEX_PATTERN), INTENT(IN) :: PATTERN
       CHARACTER(LEN=*,KIND=RCK), INTENT(IN) :: STRING
       LOGICAL, INTENT(IN) :: BACK
       INTEGER :: MATCHLENGTH
       INDEX = RE_MATCHP(STRING, PATTERN, MATCHLENGTH, BACK)
    END FUNCTION RE_MATCHP_NOLENGTH

    INTEGER FUNCTION RE_MATCHP_NOLENGTH_NOBACK(STRING, PATTERN) RESULT(INDEX)
       TYPE(REGEX_PATTERN), INTENT(IN) :: PATTERN
       CHARACTER(LEN=*,KIND=RCK), INTENT(IN) :: STRING
       INTEGER :: MATCHLENGTH
       INDEX = RE_MATCHP(STRING, PATTERN, MATCHLENGTH, .FALSE.)
    END FUNCTION RE_MATCHP_NOLENGTH_NOBACK

    INTEGER FUNCTION RE_MATCHP_NOBACK(STRING, PATTERN, LENGTH) RESULT(INDEX)
       TYPE(REGEX_PATTERN), INTENT(IN) :: PATTERN
       CHARACTER(LEN=*,KIND=RCK), INTENT(IN) :: STRING
       INTEGER, INTENT(OUT) :: LENGTH
       INDEX = RE_MATCHP(STRING, PATTERN, LENGTH, .FALSE.)
    END FUNCTION RE_MATCHP_NOBACK


    INTEGER FUNCTION RE_MATCHP(STRING, PATTERN, LENGTH, BACK) RESULT(INDEX)
       TYPE(REGEX_PATTERN), INTENT(IN) :: PATTERN
       CHARACTER(LEN=*,KIND=RCK), INTENT(IN) :: STRING
       INTEGER, INTENT(OUT) :: LENGTH
       LOGICAL, INTENT(IN) :: BACK

       INTEGER :: FIRST,LAST,STEP

       IF (PATTERN%N>0) THEN

          IF (PATTERN%PATTERN(1)%TYPE == BEGIN_WITH) THEN

             ! String must begin with this pattern
             LENGTH = 0
             INDEX = MERGE(1,0,MATCHPATTERN(PATTERN%PATTERN(2:), STRING, LENGTH) .AND. LEN(STRING)>0)

          ELSE

             FIRST  = MERGE(1,LEN(STRING),.NOT.BACK)
             LAST   = MERGE(1,LEN(STRING),BACK)
             STEP   = SIGN(1,LAST-FIRST)

             DO INDEX=FIRST,LAST,STEP
                LENGTH = 0
                IF (MATCHPATTERN(PATTERN%PATTERN,STRING(INDEX:),LENGTH)) GOTO 1
             END DO

             INDEX = 0

          END IF

       ELSE

          ! On an empty/invalid pattern, return -1
          INDEX = -1

       END IF

       1 IF (DEBUG) THEN
          IF (INDEX==-1) THEN
             PRINT "('[regex] end: empty/invalid regex pattern. ')"
          ELSEIF (INDEX==0) THEN
             PRINT "('[regex] end: pattern not found. ')"
          ELSE
             PRINT "('[regex] end: pattern found at ',i0,': ',a)", INDEX,STRING(INDEX:)
          END IF
       END IF

    END FUNCTION RE_MATCHP


   ! Iterative matching
   RECURSIVE LOGICAL FUNCTION MATCHPATTERN(PATTERN, TEXT, MATCHLENGTH) RESULT(MATCH)
      TYPE(REGEX_TOKEN), INTENT(IN) :: PATTERN(:)
      CHARACTER(KIND=RCK,LEN=*), INTENT(IN) :: TEXT
      INTEGER, INTENT(INOUT) :: MATCHLENGTH

      INTEGER :: PRE,IP,IT

      PRE = MATCHLENGTH
      IP  = 1
      IT  = 1

      ITERATE: DO WHILE (IP<=SIZE(PATTERN))

         IF (PATTERN(IP)%TYPE == UNUSED .OR. PATTERN(IP+1)%TYPE == QUESTIONMARK) THEN

            MATCH = MATCHQUESTION(PATTERN(IP),PATTERN(IP+2:),TEXT(IT:),MATCHLENGTH)
            RETURN

         ELSEIF (PATTERN(IP+1)%TYPE == STAR) THEN

            MATCH = MATCHSTAR(PATTERN(IP),PATTERN(IP+2:), TEXT, IT, MATCHLENGTH)
            RETURN

         ELSEIF (PATTERN(IP+1)%TYPE == PLUS) THEN

            MATCH = MATCHPLUS(PATTERN(IP),PATTERN(IP+2:), TEXT, IT, MATCHLENGTH)
            RETURN

         ELSEIF (PATTERN(IP)%TYPE == END_WITH .AND. PATTERN(IP+1)%TYPE == UNUSED) THEN

            IF (DEBUG .AND. LEN(TEXT(IT:))>0) PRINT *, '[regex] at end: remaining = ',TEXT(IT:),' len=',MATCHLENGTH

            MATCH = IT>LEN(TEXT)
            RETURN

         END IF

         IF (IT>LEN(TEXT)) EXIT ITERATE

         MATCHLENGTH = MATCHLENGTH+1

         IF (DEBUG) PRINT "('[regex] matching ',i0,'-th pattern on chunk <',i0,':',i0,'>')", IP,IT,LEN(TEXT)
         IF (.NOT. PAT_MATCH(PATTERN(IP), TEXT(IT:IT))) EXIT ITERATE
         IP = IP+1
         IT = IT+1

      END DO ITERATE

      MATCHLENGTH = PRE
      MATCH = .FALSE.
      RETURN

   END FUNCTION MATCHPATTERN




END MODULE ModLib_TinyRegex

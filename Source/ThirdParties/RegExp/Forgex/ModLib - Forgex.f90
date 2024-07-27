!! Fortran Regular Expression (Forgex)
!!
!! MIT License
!!
!! (C) Amasaki Shinobu, 2023
!!     A regular expression engine for Fortran.
!!
!!     forgex_m defines APIs of Forgex.
!!
MODULE ModLib_Forgex
   USE, INTRINSIC :: ISO_FORTRAN_ENV, STDERR=>ERROR_UNIT

   USE :: ModLib_SyntaxTree
   USE :: ModLib_Automaton
   IMPLICIT NONE
   PRIVATE

   PUBLIC :: OPERATOR(.IN.)
   PUBLIC :: OPERATOR(.MATCH.)

   PUBLIC :: REGEX

   INTERFACE OPERATOR(.IN.)
      MODULE PROCEDURE :: IN__MATCHING
   END INTERFACE

   INTERFACE OPERATOR(.MATCH.)
      MODULE PROCEDURE :: MATCH__MATCHING
   END INTERFACE

   INTERFACE REGEX
      MODULE PROCEDURE :: REGEX__MATCHING
   END INTERFACE

   TYPE(AUTOMATON_T) :: CACHE
   CHARACTER(:), ALLOCATABLE :: PATTERN_CACHE

CONTAINS

   FUNCTION IN__MATCHING (PATTERN, STR) RESULT(RES)
      IMPLICIT NONE
      CHARACTER(*), INTENT(IN) :: PATTERN, STR

      CHARACTER(:), ALLOCATABLE :: BUFF
      INTEGER(INT32) :: FROM, TO
      LOGICAL :: RES

      TYPE(TREE_T), POINTER :: ROOT
      TYPE(TAPE_T) :: TAPE


      FROM = 0
      TO = 0

      IF (PATTERN /= PATTERN_CACHE) THEN

         ! initialize
         CALL CACHE%FREE()
         CALL CACHE%INIT()

         BUFF = PATTERN
         ROOT => BUILD_SYNTAX_TREE(TAPE, BUFF)

         ! call print_tree(root)

         CALL CACHE%BUILD_NFA(ROOT)

         ! call cache%print_nfa()

         CALL CACHE%CONVERT_NFA_TO_DFA()

         ! call cache%print_dfa()
         PATTERN_CACHE = PATTERN
         CALL DEALLOCATE_TREE()

      END IF

      CALL CACHE%MATCHING(CHAR(10)//STR//CHAR(10), FROM, TO)


      IF (IS_THERE_CARET_AT_THE_TOP(PATTERN)) THEN
         FROM = FROM
      ELSE
         FROM = FROM - 1
      END IF

      IF (IS_THERE_DOLLAR_AT_THE_END(PATTERN)) THEN
         TO = TO - 2
      ELSE
         TO = TO - 1
      END IF

      ! res = .true.
      IF (FROM  > 0 .AND. TO > 0) THEN
         RES = .TRUE.

      ELSE
         RES = .FALSE.
      END IF

   END FUNCTION IN__MATCHING


   FUNCTION MATCH__MATCHING(PATTERN, STR) RESULT(RES)
      IMPLICIT NONE
      CHARACTER(*), INTENT(IN) :: PATTERN, STR
      INTEGER(INT32) :: FROM, TO
      CHARACTER(:), ALLOCATABLE :: BUFF
      LOGICAL :: RES

      TYPE(TREE_T), POINTER :: ROOT
      TYPE(TAPE_T) :: TAPE

      FROM = 0
      TO = 0

      IF (PATTERN /= PATTERN_CACHE) THEN

         IF (IS_THERE_CARET_AT_THE_TOP(PATTERN)) THEN
            BUFF = PATTERN(2:LEN(PATTERN))
         ELSE
            BUFF = PATTERN(1:LEN(PATTERN))
         END IF

         IF (IS_THERE_DOLLAR_AT_THE_END(PATTERN)) THEN
            BUFF = BUFF(1:LEN_TRIM(PATTERN)-1)
         END IF

         ROOT => BUILD_SYNTAX_TREE(TAPE, BUFF)

         ! call print_tree(root)

         CALL CACHE%FREE()
         CALL CACHE%INIT()
         CALL CACHE%BUILD_NFA(ROOT)

         ! call cache%print_nfa()

         CALL CACHE%CONVERT_NFA_TO_DFA()

         ! call cache%print_dfa()
         PATTERN_CACHE = PATTERN
         CALL DEALLOCATE_TREE()

      END IF

      RES = CACHE%MATCHING_EXACTLY(STR)

      ! write(stderr, *) from, to

   END FUNCTION MATCH__MATCHING


   FUNCTION REGEX__MATCHING (PATTERN, STR, LENGTH, FROM, TO) RESULT(RES)
      IMPLICIT NONE
      CHARACTER(*), INTENT(IN) :: PATTERN, STR
      INTEGER(INT32), INTENT(INOUT), OPTIONAL :: LENGTH
      INTEGER(INT32), INTENT(INOUT), OPTIONAL :: FROM, TO
      CHARACTER(:), ALLOCATABLE :: RES

      CHARACTER(:), ALLOCATABLE :: BUFF
      INTEGER(INT32) :: FROM_L, TO_L

      TYPE(TREE_T), POINTER :: ROOT
      TYPE(TAPE_T) :: TAPE

      FROM_L = 0
      TO_L = 0

      IF (PATTERN /= PATTERN_CACHE) THEN
         BUFF = PATTERN
         ROOT => BUILD_SYNTAX_TREE(TAPE, BUFF)


         ! call print_tree(root)
         CALL CACHE%FREE()
         CALL CACHE%INIT()

         CALL CACHE%BUILD_NFA(ROOT)

         ! call cache%print_nfa()

         CALL CACHE%CONVERT_NFA_TO_DFA()

         ! call cache%print_dfa()
         PATTERN_CACHE = PATTERN
         CALL DEALLOCATE_TREE()

      END IF

      CALL CACHE%MATCHING(CHAR(10)//STR//CHAR(10), FROM_L, TO_L)

      IF (IS_THERE_CARET_AT_THE_TOP(PATTERN)) THEN
         FROM_L = FROM_L
      ELSE
         FROM_L = FROM_L - 1
      END IF

      IF (IS_THERE_DOLLAR_AT_THE_END(PATTERN)) THEN
         TO_L = TO_L - 2
      ELSE
         TO_L = TO_L - 1
      END IF

      IF (FROM_L > 0 .AND. TO_L > 0) THEN
         RES = STR(FROM_L:TO_L)
         IF (PRESENT(LENGTH)) LENGTH = TO_L - FROM_L + 1
         IF (PRESENT(FROM)) FROM = FROM_L
         IF (PRESENT(TO)) TO = TO_L
      ELSE
         RES = ''
         IF (PRESENT(LENGTH)) LENGTH = 0
         IF (PRESENT(FROM)) FROM = 0
         IF (PRESENT(TO)) TO = 0
      END IF

   END FUNCTION REGEX__MATCHING

!---------------------------------------------------------------------!

   FUNCTION IS_THERE_CARET_AT_THE_TOP(PATTERN) RESULT(RES)
      IMPLICIT NONE
      CHARACTER(*), INTENT(IN) :: PATTERN
      CHARACTER(:), ALLOCATABLE :: BUFF
      LOGICAL :: RES

      BUFF = ADJUSTL(PATTERN)

      RES = BUFF(1:1) == '^'

   END FUNCTION IS_THERE_CARET_AT_THE_TOP


   FUNCTION IS_THERE_DOLLAR_AT_THE_END(PATTERN) RESULT(RES)
      IMPLICIT NONE
      CHARACTER(*), INTENT(IN) :: PATTERN
      CHARACTER(:), ALLOCATABLE :: BUFF

      LOGICAL :: RES

      BUFF = TRIM(PATTERN)

      RES = BUFF(LEN_TRIM(BUFF):LEN_TRIM(BUFF)) == '$'

   END FUNCTION IS_THERE_DOLLAR_AT_THE_END



END MODULE ModLib_Forgex

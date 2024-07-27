!==============================================================================!
! REGEX                                                                        !
!==============================================================================!
! Module containing routines for string manipulations with regular             !
! expressions.                                                                 !
!                                                                              !
!   Implementation based on the description (and some of the code) from        !
!   https://swtch.com/~rsc/regexp. Many thanks to Russ for his excellent       !
!   webpage!                                                                   !
!                                                                              !
!------------------------------------------------------------------------------!
! Author:  Edward Higgins <ed.j.higgins@gmail.com>                             !
!------------------------------------------------------------------------------!
! Version: 0.3.1, 2017-12-04                                                   !
!------------------------------------------------------------------------------!
! This code is distributed under the MIT license.                              !
!==============================================================================!

MODULE ModLib_RegExp

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: RE_MATCH, RE_MATCH_STR, RE_SPLIT, RE_REPLACE

  INTEGER,  PARAMETER ::  PF_BUFF_SIZE    = 8192  ! Maximum size of the postfix buffer
  INTEGER,  PARAMETER ::  PF_STACK_SIZE   = 4096  ! Maximum size of the postfix stack
  INTEGER,  PARAMETER ::  NFA_MAX_PRINT   = 16    ! Maximum depth for print_state
  INTEGER,  PARAMETER ::  MAX_PAREN_DEPTH = 128   ! Maximum depth of nested ()'s

  ! Special NFA states
  INTEGER,  PARAMETER ::  NULL_ST      = -1  ! denotes a NULL node in the nfa
  INTEGER,  PARAMETER ::  SPLIT_ST     = 256 ! denotes a SPLIT node in the nfa
  INTEGER,  PARAMETER ::  MATCH_ST     = 257 ! denotes a MATCH node in the nfa

  ! /re/ and postscript operators
  INTEGER,  PARAMETER ::  STAR_OP      = 301 ! * operator (0 or more)
  INTEGER,  PARAMETER ::  PLUS_OP      = 302 ! + operator (1 or more)
  INTEGER,  PARAMETER ::  QUEST_OP     = 303 ! ? operator (0 or 1)
  INTEGER,  PARAMETER ::  OR_OP        = 304 ! | operator (a or b)
  INTEGER,  PARAMETER ::  CAT_OP       = 305 ! . operator (cats 2 fragments)
  INTEGER,  PARAMETER ::  OPEN_PAR_CH  = 306 ! ( operator (for constructing match list)
  INTEGER,  PARAMETER ::  CLOSE_PAR_CH = 307 ! ) operator (for constructing match list)

  ! NFA special matches
  INTEGER,  PARAMETER ::  ANY_CH       = 401 ! .  match (anything)
  INTEGER,  PARAMETER ::  ALPHA_CH     = 402 ! \a match ([a..z]|[A..Z])
  INTEGER,  PARAMETER ::  NUMERIC_CH   = 403 ! \d match ([0..9])
  INTEGER,  PARAMETER ::  WORD_CH      = 404 ! \w match (\d|\a|_)
  INTEGER,  PARAMETER ::  SPACE_CH     = 405 ! \s match (" "|\t)
  INTEGER,  PARAMETER ::  N_ALPHA_CH   = 406 ! \A match (anything but \a)
  INTEGER,  PARAMETER ::  N_NUMERIC_CH = 407 ! \D match (anything but \d)
  INTEGER,  PARAMETER ::  N_WORD_CH    = 408 ! \W match (anything but \w)
  INTEGER,  PARAMETER ::  N_SPACE_CH   = 409 ! \S match (anything but \s)
  INTEGER,  PARAMETER ::  START_CH     = 410 ! ^  match (start of the string)
  INTEGER,  PARAMETER ::  FINISH_CH    = 411 ! $  match (end of the string)

  LOGICAL,  PARAMETER :: DEBUG = .FALSE.

  ! List of parentheses for building the postfix (I'll be honest, I don't quite get how this works)
  TYPE  ::  PAREN_LIST
    INTEGER ::  N_ATOM
    INTEGER ::  N_ALT
  END TYPE PAREN_LIST

  ! Full NFA and list of states
  TYPE, PUBLIC :: NFA_TYPE
    TYPE(STATE),    POINTER :: HEAD             ! Starting state for the NFA
    TYPE(PTR_LIST), POINTER :: STATES => NULL() ! A list of all the states in this nfa
    INTEGER                 :: N_STATES         ! Number of states in the NFA
  END TYPE NFA_TYPE

  ! State in the NFA
  TYPE, PUBLIC  :: STATE
    INTEGER               ::  C                 ! Character/code to match
    TYPE(STATE),  POINTER ::  OUT1 => NULL()    ! Optional output 1 from the state
    TYPE(STATE),  POINTER ::  OUT2 => NULL()    ! Optional output 1 from the state
    INTEGER               ::  LAST_LIST         ! State list tracker for fast NFA running
  END TYPE STATE

  ! List of pointers to states
  TYPE  :: PTR_LIST
    TYPE(STATE),    POINTER ::  S    => NULL()  ! The state
    INTEGER                 ::  SIDE =  -1      ! Is this the left or right side of a branch?
    TYPE(PTR_LIST), POINTER ::  NEXT => NULL()  ! Next state in the list
    INTEGER                 ::  REFS =  0       ! Number of references to this list item
  END TYPE PTR_LIST

  ! NFA fragment
  TYPE  :: FRAG
    TYPE(STATE),    POINTER ::  START => NULL() ! Starting state of the fragment
    TYPE(PTR_LIST), POINTER ::  OUT1  => NULL() ! List of all output states from the fragment
  END TYPE FRAG

  ! Fragment stack node
  TYPE  :: FRAG_STACK
    TYPE(FRAG), POINTER ::  ELEM
  END TYPE FRAG_STACK

  INTEGER ::  SUBMATCH_PARS(2, PF_STACK_SIZE, MAX_PAREN_DEPTH)

CONTAINS

  !------------------------------------------------------------------------------!
    SUBROUTINE ABORT(ERROR, REGEX, LOCATION)                                     !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Throw an error and abort the program                                       !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   character(len=*), intent(in) :: error                                      !
  !     String to tell the user what's happened                                  !
  !   character(len=*), intent(in), optional :: regex                            !
  !     The regex that has failed                                                !
  !   integer,          intent(in), optional :: location                         !
  !     Where in the regex the failure occured                                   !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2019-08-18                                                 !
  !------------------------------------------------------------------------------!
    USE ISO_FORTRAN_ENV, ONLY: ERROR_UNIT
     CHARACTER(LEN=*), INTENT(IN)           :: ERROR
     CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: REGEX
     INTEGER,          INTENT(IN), OPTIONAL :: LOCATION

     INTEGER :: I

     WRITE(ERROR_UNIT, '(2a)') "ERROR: ", ERROR

     IF (PRESENT(REGEX)) THEN
       WRITE(ERROR_UNIT, '(a)') "Problem occured in regular expression:"
       WRITE(ERROR_UNIT, '(a)') '  /' // REGEX //'/'

       IF (PRESENT(LOCATION)) THEN
         DO I=1, LOCATION+2
           WRITE(ERROR_UNIT, '(a)', ADVANCE="no") " "
         END DO
         WRITE(ERROR_UNIT, '(a)') "^ Here"
       END IF
     END IF

     ERROR STOP

   END SUBROUTINE ABORT

  !------------------------------------------------------------------------------!
    SUBROUTINE WARN(ERROR, REGEX, LOCATION)                                      !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Throw a warning but don't abort the program                                !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   character(len=*), intent(in) :: error                                      !
  !     String to tell the user what's happened                                  !
  !   character(len=*), intent(in), optional :: regex                            !
  !     The regex that has failed                                                !
  !   integer                     , optional :: location                         !
  !     Where in the regex the failure occured                                   !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2019-08-18                                                 !
  !------------------------------------------------------------------------------!
    USE ISO_FORTRAN_ENV, ONLY: ERROR_UNIT
     CHARACTER(LEN=*), INTENT(IN)           :: ERROR
     CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: REGEX
     INTEGER,          INTENT(IN), OPTIONAL :: LOCATION

     INTEGER :: I

     WRITE(ERROR_UNIT, '(2a)') "WARNING: ", ERROR

     IF (PRESENT(REGEX)) THEN
       WRITE(ERROR_UNIT, '(a)') "Problem occured in regular expression:"
       WRITE(ERROR_UNIT, '(a)') '  /' // REGEX //'/'

       IF (PRESENT(LOCATION)) THEN
         DO I=1, LOCATION+2
           WRITE(ERROR_UNIT, '(a)', ADVANCE="no") " "
         END DO
         WRITE(ERROR_UNIT, '(a)') "^ Here"
       END IF
     END IF

   END SUBROUTINE WARN


  !------------------------------------------------------------------------------!
    FUNCTION TOKEN(CH)                                                           !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Convert an integer char code to a printable token
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   integer, intent(in) :: pf(:)                                               !
  !     Postfix expression stored as an array of integers                        !
  !------------------------------------------------------------------------------!
  ! RETURNS                                                                      !
  !   char(len=5) :: token                                                       !
  !     The printable token                                                      !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2019-07-19                                                 !
  !------------------------------------------------------------------------------!
    CHARACTER(LEN=5)      :: TOKEN
    INTEGER,  INTENT(IN)  ::  CH

      SELECT CASE(CH)
        CASE(NULL_ST)
          TOKEN = "     "
        CASE(1:255)
          TOKEN = ACHAR(CH) // "   "
        CASE(OPEN_PAR_CH)
          TOKEN = "OP ( "
        CASE(CLOSE_PAR_CH)
          TOKEN = "CL ) "
        CASE(CAT_OP)
          TOKEN = "CAT  "
        CASE(PLUS_OP)
          TOKEN = "PLUS "
        CASE(OR_OP)
          TOKEN = "OR   "
        CASE(QUEST_OP)
          TOKEN = "QUE  "
        CASE(STAR_OP)
          TOKEN = "STAR "

        CASE(SPLIT_ST)
          TOKEN = "SPLIT"
        CASE(MATCH_ST)
          TOKEN = "MATCH"
        CASE(ANY_CH)
          TOKEN = ".    "
        CASE(START_CH)
          TOKEN = "START"
        CASE(FINISH_CH)
          TOKEN = "FIN  "
        CASE(ALPHA_CH)
          TOKEN = "\a   "
        CASE(NUMERIC_CH)
          TOKEN = "\d   "
        CASE(WORD_CH)
          TOKEN = "\w   "
        CASE(SPACE_CH)
          TOKEN = "\s   "
        CASE(N_ALPHA_CH)
          TOKEN = "\A   "
        CASE(N_NUMERIC_CH)
          TOKEN = "\D   "
        CASE(N_WORD_CH)
          TOKEN = "\W   "
        CASE(N_SPACE_CH)
          TOKEN = "\S   "
        CASE DEFAULT
          CALL ABORT("Unrecognised character" //  CHAR(CH))
      END SELECT

    END FUNCTION TOKEN

  !------------------------------------------------------------------------------!
    SUBROUTINE PRINT_PF(PF)                                                      !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to print out a postfix expression in a human readable manner       !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   integer, intent(in) :: pf(:)                                               !
  !     Postfix expression stored as an array of integers                        !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    INTEGER,  INTENT(IN)  ::  PF(:)

    INTEGER ::  I

    PRINT_LOOP: DO I = 1, SIZE(PF)
      IF (PF(I) == NULL_ST) EXIT PRINT_LOOP
      WRITE(*,'(A7,A5)') TOKEN(PF(I))
    END DO PRINT_LOOP

  END SUBROUTINE PRINT_PF

  !------------------------------------------------------------------------------!
    RECURSIVE SUBROUTINE PRINT_STATE(S, DEPTH)                                   !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to print out an NFA state in a human readable manner. It is        !
  !   recursively called on all outputs of the state until nfa_max_print is      !
  !   reached.                                                                   !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   type(state), pointer, intent(in) :: s                                      !
  !     State to be printed                                                      !
  !                                                                              !
  !   integer, optional ,   intent(in) :: depth = 0                              !
  !     Depth of the state into the NFA                                          !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    TYPE(STATE), POINTER, INTENT(IN) ::  S
    INTEGER,  OPTIONAL,   INTENT(IN) :: DEPTH

    INTEGER ::  LOCAL_DEPTH, I
    TYPE(STATE), POINTER  ::  TMP_S

    LOCAL_DEPTH=0
    IF (PRESENT(DEPTH)) THEN
      LOCAL_DEPTH = DEPTH
    END IF

    ! Limit depth of print, mostly to avoid infinite loops
    IF (LOCAL_DEPTH > NFA_MAX_PRINT) THEN
      PRINT *, "Trying to print a superdeep structure!"
    ELSE
      TMP_S => S
      IF (TMP_S%C /= NULL_ST) THEN
        ! Make sure the state is properly indeneted
        DO I = 1, LOCAL_DEPTH
          WRITE(*,'(A3)', ADVANCE="no") "|  "
        END DO
        WRITE(*,'(A7,A5)') TOKEN(TMP_S%C)
      END IF

      ! if the state has any output states, print them too
      IF (ASSOCIATED(TMP_S%OUT1)) CALL PRINT_STATE(TMP_S%OUT1, DEPTH=LOCAL_DEPTH+1)
      IF (ASSOCIATED(TMP_S%OUT2)) CALL PRINT_STATE(TMP_S%OUT2, DEPTH=LOCAL_DEPTH+1)
    END IF

  END SUBROUTINE PRINT_STATE

  !------------------------------------------------------------------------------!
    FUNCTION NEW_LIST(OUTP, SIDE)                                                !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to create a new state list, with outp as the first state.          !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   type(state),    pointer,  intent(in)  ::  outp                             !
  !     First NFA state in the list                                              !
  !                                                                              !
  !   integer,                  intent(in)  ::  side                             !
  !     Which side of the the state goes on                                      !
  !------------------------------------------------------------------------------!
  ! RETURNS                                                                      !
  !   type(ptr_list), pointer                                                    !
  !     Pointer to the newly created list                                        !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    TYPE(PTR_LIST), POINTER :: NEW_LIST
    TYPE(STATE),    POINTER,  INTENT(IN)  ::  OUTP
    INTEGER,                  INTENT(IN)  ::  SIDE

    INTEGER ::  IERR

    NEW_LIST => NULL()

    ALLOCATE(NEW_LIST, STAT=IERR)
    IF (IERR /= 0) CALL ABORT("Unable to allocate new_list")

    NEW_LIST%S    => OUTP
    NEW_LIST%SIDE =  SIDE
    NEW_LIST%NEXT => NULL()
    NEW_LIST%REFS =  0

  END FUNCTION NEW_LIST

  !------------------------------------------------------------------------------!
    RECURSIVE SUBROUTINE NULLIFY_LIST(L)                                         !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   A routine to nullify a ptr_list. If the list is left unreferenced,         !
  !   also deallocate it.                                                        !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   type(ptr_list),    pointer,  intent(in)  ::  l                             !
  !     The list to be nullified                                                 !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2017-12-04                                                 !
  !------------------------------------------------------------------------------!
    TYPE(PTR_LIST), POINTER, INTENT(INOUT) :: L

    IF (ASSOCIATED(L)) THEN
      L%REFS=L%REFS-1

      IF(L%REFS == 0) THEN
        IF(ASSOCIATED(L%NEXT)) CALL NULLIFY_LIST(L%NEXT)
        DEALLOCATE(L)
      END IF

      L => NULL()
    END IF

  END SUBROUTINE NULLIFY_LIST

  !------------------------------------------------------------------------------!
    SUBROUTINE POINT_LIST(L1, L2)                                                !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   A routine to point one ptr_list at another (l1 => l2), whilst also keeping !
  !   track of how many references each list has pointing to it.                 !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   type(ptr_list),    pointer,  intent(inout)  :: l1                          !
  !     The list to be nullified                                                 !
  !   type(ptr_list),    pointer,  intent(in)     :: l2                          !
  !     The list to be nullified                                                 !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2017-12-04                                                 !
  !------------------------------------------------------------------------------!
    TYPE(PTR_LIST), POINTER, INTENT(INOUT)  :: L1
    TYPE(PTR_LIST), POINTER, INTENT(IN)     :: L2

    IF(ASSOCIATED(L1)) CALL NULLIFY_LIST(L1)

    IF(ASSOCIATED(L2)) THEN
      L1 => L2
      L1%REFS = L1%REFS + 1
    ENDIF

  END SUBROUTINE POINT_LIST

  !------------------------------------------------------------------------------!
    SUBROUTINE APPEND(L1, L2)                                                    !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to append ptr_list l2 to the end of ptr_list l1.                   !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   type(ptr_list), pointer,  intent(inout) :: l1                              !
  !     list to be appended to                                                   !
  !                                                                              !
  !   type(ptr_list), pointer,  intent(in)    :: l2                              !
  !     list to be appended                                                      !
  !------------------------------------------------------------------------------!
  ! RETURNS                                                                      !
  !   type(ptr_list), pointer                                                    !
  !     resultant list                                                           !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    TYPE(PTR_LIST), POINTER,  INTENT(INOUT) :: L1
    TYPE(PTR_LIST), POINTER,  INTENT(IN)    :: L2

    TYPE(PTR_LIST), POINTER :: TMP_L

    TMP_L => NULL()

    CALL POINT_LIST(TMP_L, L1)
    DO WHILE ( ASSOCIATED(TMP_L%NEXT) )
      CALL POINT_LIST(TMP_L, TMP_L%NEXT)
    END DO

    CALL POINT_LIST(TMP_L%NEXT, L2)

    CALL NULLIFY_LIST(TMP_L)

  END SUBROUTINE APPEND

  !------------------------------------------------------------------------------!
    SUBROUTINE DEALLOCATE_LIST(L, KEEP_STATES, N_STATES)                         !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to deallocate a ptr_list and, optionally, the NFA states in it.    !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   type(ptr_list), pointer,  intent(inout) ::  l                              !
  !     Pointer list to be deallocated                                           !
  !                                                                              !
  !   logical,        optional, intent(in)    ::  keep_states = false            !
  !     Whether or not the states within the list should be deallocated as well  !
  !                                                                              !
  !   integer,        optional, intent(inout) ::  n_states = 0                   !
  !     Number of allocated states in the list                                   !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    TYPE(PTR_LIST), POINTER,  INTENT(INOUT) ::  L
    LOGICAL,        OPTIONAL, INTENT(IN)    ::  KEEP_STATES
    INTEGER,        OPTIONAL, INTENT(INOUT) ::  N_STATES

    TYPE(PTR_LIST), POINTER ::  TMP_L
    LOGICAL ::  LOCAL_KS
    INTEGER ::  IERR

    TMP_L => NULL()
    LOCAL_KS = .FALSE.
    IF (PRESENT(KEEP_STATES)) LOCAL_KS = KEEP_STATES

    IF (.NOT. ASSOCIATED(L)) RETURN

    DO WHILE (ASSOCIATED(L%NEXT))
      CALL POINT_LIST(TMP_L, L)
      CALL POINT_LIST(L, TMP_L%NEXT)
      IF ((ASSOCIATED(TMP_L%S)) .AND. (.NOT. LOCAL_KS)) THEN
        DEALLOCATE(TMP_L%S)
        IF (PRESENT(N_STATES)) N_STATES = N_STATES - 1
      ELSE
        TMP_L%S => NULL()
      END IF
      CALL NULLIFY_LIST(TMP_L)
    END DO

    IF ((ASSOCIATED(L%S)) .AND. (.NOT. LOCAL_KS)) THEN
      DEALLOCATE(L%S, STAT=IERR)
      IF (IERR /= 0) CALL WARN("Unable to deallocate l%s")
      IF (PRESENT(N_STATES)) N_STATES = N_STATES - 1
    ELSE
      L%S => NULL()
    END IF

    CALL NULLIFY_LIST(L)

  END SUBROUTINE DEALLOCATE_LIST

  !------------------------------------------------------------------------------!
    SUBROUTINE PATCH(L, S)                                                       !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to append state s to every dangling output in ptr_list l.          !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   type(ptr_list), pointer, intent(inout)  ::  l                              !
  !     List to be patched                                                       !
  !                                                                              !
  !   type(state),    pointer, intent(in)     ::  s                              !
  !     state with which to patch the list                                       !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    TYPE(PTR_LIST), POINTER, INTENT(INOUT)  ::  L
    TYPE(STATE),    POINTER, INTENT(IN)     ::  S

    TYPE(PTR_LIST), POINTER :: TMP_L

    TMP_L => NULL()

    CALL POINT_LIST(TMP_L, L)
    DO WHILE ( ASSOCIATED(TMP_L) )
      SELECT CASE(TMP_L%SIDE)
        CASE(1)
          TMP_L%S%OUT1 => S
        CASE(2)
          TMP_L%S%OUT2 => S
        CASE DEFAULT
          CALL ABORT("Unexpected value of side")
      END SELECT
      CALL POINT_LIST(TMP_L, TMP_L%NEXT)
    END DO

    CALL NULLIFY_LIST(TMP_L)

  END SUBROUTINE PATCH

  !------------------------------------------------------------------------------!
    FUNCTION RE_TO_PF(RE) RESULT(PF)                                             !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to convert a regular expression string to a a postfix expression,  !
  !   stored in an array of integers.                                            !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   character(len=*),   intent(in) :: re
  !     Regular expression to be converted to postfix                            !
  !------------------------------------------------------------------------------!
  ! RETURNS                                                                      !
  !   integer ::  pf(pf_buff_size)                                               !
  !     Postfix expression, stored as an array of integers                       !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    INTEGER ::  PF(PF_BUFF_SIZE)
    CHARACTER(LEN=*),   INTENT(IN) :: RE

    INTEGER          :: N_ALT                   ! Number of alternatives
    INTEGER          :: N_ATOM                  ! Number of single units
    INTEGER          :: RE_LOC                  ! Location in the regex string
    INTEGER          :: PF_LOC                  ! Location in the postfix list
    TYPE(PAREN_LIST) :: PAREN(MAX_PAREN_DEPTH)  ! List of opened parens at a given point
    INTEGER          :: PAR_LOC                 ! Current position in the paren list
    LOGICAL          :: ESCAPED                 ! Whether or not the current character is escaped
    INTEGER          :: ESCAPED_CHR             ! The charcter which has been escaped

    ! Initialise key variables
    PAR_LOC = 1
    RE_LOC  = 1
    PF_LOC  = 1
    N_ALT   = 0
    N_ATOM  = 0
    ESCAPED = .FALSE.

    PF = NULL_ST

    ! If the regex won't fit in the pf list, abort
    IF (LEN_TRIM(RE) > PF_BUFF_SIZE/2) CALL ABORT("Regex too long", TRIM(RE))

    ! Loop over characters in the regex
    DO WHILE (RE_LOC <= LEN_TRIM(RE))
      IF (.NOT. ESCAPED) THEN

        ! What is the current character?
        SELECT CASE(RE(RE_LOC:RE_LOC))

          CASE('\') ! The next character will be escaped
            ESCAPED = .TRUE.

          CASE('(') ! We've found an open bracket
            IF (PAR_LOC > SIZE(PAREN)) CALL ABORT("Too many embedded brackets!", RE, RE_LOC)

            ! Concatinate this set of brackets with anything previous and add it to the postfix list
            IF (N_ATOM > 1) CALL PUSH_ATOM(CAT_OP)
            ! Push an open_paren to track brackets in the automaton
            CALL PUSH_ATOM(OPEN_PAR_CH)

            ! Store the state outside of the brackes and reset the counters
            PAREN(PAR_LOC)%N_ALT  = N_ALT
            PAREN(PAR_LOC)%N_ATOM = N_ATOM
            PAR_LOC = PAR_LOC + 1
            N_ALT   = 0
            N_ATOM  = 0

          CASE('|') ! We've found an OR operation
            IF (N_ATOM == 0) CALL ABORT("OR has no left hand side", RE, RE_LOC)

            ! Add all the current atoms to the postfix list and start a new alternate list
            N_ATOM = N_ATOM - 1
            DO WHILE (N_ATOM > 0)
              CALL PUSH_ATOM(CAT_OP)
            END DO
            N_ALT = N_ALT + 1

          CASE (')') ! We've found a close bracket
            IF (PAR_LOC == 1) CALL ABORT("Unmatched ')'", RE, RE_LOC)
            IF (N_ATOM == 0)  CALL ABORT("Empty parentheses", RE, RE_LOC)

            ! Add all the current atoms to the postfix list
            N_ATOM = N_ATOM - 1
            DO WHILE (N_ATOM > 0)
              CALL PUSH_ATOM(CAT_OP)
            END DO

            ! Close off any alternatives that exist in the bracktes
            DO WHILE (N_ALT > 0)
              CALL PUSH_ATOM(OR_OP)
            END DO

            ! Revert state to that of the outer brackets
            PAR_LOC = PAR_LOC - 1
            N_ALT = PAREN(PAR_LOC)%N_ALT
            N_ATOM = PAREN(PAR_LOC)%N_ATOM
            N_ATOM = N_ATOM + 1

            ! Push a close_paren to track brackets in the automaton
            CALL PUSH_ATOM(CLOSE_PAR_CH)

          CASE('*') ! We've found a STAR operation
            IF (N_ATOM == 0) CALL ABORT("Nothing to *", RE, RE_LOC)
            CALL PUSH_ATOM(STAR_OP)

          CASE('+') ! We've found a PLUS operation
            IF (N_ATOM == 0) CALL ABORT("Nothing to +", RE, RE_LOC)
            CALL PUSH_ATOM(PLUS_OP)

          CASE('?') ! We've found a QUESTION operation
            IF (N_ATOM == 0) CALL ABORT("Nothing to ?", RE, RE_LOC)
            CALL PUSH_ATOM(QUEST_OP)

          CASE ('.') ! We've found and ANY character
            IF (N_ATOM > 1) CALL PUSH_ATOM(CAT_OP)
            CALL PUSH_ATOM(ANY_CH)

          CASE ('^') ! We've found a line-start anchor
            IF (N_ATOM > 1) CALL PUSH_ATOM(CAT_OP)
            CALL PUSH_ATOM(START_CH)

          CASE ('$') ! We've foudn a line-end anchor
            IF (N_ATOM > 1) CALL PUSH_ATOM(CAT_OP)
            CALL PUSH_ATOM(FINISH_CH)

          CASE(' ', ACHAR(9)) ! We've found whitespace in the regex
            ! Do nothing, ignore whitespace in the regex

          CASE DEFAULT ! We've found a regular charcter
            ! If there are already atoms, add a concat. operation and then add this character
            IF (N_ATOM > 1) CALL PUSH_ATOM(CAT_OP)
            CALL PUSH_ATOM(IACHAR(RE(RE_LOC:RE_LOC)))

        END SELECT
      ELSE IF (ESCAPED) THEN

        ! Deal with escaped characters
        SELECT CASE(RE(RE_LOC:RE_LOC))
          CASE('(','|',')','*','+','?','\','.','^','$',' ',ACHAR(9))
            ESCAPED_CHR = IACHAR(RE(RE_LOC:RE_LOC))
          CASE('a')
            ESCAPED_CHR = ALPHA_CH
          CASE('d')
            ESCAPED_CHR = NUMERIC_CH
          CASE('w')
            ESCAPED_CHR = WORD_CH
          CASE('s')
            ESCAPED_CHR = SPACE_CH
          CASE('A')
            ESCAPED_CHR = N_ALPHA_CH
          CASE('D')
            ESCAPED_CHR = N_NUMERIC_CH
          CASE('W')
            ESCAPED_CHR = N_WORD_CH
          CASE('S')
            ESCAPED_CHR = N_SPACE_CH

          CASE DEFAULT
            CALL ABORT("Unrecognised escape character \" // RE(RE_LOC:RE_LOC), RE, RE_LOC)
        END SELECT

        ! If there are already atoms, add a concat. operation and then add this character
        IF (N_ATOM > 1) CALL PUSH_ATOM(CAT_OP)
        CALL PUSH_ATOM(ESCAPED_CHR)
        ESCAPED = .FALSE.
      END IF

      ! Go to the next character in the regex
      RE_LOC = RE_LOC + 1
    END DO

    IF (PAR_LOC /= 1) CALL ABORT("I think you've got unmatched parentheses", RE, RE_LOC)

    ! Add any remaining atoms to the postfix list
    N_ATOM = N_ATOM - 1
    DO WHILE (N_ATOM > 0)
      CALL PUSH_ATOM(CAT_OP)
    END DO

    ! Add any remaining alternatives to the postfix list
    DO WHILE (N_ALT > 0)
      CALL PUSH_ATOM(OR_OP)
    END DO

  CONTAINS

    ! A routine to push a given atom to the end of the postfix list
    SUBROUTINE PUSH_ATOM(ATOM)
      INTEGER, INTENT(IN) :: ATOM
      INTEGER :: TMP_PF_LOC

      PF(PF_LOC) = ATOM
      PF_LOC = PF_LOC + 1

      SELECT CASE(ATOM)
        CASE (CAT_OP)
          N_ATOM = N_ATOM - 1

        CASE (OR_OP)
          N_ALT = N_ALT - 1

        ! If this character operates on a previous atom, we need to make sure
        ! that it applied to that atom and not any close_par.s instead
        CASE (QUEST_OP, PLUS_OP, STAR_OP)
          TMP_PF_LOC = PF_LOC-1
          DO WHILE (PF(TMP_PF_LOC-1) == CLOSE_PAR_CH)
            PF(TMP_PF_LOC) = CLOSE_PAR_CH
            PF(TMP_PF_LOC-1) = ATOM
            TMP_PF_LOC = TMP_PF_LOC - 1
          END DO

        CASE DEFAULT
        N_ATOM = N_ATOM + 1

      END SELECT

    END SUBROUTINE PUSH_ATOM

  END FUNCTION RE_TO_PF

  !------------------------------------------------------------------------------!
    SUBROUTINE ALLOCATE_NFA(NFA)                                                 !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to allocate and initialise the constituent parts of the nfa type.  !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   type(nfa), intent(inout) :: nfa                                            !
  !     Finite automaton to be allocated                                         !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-12-29                                                 !
  !------------------------------------------------------------------------------!
    TYPE(NFA_TYPE), INTENT(INOUT) :: NFA

    NFA%HEAD => NULL()
    NFA%STATES => NULL()
    CALL POINT_LIST(NFA%STATES, NEW_LIST(NULL(), 0))
    NFA%N_STATES = 0

  END SUBROUTINE ALLOCATE_NFA

  !------------------------------------------------------------------------------!
    SUBROUTINE DEALLOCATE_NFA(NFA)                                               !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to deallocate the constituent parts of the nfa type.               !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   type(nfa), intent(inout) :: nfa                                            !
  !     Finite automaton to be allocated                                         !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-12-29                                                 !
  !------------------------------------------------------------------------------!
    TYPE(NFA_TYPE), INTENT(INOUT) :: NFA

    CALL DEALLOCATE_LIST(NFA%STATES, KEEP_STATES=.FALSE., N_STATES = NFA%N_STATES)
    NFA%HEAD => NULL()
    IF (NFA%N_STATES /= 0) CALL WARN("Some states are still allocated!")

  END SUBROUTINE DEALLOCATE_NFA

  !------------------------------------------------------------------------------!
    FUNCTION PF_TO_NFA(POSTFIX) RESULT(NFA)                                      !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to convert a postfix expression to a Nondeterministic Finite       !
  !   Automaton, with the head stored in state 'states'                          !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   integer, intent(in) :: postfix(pf_buff_size)                               !
  !     Postfix expression stored as an array of integers                        !
  !------------------------------------------------------------------------------!
  ! RETURNS                                                                      !
  !   type(nfa)                                                                  !
  !     Resultant NFA                                                            !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    TYPE(NFA_TYPE)  ::  NFA
    INTEGER,  INTENT(IN)  ::  POSTFIX(PF_BUFF_SIZE)

    INTEGER ::  PF_LOC, S_LOC
    TYPE(FRAG_STACK), ALLOCATABLE ::  STACK(:)            ! Stack of unassigned NFA fragments
    TYPE(FRAG_STACK), ALLOCATABLE ::  ALLOCATED_FRAGS(:)  ! Stack of all allocated NFA fragments
    TYPE(FRAG),     POINTER ::  STACK_P                   ! Pointer to the top of the stack
    TYPE(FRAG),     POINTER ::  E1, E2, E                 ! Pointers to fragments being operated on
    TYPE(STATE),    POINTER ::  S                         ! Pointer to a state
    TYPE(STATE),    POINTER ::  MATCHSTATE                ! The "Successful match" state
    TYPE(STATE),    POINTER ::  NULLSTATE                 ! The "Nothing to be pointed to" state

    INTEGER ::  NFRAGS, I, IERR

    CALL ALLOCATE_NFA(NFA)

    ! Allocate and initalise the appropriate datastructures
    NFRAGS = 0
    ALLOCATE(ALLOCATED_FRAGS(PF_STACK_SIZE), STAT=IERR)
    IF (IERR /= 0) CALL ABORT("Unable to allocate frag stack")
    ALLOCATE(STACK(PF_STACK_SIZE),STAT=IERR)
    IF (IERR /= 0) CALL ABORT("Unable to allocate stack")

    DO I = 1, PF_STACK_SIZE
      STACK(I)%ELEM => NULL()
      ALLOCATED_FRAGS(I)%ELEM => NULL()
    END DO

    IF (NFA%STATES%SIDE /= 0) CALL ABORT("Trying to build nfa with in-use states")

    ! Allocate the Match and Null states for this NFA
    MATCHSTATE => NEW_STATE(MATCH_ST, NULL(), NULL())
    NULLSTATE => NEW_STATE(NULL_ST, NULL(), NULL())

    STACK_P => STACK(1)%ELEM
    PF_LOC  = 1
    S_LOC   = 1

    ! While there are still states in the postfix list:
    DO WHILE (POSTFIX(PF_LOC) /= NULL_ST)
      S => NULL()
      SELECT CASE( POSTFIX(PF_LOC) )

      CASE(CAT_OP) ! A concatanation operation
        ! Pop the top two fragments off the stack (e1 and e2)
        E2 => POP()
        E1 => POP()

        ! Patch the ends of e1 to the start of e2
        CALL PATCH(E1%OUT1, E2%START)

        ! Push the resultant fragment, starting at e1, back onto the stack
        CALL PUSH(NEW_FRAG(E1%START, E2%OUT1))
        E1 => NULL()
        E2 => NULL()

      CASE(OR_OP) ! A '|' operation
        ! Pop the top two fragments off the stack (e1 and e2)
        E2 => POP()
        E1 => POP()

        ! Create a split state pointing to the start of e1 and e2
        S => NEW_STATE( SPLIT_ST, E1%START, E2%START )

        ! Append the outputs of e2 to the output list of e1
        CALL APPEND(E1%OUT1, E2%OUT1)

        ! Push a new fragment, starting at s, back onto the stack
        CALL PUSH( NEW_FRAG(S, E1%OUT1) )
        E1 => NULL()
        E2 => NULL()

      CASE(QUEST_OP) ! A '?' operation
        ! Pop the top fragment off the stack (e) and create a split state (s)
        E => POP()
        S => NEW_STATE( SPLIT_ST, E%START, NULLSTATE )

        ! Create a new state list contining s and append the outputs to the output list of e
        CALL APPEND(E%OUT1, NEW_LIST(S,2))

        ! Push a new fragment, starting at s, onto the stack
        CALL PUSH( NEW_FRAG(S, E%OUT1) )
        E => NULL()

      CASE(STAR_OP) ! A '*' operation
        ! Pop the top fragment off the stack (e) and create a split state (s)
        E => POP()
        S => NEW_STATE( SPLIT_ST, E%START, NULLSTATE )

        ! Patch the ends of s to the start of e
        CALL PATCH(E%OUT1, S)

        ! Push a new fragment, starting at s, onto the stack
        CALL PUSH( NEW_FRAG(S, NEW_LIST(S, 2))  )
        E => NULL()

      CASE(PLUS_OP) ! A '+' operation
        ! Pop the top fragment off the stack (e) and create a split state (s)
        E => POP()
        S => NEW_STATE( SPLIT_ST, E%START, NULLSTATE )

        ! Patch the ends of s to the start of e
        CALL PATCH(E%OUT1, S)

        ! Push a new fragment, starting at e, onto the stack
        CALL PUSH( NEW_FRAG(E%START, NEW_LIST(S, 2))  )
        E => NULL()

      CASE DEFAULT ! Everything else
        ! Create a new state for this particular character
        S => NEW_STATE( POSTFIX(PF_LOC), NULLSTATE, NULLSTATE )

        ! Push a new fragment, starting at e, onto the stack
        CALL PUSH( NEW_FRAG(S, NEW_LIST(S, 1)) )
        E => NULL()

      END SELECT

      ! Advance to the next postfix token
      PF_LOC = PF_LOC + 1
    END DO

    ! Pop off (hopefully) the final element on the stack
    E => POP()
    IF (S_LOC /= 1) CALL WARN("Stack is not empty on exit")
    CALL PATCH(E%OUT1, MATCHSTATE)

    NFA%HEAD => E%START

    ! If we've messed up, matchstate or nullstate might have changed; check for this
    IF (MATCHSTATE%C /= MATCH_ST) CALL WARN("***** Matchstate has changed!")
    IF (NULLSTATE%C /= NULL_ST) CALL WARN("***** Nullstate has changed!")

    ! Deallocate the memory we used along the way
    DO I = 1, NFRAGS
      IF (ASSOCIATED(ALLOCATED_FRAGS(I)%ELEM)) THEN
        CALL DEALLOCATE_LIST(ALLOCATED_FRAGS(I)%ELEM%OUT1, KEEP_STATES=.TRUE.)
        IF (ASSOCIATED(ALLOCATED_FRAGS(I)%ELEM%START)) ALLOCATED_FRAGS(I)%ELEM%START => NULL()
        DEALLOCATE(ALLOCATED_FRAGS(I)%ELEM, STAT=IERR)
        IF (IERR /= 0) CALL WARN("Unable to deallocate fragment")
        ALLOCATED_FRAGS(I)%ELEM => NULL()
      END IF
    END DO

    DEALLOCATE(STACK, ALLOCATED_FRAGS, STAT=IERR)
    IF (IERR /= 0) CALL WARN("Unable to deallocate stacks")
    E => NULL()

  CONTAINS

    !------------------------------------------------------------------------------!
      FUNCTION NEW_FRAG(S, L)                                                      !
    !------------------------------------------------------------------------------!
    ! DESCRPTION                                                                   !
    !   Routine to create a new NFA fragment.                                      !
    !------------------------------------------------------------------------------!
      TYPE(FRAG), POINTER ::  NEW_FRAG
      TYPE(STATE),    POINTER,  INTENT(IN)  ::  S
      TYPE(PTR_LIST), POINTER,  INTENT(IN)  ::  L

      ALLOCATE(NEW_FRAG)
      NEW_FRAG%START => S
      CALL POINT_LIST(NEW_FRAG%OUT1, L)

      NFRAGS = NFRAGS + 1
      ALLOCATED_FRAGS(NFRAGS)%ELEM => NEW_FRAG

    END FUNCTION NEW_FRAG

    !------------------------------------------------------------------------------!
      FUNCTION NEW_STATE(C, OUT1, OUT2)                                            !
    !------------------------------------------------------------------------------!
    ! DESCRPTION                                                                   !
    !   Routine to create a new NFA state, outputting to out1 and out2.            !
    !------------------------------------------------------------------------------!
      TYPE(STATE), POINTER  ::  NEW_STATE
      INTEGER,                INTENT(IN)  ::  C
      TYPE(STATE),  POINTER,  INTENT(IN)  ::  OUT1, OUT2

      INTEGER ::  IERR

      NEW_STATE => NULL()
      ALLOCATE(NEW_STATE, STAT=IERR)
      IF (IERR /= 0) CALL ABORT("Unable to allocate new_state")
      NEW_STATE%LAST_LIST = 0
      NEW_STATE%C = C
      NEW_STATE%OUT1 => OUT1
      NEW_STATE%OUT2 => OUT2

      CALL APPEND(NFA%STATES, NEW_LIST(NEW_STATE, -1))
      NFA%N_STATES = NFA%N_STATES + 1

    END FUNCTION NEW_STATE

    !------------------------------------------------------------------------------!
      SUBROUTINE PUSH(F)                                                           !
    !------------------------------------------------------------------------------!
    ! DESCRPTION                                                                   !
    !   Routine to push NFA fragment onto the stack.                               !
    !------------------------------------------------------------------------------!
      TYPE(FRAG), INTENT(IN), POINTER  ::  F

      S_LOC = S_LOC + 1
      STACK(S_LOC)%ELEM => F

    END SUBROUTINE PUSH

    !------------------------------------------------------------------------------!
      FUNCTION POP()                                                               !
    !------------------------------------------------------------------------------!
    ! DESCRPTION                                                                   !
    !   Routine to push an NFA off the stack, and returning it.                    !
    !------------------------------------------------------------------------------!
      TYPE(FRAG), POINTER :: POP

      POP => STACK(S_LOC)%ELEM
      S_LOC = S_LOC - 1

    END FUNCTION POP

  END FUNCTION PF_TO_NFA

  !------------------------------------------------------------------------------!
    FUNCTION RUN_NFA_FAST(NFA, STR, START, FINISH) RESULT(RES)                   !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to simulate the NFA 'nfa' o n the string 'str', starting 'start'   !
  !   characters in. This routine uses the fast algorithm. This algorithm        !
  !   doesn't allow submatching.                                                 !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   type(nfa_type),   intent(inout)             ::  nfa                        !
  !     NFA to be simulated                                                      !
  !                                                                              !
  !   character(len=*), intent(in)                ::  str                        !
  !     String to be searched                                                    !
  !                                                                              !
  !   integer,          intent(inout)             ::  start                      !
  !     Where in str to start. On exit, returns the start of the match if        !
  !     matched                                                                  !
  !                                                                              !
  !   integer,          intent(out),    optional  ::  finish                     !
  !     Last character of matched string                                         !
  !------------------------------------------------------------------------------!
  ! RETURNS                                                                      !
  !   TRUE if there is a match, FALSE otherwise.                                 !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    LOGICAL :: RES
    TYPE(NFA_TYPE),   INTENT(INOUT)             ::  NFA
    CHARACTER(LEN=*), INTENT(IN)                ::  STR
    INTEGER,          INTENT(INOUT)             ::  START
    INTEGER,          INTENT(OUT),    OPTIONAL  ::  FINISH

    TYPE  ::  LIST
      TYPE(STATE),  POINTER ::  S
    END TYPE LIST

    TYPE(LIST), ALLOCATABLE, TARGET ::  L1(:), L2(:)
    INTEGER                         ::  LIST_ID = 0
    INTEGER ::  LOC_START
    LOGICAL ::  NO_ADVANCE

    TYPE(LIST), POINTER ::  C_LIST(:), N_LIST(:), T(:)
    INTEGER ::  CH_LOC, N_CL, N_NL, N_T
    INTEGER ::  ISTART, I, IERR

    ALLOCATE(L1(1:NFA%N_STATES), L2(1:NFA%N_STATES), STAT=IERR)
    IF (IERR /= 0) CALL ABORT("Error allocating l1,l2 in run_nfa_fast")

    ! The match might not start on the first character of the string,
    !   test the NFA starting on each character until we find a match
    START_LOOP: DO ISTART = START, LEN(STR)

      ! Initialise the variables for a new run
      DO I = 1, NFA%N_STATES
        L1(I)%S => NULL()
        L2(I)%S => NULL()
      END DO

      N_CL = 1
      N_NL = 1

      C_LIST => START_LIST(L1, N_CL, NFA%HEAD)
      N_LIST => L2

      CH_LOC = ISTART
      LOC_START = ISTART

      RES = .FALSE.

      IF (PRESENT(FINISH)) FINISH = -1

      ! If the first character matches, we're done!
      IF ( IS_MATCH(C_LIST, N_CL) ) THEN
        RES = .TRUE.
        IF (PRESENT(FINISH)) FINISH = MIN(CH_LOC, LEN(STR))
      END IF

      ! Keep trying to match until we hit the end of the string
      DO WHILE (CH_LOC <= LEN(STR)+1)
        NO_ADVANCE  = .FALSE.

        ! Step each possible path through the NFA
        CALL STEP()

        ! Swap the current and next lists wround
        T      => C_LIST
        C_LIST => N_LIST
        N_LIST => T
        N_T  = N_CL
        N_CL = N_NL
        N_NL = N_T

        ! If any of the new current list match, make a note of that
        IF ( IS_MATCH(C_LIST, N_CL) ) THEN
          RES = .TRUE.
          IF (PRESENT(FINISH)) FINISH = MIN(CH_LOC, LEN(STR))
        END IF

        ! Possibly advance to the next character in the string (not if we're matching, e.g. '^')
        IF (.NOT. NO_ADVANCE) CH_LOC = CH_LOC + 1
      END DO

      ! Return if we've found a match
      IF (RES) EXIT START_LOOP
    END DO START_LOOP

    ! If we matched, store the start of the match
    IF (RES) START = LOC_START
    DEALLOCATE(L1, L2)

  CONTAINS

    !------------------------------------------------------------------------------!
      FUNCTION START_LIST(L, N_L, S)                                               !
    !------------------------------------------------------------------------------!
    ! DESCRPTION                                                                   !
    !   Routine to initialise a list of active states.                             !
    !------------------------------------------------------------------------------!
      TYPE(LIST), POINTER ::  START_LIST(:)
      TYPE(LIST),   TARGET,   INTENT(INOUT)  ::  L(:)
      INTEGER,                INTENT(INOUT)  ::  N_L
      TYPE(STATE),  POINTER,  INTENT(INOUT)  ::  S


      N_L = 1
      LIST_ID = LIST_ID + 1
      START_LIST => L

      CALL ADD_STATE(START_LIST, N_L, S)

    END FUNCTION START_LIST

    !------------------------------------------------------------------------------!
      SUBROUTINE STEP()                                                            !
    !------------------------------------------------------------------------------!
    ! DESCRPTION                                                                   !
    !   Routine to step through one node of the NFA for each state in the current  !
    !   list.                                                                      !
    !------------------------------------------------------------------------------!
      INTEGER ::  I
      TYPE(STATE),  POINTER ::  S => NULL()

      LIST_ID = LIST_ID + 1
      N_NL = 1

      DO I=1, N_CL-1
        S => C_LIST(I)%S

        IF (CH_LOC <= LEN(STR)) THEN
          SELECT CASE(S%C)

            CASE(0:255)
              IF ( S%C == IACHAR(STR(CH_LOC:CH_LOC)) ) THEN
                CALL ADD_STATE(N_LIST, N_NL, S%OUT1)
              END IF

            CASE(ANY_CH)
              CALL ADD_STATE(N_LIST, N_NL, S%OUT1)
            CASE(ALPHA_CH)
              SELECT CASE( STR(CH_LOC:CH_LOC) )
                CASE("a":"z","A":"Z")
                  CALL ADD_STATE(N_LIST, N_NL, S%OUT1)
              END SELECT
            CASE(NUMERIC_CH)
              SELECT CASE( STR(CH_LOC:CH_LOC) )
                CASE("0":"9")
                  CALL ADD_STATE(N_LIST, N_NL, S%OUT1)
              END SELECT
            CASE(WORD_CH)
              SELECT CASE( STR(CH_LOC:CH_LOC) )
                CASE("a":"z","A":"Z","0":"9","_")
                  CALL ADD_STATE(N_LIST, N_NL, S%OUT1)
              END SELECT
            CASE(SPACE_CH)
              SELECT CASE( STR(CH_LOC:CH_LOC) )
                CASE(" ", ACHAR(9), ACHAR(10))
                  CALL ADD_STATE(N_LIST, N_NL, S%OUT1)
              END SELECT

            CASE(N_ALPHA_CH)
              SELECT CASE( STR(CH_LOC:CH_LOC) )
                CASE("a":"z","A":"Z")
                CASE DEFAULT
                  CALL ADD_STATE(N_LIST, N_NL, S%OUT1)
              END SELECT
            CASE(N_NUMERIC_CH)
              SELECT CASE( STR(CH_LOC:CH_LOC) )
                CASE("0":"9")
                CASE DEFAULT
                  CALL ADD_STATE(N_LIST, N_NL, S%OUT1)
              END SELECT
            CASE(N_WORD_CH)
              SELECT CASE( STR(CH_LOC:CH_LOC) )
                CASE("a":"z","A":"Z","0:9","_")
                CASE DEFAULT
                  CALL ADD_STATE(N_LIST, N_NL, S%OUT1)
              END SELECT
            CASE(N_SPACE_CH)
              SELECT CASE( STR(CH_LOC:CH_LOC) )
                CASE(" ", ACHAR(9), ACHAR(10))
                CASE DEFAULT
                  CALL ADD_STATE(N_LIST, N_NL, S%OUT1)
              END SELECT

            CASE(START_CH)
              IF (CH_LOC == 1) CALL ADD_STATE(N_LIST, N_NL, S%OUT1)
              NO_ADVANCE = .TRUE.

            CASE(OPEN_PAR_CH)
              CALL ADD_STATE(N_LIST, N_NL, S%OUT1)
              NO_ADVANCE = .TRUE.

            CASE(CLOSE_PAR_CH)
              CALL ADD_STATE(N_LIST, N_NL, S%OUT1)
              NO_ADVANCE = .TRUE.

            CASE(FINISH_CH)

            CASE( MATCH_ST )

            CASE DEFAULT
              CALL ABORT("Unrecognised state " // ACHAR(S%C))
          END SELECT
        ELSE
          IF (S%C == FINISH_CH) THEN
            CALL ADD_STATE(N_LIST, N_NL, S%OUT1)
          END IF
        END IF
      END DO

    END SUBROUTINE STEP

    !------------------------------------------------------------------------------!
      FUNCTION IS_MATCH(L, N_L)                                                    !
    !------------------------------------------------------------------------------!
    ! DESCRPTION                                                                   !
    !   Routine to check if any nodes in the list l are match states in the NFA.   !
    !------------------------------------------------------------------------------!
      LOGICAL ::  IS_MATCH
      TYPE(LIST), POINTER,  INTENT(IN)  ::  L(:)
      INTEGER,              INTENT(IN)  ::  N_L

      INTEGER ::  I

      DO I = 1, N_L-1
        IF ( L(I)%S%C == MATCH_ST ) THEN
          IS_MATCH = .TRUE.
          RETURN
        END IF
      END DO
      IS_MATCH = .FALSE.

    END FUNCTION IS_MATCH

    !------------------------------------------------------------------------------!
      RECURSIVE SUBROUTINE ADD_STATE(L, N_L, S)                                    !
    !------------------------------------------------------------------------------!
    ! DESCRPTION                                                                   !
    !   Routine to add the state s to the end of list l. If s is a split_st, add   !
    !   its output instead.                                                        !
    !------------------------------------------------------------------------------!
      TYPE(LIST),   POINTER,  INTENT(INOUT) ::  L(:)
      INTEGER,                INTENT(INOUT) ::  N_L
      TYPE(STATE),  POINTER,  INTENT(INOUT) ::  S

      IF ( (S%C == NULL_ST) .OR. (S%LAST_LIST == LIST_ID) ) RETURN
      S%LAST_LIST = LIST_ID
      IF (S%C == SPLIT_ST) THEN
        CALL ADD_STATE(L, N_L, S%OUT1)
        CALL ADD_STATE(L, N_L, S%OUT2)
        RETURN
      END IF
      L(N_L)%S => S
      N_L = N_L + 1

    END SUBROUTINE ADD_STATE

  END FUNCTION RUN_NFA_FAST

  !------------------------------------------------------------------------------!
    RECURSIVE FUNCTION RUN_NFA_FULL(NFA, STR, START, FINISH, S_IN) RESULT(RES)   !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to simulate the NFA 'nfa' o n the string 'str', starting 'start'   !
  !   characters in. This routine uses the slower algorithm. This algorithm      !
  !   does allow submatching.                                                    !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   type(nfa_type),       intent(inout)           ::  nfa                      !
  !     NFA to be simulated                                                      !
  !                                                                              !
  !   character(len=*),     intent(in)              ::  str                      !
  !     String to be searched                                                    !
  !                                                                              !
  !   integer,              intent(inout)           ::  start                    !
  !     Where in str to start. On exit, returns the start of the match if        !
  !     matched                                                                  !
  !                                                                              !
  !   integer,              intent(out),  optional  ::  finish                   !
  !     Last character of matched string                                         !
  !                                                                              !
  !   type(state), pointer, intent(in),   optional  ::  s_in                     !
  !     Node to start on  is the start of the NFA or not.                        !
  !------------------------------------------------------------------------------!
  ! RETURNS                                                                      !
  !   TRUE if there is a match, FALSE otherwise.                                 !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    LOGICAL :: RES
    TYPE(NFA_TYPE),       INTENT(INOUT)           ::  NFA
    CHARACTER(LEN=*),     INTENT(IN)              ::  STR
    INTEGER,              INTENT(INOUT)           ::  START
    INTEGER,              INTENT(OUT),  OPTIONAL  ::  FINISH
    TYPE(STATE), POINTER, INTENT(IN),   OPTIONAL  ::  S_IN

    TYPE(STATE), POINTER :: S
    INTEGER ::  ISTART, FIN

    RES = .FALSE.
    IF (PRESENT(FINISH)) FINISH = -1
    FIN = -1


    IF (PRESENT(S_IN)) THEN
      ISTART = START
      S => S_IN
      IF (DEBUG) WRITE(*,*) "Checking " // TOKEN(S%C) // " against " // STR(START:START)
      CALL STEP()
    ELSE
      START_LOOP: DO ISTART = START, LEN(STR)
        S => NFA%HEAD
        IF (DEBUG) WRITE(*,*) "Checking " // TOKEN(S%C) // " against " // STR(START:START)
        CALL STEP()
        IF (RES) EXIT START_LOOP
      END DO START_LOOP
    END IF

    IF (PRESENT(FINISH)) THEN
      IF (FINISH == -1) FINISH = FIN
    END IF
    START = ISTART

    IF (DEBUG) WRITE(*,*) "res = ", RES, START, FINISH

  CONTAINS

    !------------------------------------------------------------------------------!
      RECURSIVE SUBROUTINE STEP()                                                  !
    !------------------------------------------------------------------------------!
    ! DESCRPTION                                                                   !
    !   Routine to step through the NFA. If it does not reach an end, run_nfa_full !
    !   is re-called.                                                              !
    !------------------------------------------------------------------------------!
      INTEGER ::  NEXT_START
      INTEGER :: LOCAL_ISTART

      LOCAL_ISTART = ISTART
      NEXT_START = -1
      IF (LOCAL_ISTART <= LEN(STR)) THEN
        SELECT CASE(S%C)
          CASE( MATCH_ST )
            RES = .TRUE.
            IF (PRESENT(FINISH)) FINISH = LOCAL_ISTART-1

          CASE( SPLIT_ST )
            RES = RUN_NFA_FULL(NFA, STR, LOCAL_ISTART, FIN, S_IN = S%OUT1)
            IF (.NOT. RES) RES = RUN_NFA_FULL(NFA, STR, LOCAL_ISTART, FIN, S_IN = S%OUT2)

          CASE(0:255)
            IF ( S%C == IACHAR(STR(LOCAL_ISTART:LOCAL_ISTART)) ) THEN
              NEXT_START = LOCAL_ISTART + 1
              RES = RUN_NFA_FULL(NFA, STR, NEXT_START, FIN, S_IN = S%OUT1)
            END IF

          CASE(ANY_CH)
            NEXT_START = LOCAL_ISTART + 1
            RES = RUN_NFA_FULL(NFA, STR, NEXT_START, FIN, S_IN = S%OUT1)
          CASE(ALPHA_CH)
            SELECT CASE( STR(LOCAL_ISTART:LOCAL_ISTART) )
              CASE("a":"z","A":"Z")
                NEXT_START = LOCAL_ISTART + 1
                RES = RUN_NFA_FULL(NFA, STR, NEXT_START, FIN, S_IN = S%OUT1)
            END SELECT
          CASE(NUMERIC_CH)
            SELECT CASE( STR(LOCAL_ISTART:LOCAL_ISTART) )
              CASE("0":"9")
                NEXT_START = LOCAL_ISTART + 1
                RES = RUN_NFA_FULL(NFA, STR, NEXT_START, FIN, S_IN = S%OUT1)
            END SELECT
          CASE(WORD_CH)
            SELECT CASE( STR(LOCAL_ISTART:LOCAL_ISTART) )
              CASE("a":"z","A":"Z","0":"9","_")
                NEXT_START = LOCAL_ISTART + 1
                RES = RUN_NFA_FULL(NFA, STR, NEXT_START, FIN, S_IN = S%OUT1)
            END SELECT
          CASE(SPACE_CH)
            SELECT CASE( STR(LOCAL_ISTART:LOCAL_ISTART) )
              CASE(" ", ACHAR(9), ACHAR(10))
                NEXT_START = LOCAL_ISTART + 1
                RES = RUN_NFA_FULL(NFA, STR, NEXT_START, FIN, S_IN = S%OUT1)
            END SELECT

          CASE(N_ALPHA_CH)
            SELECT CASE( STR(LOCAL_ISTART:LOCAL_ISTART) )
              CASE("a":"z","A":"Z")
              CASE DEFAULT
                NEXT_START = LOCAL_ISTART + 1
                RES = RUN_NFA_FULL(NFA, STR, NEXT_START, FIN, S_IN = S%OUT1)
            END SELECT
          CASE(N_NUMERIC_CH)
            SELECT CASE( STR(LOCAL_ISTART:LOCAL_ISTART) )
              CASE("0":"9")
              CASE DEFAULT
                NEXT_START = LOCAL_ISTART + 1
                RES = RUN_NFA_FULL(NFA, STR, NEXT_START, FIN, S_IN = S%OUT1)
            END SELECT
          CASE(N_WORD_CH)
            SELECT CASE( STR(LOCAL_ISTART:LOCAL_ISTART) )
              CASE("a":"z","A":"Z","0:9","_")
              CASE DEFAULT
                NEXT_START = LOCAL_ISTART + 1
                RES = RUN_NFA_FULL(NFA, STR, NEXT_START, FIN, S_IN = S%OUT1)
            END SELECT
          CASE(N_SPACE_CH)
            SELECT CASE( STR(LOCAL_ISTART:LOCAL_ISTART) )
              CASE(" ", ACHAR(9), ACHAR(10))
              CASE DEFAULT
                NEXT_START = LOCAL_ISTART + 1
                RES = RUN_NFA_FULL(NFA, STR, NEXT_START, FIN, S_IN = S%OUT1)
            END SELECT

          CASE(START_CH)
            IF (START == 1) RES = RUN_NFA_FULL(NFA, STR, START, FIN, S_IN = S%OUT1)

          CASE(OPEN_PAR_CH)
            RES = RUN_NFA_FULL(NFA, STR, LOCAL_ISTART, FIN, S_IN = S%OUT1)

          CASE(CLOSE_PAR_CH)
            RES = RUN_NFA_FULL(NFA, STR, LOCAL_ISTART, FIN, S_IN = S%OUT1)

          CASE(FINISH_CH)

          CASE DEFAULT
            CALL ABORT("Unrecognised state " // ACHAR(S%C))
        END SELECT
      ELSE
        SELECT CASE(S%C)
          CASE(OPEN_PAR_CH)
            RES = RUN_NFA_FULL(NFA, STR, LOCAL_ISTART, FIN, S_IN = S%OUT1)

          CASE(CLOSE_PAR_CH)
            RES = RUN_NFA_FULL(NFA, STR, LOCAL_ISTART, FIN, S_IN = S%OUT1)

          CASE( SPLIT_ST )
            RES = RUN_NFA_FULL(NFA, STR, LOCAL_ISTART, FIN, S_IN = S%OUT1)
            IF (.NOT. RES) RES = RUN_NFA_FULL(NFA, STR, LOCAL_ISTART, FIN, S_IN = S%OUT2)
          CASE( MATCH_ST )
            RES = .TRUE.
            IF (PRESENT(FINISH)) FINISH = LEN(STR)
          CASE( FINISH_CH )
            RES = RUN_NFA_FULL(NFA, STR, LOCAL_ISTART, FIN, S_IN = S%OUT1)
        END SELECT
      END IF

    END SUBROUTINE STEP

  END FUNCTION RUN_NFA_FULL

  !------------------------------------------------------------------------------!
    FUNCTION RE_MATCH(RE, STR)                                                   !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to check a string str against a regular expression re.             !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   character(len=*), intent(in)  ::  re                                       !
  !     Regualr expression to be matched                                         !
  !                                                                              !
  !   character(len=*), intent(in)  ::  str
  !     String to be searched                                                    !
  !------------------------------------------------------------------------------!
  ! RETURNS                                                                      !
  !   TRUE if there is a match, FALSE otherwise.                                 !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    LOGICAL :: RE_MATCH
    CHARACTER(LEN=*), INTENT(IN)  ::  RE
    CHARACTER(LEN=*), INTENT(IN)  ::  STR

    INTEGER                 ::  POSTFIX(PF_BUFF_SIZE)
    TYPE(NFA_TYPE)          ::  NFA
    INTEGER ::  ISTART

    ISTART = 1

    IF (LEN_TRIM(RE) < 1) CALL ABORT("Regular expression cannot be of length 0")
    POSTFIX = RE_TO_PF(TRIM(RE))
    IF(DEBUG) CALL PRINT_PF(POSTFIX)

    NFA = PF_TO_NFA(POSTFIX)
    IF(DEBUG) CALL PRINT_STATE(NFA%HEAD)

    RE_MATCH = RUN_NFA_FULL(NFA, TRIM(STR), ISTART)

    CALL DEALLOCATE_NFA(NFA)

  END FUNCTION RE_MATCH

  !------------------------------------------------------------------------------!
    FUNCTION RE_MATCH_STR(RE, STR)                                               !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to get a substring from str that matches the regular expression re.!
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   character(len=*), intent(in)  ::  re                                       !
  !     Regualr expression to be matched                                         !
  !                                                                              !
  !   character(len=*), intent(in)  ::  str                                      !
  !     String to be searched                                                    !
  !------------------------------------------------------------------------------!
  ! RETURNS                                                                      !
  !   The matching string if there is a match, an empty string otherwise.        !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    CHARACTER(LEN=PF_BUFF_SIZE) :: RE_MATCH_STR
    CHARACTER(LEN=*), INTENT(IN)  ::  RE
    CHARACTER(LEN=*), INTENT(IN)  ::  STR

    INTEGER                 ::  POSTFIX(PF_BUFF_SIZE)
    TYPE(NFA_TYPE)          ::  NFA
    INTEGER ::  ISTART, IFIN
    LOGICAL :: MATCH

    ISTART = 1
    IFIN = -1

    RE_MATCH_STR = " "

    IF (LEN_TRIM(RE) < 1) CALL ABORT("Regular expression cannot be of length 0")
    POSTFIX = RE_TO_PF(TRIM(RE))
    NFA = PF_TO_NFA(POSTFIX)

    MATCH = RUN_NFA_FULL(NFA, TRIM(STR), ISTART, FINISH=IFIN)
    IF (MATCH) RE_MATCH_STR = STR(ISTART:IFIN)

    CALL DEALLOCATE_NFA(NFA)

  END FUNCTION RE_MATCH_STR

  !------------------------------------------------------------------------------!
    SUBROUTINE RE_SPLIT(RE, STR, OUTPUT)                                         !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to split a string into an array of substrings, based on the regular!
  !   expression re.                                                             !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   character(len=*), intent(in)  ::  re                                       !
  !     Regualr expression to be matched                                         !
  !                                                                              !
  !   character(len=*), intent(in)  ::  str                                      !
  !     String to be searched                                                    !
  !                                                                              !
  !   character(len=*), intent(inout), allocatable   :: output(:)                !
  !     Array containing the substrings. This will be (re)allocated within this  !
  !     routine to the size of the number of matches.                            !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    CHARACTER(LEN=*), INTENT(IN)  ::  RE
    CHARACTER(LEN=*), INTENT(IN)  ::  STR
    CHARACTER(LEN=*), INTENT(INOUT), ALLOCATABLE   :: OUTPUT(:)

    TYPE(NFA_TYPE)          ::  NFA
    INTEGER                 ::  POSTFIX(PF_BUFF_SIZE)
    LOGICAL                 ::   IS_MATCH

    INTEGER :: ISTART, FIN, ISPLIT, LAST_FIN, N_SPLITS

    ISTART = 1

    IF (LEN_TRIM(RE) < 1) CALL ABORT("Regular expression cannot be of length 0")
    POSTFIX = RE_TO_PF(TRIM(RE))
    NFA = PF_TO_NFA(POSTFIX)

    ISTART = 1
    ISPLIT = 1
    N_SPLITS = 0

    IS_MATCH = RUN_NFA_FULL(NFA, TRIM(STR), ISTART, FINISH=FIN)
    IF (IS_MATCH) THEN
      N_SPLITS = N_SPLITS + 1
      LAST_FIN = FIN
      ISTART = LAST_FIN+1
      ISPLIT = 2
      DO WHILE (ISTART <= LEN_TRIM(STR))
        IS_MATCH = RUN_NFA_FULL(NFA, TRIM(STR), ISTART, FINISH=FIN)
        IF (.NOT. IS_MATCH) EXIT
        N_SPLITS = N_SPLITS + 1
        LAST_FIN = FIN
        ISPLIT = ISPLIT + 1
        ISTART = LAST_FIN+1
      END DO
      IF (LAST_FIN <= LEN_TRIM(STR)) N_SPLITS = N_SPLITS + 1
    END IF

    IF (N_SPLITS == 0) RETURN

    IF (ALLOCATED(OUTPUT)) DEALLOCATE(OUTPUT)
    ALLOCATE(OUTPUT(N_SPLITS))

    ISTART = 1
    ISPLIT = 1
    OUTPUT = " "

    IS_MATCH = RUN_NFA_FAST(NFA, TRIM(STR), ISTART, FINISH=FIN)
    IF (IS_MATCH) THEN
      OUTPUT(1) = STR(1:ISTART-1)
      LAST_FIN = FIN
      ISTART = LAST_FIN+1
      ISPLIT = 2
      DO WHILE (ISTART <= LEN_TRIM(STR))
        IS_MATCH = RUN_NFA_FAST(NFA, TRIM(STR), ISTART, FINISH=FIN)
        IF (.NOT. IS_MATCH) EXIT
        OUTPUT(ISPLIT) = STR(LAST_FIN+1:ISTART-1)
        LAST_FIN = FIN
        ISPLIT = ISPLIT + 1
        ISTART = LAST_FIN+1
      END DO
      IF (LAST_FIN < LEN_TRIM(STR)) OUTPUT(ISPLIT) = STR(LAST_FIN+1:)
    END IF

    CALL DEALLOCATE_NFA(NFA)

  END SUBROUTINE RE_SPLIT

  !------------------------------------------------------------------------------!
    FUNCTION RE_REPLACE(RE, REPL, STR)                                           !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to replace each occurance of re with repl in str                  .!
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   character(len=*), intent(in)  ::  re                                       !
  !     Regualr expression to be matched                                         !
  !                                                                              !
  !   character(len=*), intent(in)  ::  repl                                     !
  !     String to replace the regular expression                                 !
  !                                                                              !
  !   character(len=*), intent(in)  ::  str                                      !
  !     String to be searched                                                    !
  !------------------------------------------------------------------------------!
  ! RETURNS                                                                      !
  !   The matching string if there is a match, an empty string otherwise.        !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    CHARACTER(LEN=PF_BUFF_SIZE) :: RE_REPLACE
    CHARACTER(LEN=*), INTENT(IN)  ::  RE
    CHARACTER(LEN=*), INTENT(IN)  ::  REPL
    CHARACTER(LEN=*), INTENT(IN)  ::  STR

    INTEGER                 ::  POSTFIX(PF_BUFF_SIZE)
    TYPE(NFA_TYPE)          ::  NFA
    INTEGER ::  ISTART, IFIN, LAST_FIN, REP_PTR
    LOGICAL :: MATCH

    ISTART = 1
    IFIN = -1
    LAST_FIN = 0
    REP_PTR = 0

    RE_REPLACE = " "

    IF (LEN_TRIM(RE) < 1) CALL ABORT("Regular expression cannot be of length 0")
    POSTFIX = RE_TO_PF(TRIM(RE))
    NFA = PF_TO_NFA(POSTFIX)

    MATCH = RUN_NFA_FAST(NFA, TRIM(STR), ISTART, FINISH=IFIN)
    IF (MATCH) THEN
      RE_REPLACE = STR(1:ISTART-1) // REPL
      REP_PTR = ISTART + LEN(REPL)-1
      LAST_FIN = IFIN
    END IF

    DO WHILE (IFIN <= LEN(STR))
      ISTART=IFIN+1
      MATCH = RUN_NFA_FAST(NFA, TRIM(STR), ISTART, FINISH=IFIN)
      IF (MATCH) THEN
        RE_REPLACE = RE_REPLACE(1:REP_PTR) // STR(LAST_FIN+1:ISTART-1) // REPL
        REP_PTR = REP_PTR + (ISTART-LAST_FIN+1) + LEN(REPL)-2
        LAST_FIN = IFIN
      ELSE
        EXIT
      END IF
    END DO

    RE_REPLACE = RE_REPLACE(1:REP_PTR) // STR(LAST_FIN+1:)

    CALL DEALLOCATE_NFA(NFA)

  END FUNCTION RE_REPLACE

END MODULE ModLib_RegExp

!! Fortran Regular Expression (Forgex)
!!
!! MIT License
!!
!! (C) Amasaki Shinobu, 2023
!!     A regular expression engine for Fortran.
!!     ModLib_Automaton module is a part of Forgex.

MODULE ModLib_Automaton
   USE, INTRINSIC :: ISO_FORTRAN_ENV, STDERR=>ERROR_UNIT
   USE :: ModLib_Segment
   USE :: ModLib_SyntaxTree
   USE :: ModLib_UTF8
   IMPLICIT NONE
   PRIVATE

   PUBLIC :: CHECK_NFA_STATE
   PUBLIC :: BUILD_NFA
   PUBLIC :: PRINT_NFA
   PUBLIC :: DISJOIN_NFA

   ! Upper limit of NFA state instance
   INTEGER(INT32), PARAMETER, PUBLIC :: NFA_STATE_MAX = 1024

   ! Upper limit of NFA transition instance
   INTEGER(INT32), PARAMETER, PUBLIC :: NFA_VECTOR_SIZE = NFA_STATE_MAX

   ! nlist_t is a type represents a transition on NFA.
   ! It transit to state 'to' by character segment 'c'.
   TYPE, PUBLIC :: NLIST_T
      TYPE(SEGMENT_T) :: C = SEG_EMPTY
      INTEGER(INT32) :: TO = 0
      TYPE(NLIST_T), POINTER :: NEXT => NULL()
      INTEGER(INT32) :: INDEX
   END TYPE

   ! NFA_state_set_t represents set of NFA states.
   TYPE, PUBLIC :: NFA_STATE_SET_T
      LOGICAL :: VEC(NFA_VECTOR_SIZE) = .FALSE.
   END TYPE

   ! A table of transition on NFA.
   ! type(nlist_t), public, target :: nfa(NFA_STATE_MAX)

   ! Initial and accepting state on NFA.
   INTEGER(INT32), PUBLIC :: NFA_ENTRY, NFA_EXIT

!---------------------------------------------------------------------!

      ! Upper limit for number of DFA states instance.
   INTEGER(INT32), PARAMETER :: DFA_STATE_MAX = 1024

   ! D_list_t is the type represents a list of transitionable NFA state.
   TYPE :: D_LIST_T
      TYPE(SEGMENT_T), ALLOCATABLE :: C(:)
      TYPE(NFA_STATE_SET_T) :: TO
      TYPE(D_LIST_T), POINTER :: NEXT => NULL()
   END TYPE


   ! D_slist_t is the type represents a list of transition destinations
   ! It transition to state 'to' by character segment 'c'.
   TYPE :: D_SLIST_T
      TYPE(SEGMENT_T), ALLOCATABLE :: C(:)
      TYPE(D_STATE_T), POINTER :: TO => NULL()
      TYPE(D_SLIST_T), POINTER :: NEXT => NULL()    ! pointer to next data
   END TYPE


   ! D_state_t is the type represents a state of DFA.
   TYPE :: D_STATE_T
      INTEGER(INT32) :: INDEX
      TYPE(NFA_STATE_SET_T), POINTER :: STATE => NULL()  ! a set of NFA states represented by this DFA state.
      LOGICAL :: VISITED = .FALSE.                       ! .true. if already processed.
      LOGICAL :: ACCEPTED = .FALSE.                      ! .true. if it includes accepting state.
      TYPE(D_SLIST_T), POINTER :: NEXT => NULL()         ! list of transition destinations
   END TYPE

!---------------------------------------------------------------------!

   TYPE, PUBLIC :: AUTOMATON_T
      CHARACTER(:), ALLOCATABLE :: PATTERN
      INTEGER(INT32) :: NFA_NSTATE = 0       ! Number of NFA states.
      INTEGER(INT32) :: DFA_NSTATE = 0       ! Number of DFA states.
      TYPE(NLIST_T), POINTER :: NFA(:)
      TYPE(D_STATE_T), POINTER :: DFA(:)
      TYPE(D_STATE_T), POINTER :: INITIAL_DFA_STATE => NULL()
      TYPE(D_LIST_T), POINTER :: DLIST => NULL()
   CONTAINS
      PROCEDURE :: INIT
      PROCEDURE :: GENERATE_NODE
      ! NFA procedures
      PROCEDURE :: ADD_TRANSITION
      PROCEDURE :: GENERATE_NFA
      PROCEDURE :: BUILD_NFA
      PROCEDURE :: PRINT_NFA
      PROCEDURE :: DISJOIN => DISJOIN_NFA
      ! DFA procedures
      PROCEDURE :: MARK_EMPTY_TRANSITION
      PROCEDURE :: REGISTER_D_STATE
      PROCEDURE :: FETCH_UNVISITED_D_STATE
      PROCEDURE :: COLLECT_EMPTY_TRANSITION
      PROCEDURE :: COMPUTE_REACHABLE_N_STATE
      PROCEDURE :: CONVERT_NFA_TO_DFA
      PROCEDURE :: PRINT_NFA_STATE_SET
      PROCEDURE :: PRINT_DFA
      PROCEDURE :: MATCHING
      PROCEDURE :: MATCHING_EXACTLY
      PROCEDURE :: FREE => DEALLOCATE_AUTOMATON
   END TYPE

   ! Pointer allocation monitoring

   TYPE :: DLIST_POINTER_LIST_T
      TYPE(D_LIST_T), POINTER :: NODE
   END TYPE

   TYPE :: DSLIST_POINTER_LIST_T
      TYPE(D_SLIST_T), POINTER :: NODE
   END TYPE

   TYPE :: NLIST_POINTER_LIST_T
      TYPE(NLIST_T), POINTER :: NODE
   END TYPE

   TYPE :: DSTATE_POINTER_LIST_T
      TYPE(D_STATE_T), POINTER :: NODE
   END TYPE

   TYPE(DLIST_POINTER_LIST_T) :: DLIST_NODE_LIST(DFA_STATE_MAX)
   TYPE(DSLIST_POINTER_LIST_T) :: DSLIST_NODE_LIST(DFA_STATE_MAX)
   TYPE(NLIST_POINTER_LIST_T) :: NLIST_NODE_LIST(NFA_STATE_MAX)
   TYPE(DSTATE_POINTER_LIST_T) :: DSTATE_NODE_LIST(DFA_STATE_MAX)

   INTEGER(INT32) :: DLIST_NODE_COUNT  = 0
   INTEGER(INT32) :: DSLIST_NODE_COUNT = 0
   INTEGER(INT32) :: NLIST_NODE_COUNT  = 0
   INTEGER(INT32) :: DSTATE_NODE_COUNT = 0


CONTAINS

   SUBROUTINE INIT(SELF)
      IMPLICIT NONE
      CLASS(AUTOMATON_T) :: SELF
      INTEGER :: I

      SELF%NFA_NSTATE = 0
      SELF%DFA_NSTATE = 0

      ALLOCATE(SELF%NFA(NFA_STATE_MAX))
      ALLOCATE(SELF%DFA(DFA_STATE_MAX))

      DO I = 1, SIZE(SELF%DFA, DIM=1)
         SELF%DFA(I)%INDEX = I
      END DO

      DO I = 1, SIZE(SELF%NFA, DIM=1)
         SELF%NFA(I)%INDEX = I
      END DO

   END SUBROUTINE INIT


   SUBROUTINE BUILD_NFA(SELF, TREE)
      IMPLICIT NONE
      CLASS(AUTOMATON_T) :: SELF
      TYPE(TREE_T), INTENT(IN)  :: TREE

      NFA_ENTRY = SELF%GENERATE_NODE()

      NFA_EXIT = SELF%GENERATE_NODE()

      CALL SELF%GENERATE_NFA(TREE, NFA_ENTRY, NFA_EXIT)

      CALL SELF%DISJOIN()

   END SUBROUTINE BUILD_NFA


   SUBROUTINE DEALLOCATE_AUTOMATON(SELF)
      IMPLICIT NONE
      CLASS(AUTOMATON_T) :: SELF
      INTEGER :: J, MAX

      MAX = NLIST_NODE_COUNT
      DO J = 1, MAX
         IF (ASSOCIATED(NLIST_NODE_LIST(J)%NODE)) THEN
            DEALLOCATE(NLIST_NODE_LIST(J)%NODE)
            NLIST_NODE_COUNT = NLIST_NODE_COUNT -1
         END IF
      END DO

      MAX = DLIST_NODE_COUNT
      DO J = 1, MAX
         IF (ASSOCIATED(DLIST_NODE_LIST(J)%NODE)) THEN
            IF (ALLOCATED(DLIST_NODE_LIST(J)%NODE%C)) THEN
               DEALLOCATE(DLIST_NODE_LIST(J)%NODE%C)
            END IF

            DEALLOCATE(DLIST_NODE_LIST(J)%NODE)
            DLIST_NODE_COUNT = DLIST_NODE_COUNT -1
         END IF
      END DO

      MAX = DSLIST_NODE_COUNT
      DO J = 1, MAX
         IF (ASSOCIATED(DSLIST_NODE_LIST(J)%NODE)) THEN
            IF (ALLOCATED(DSLIST_NODE_LIST(J)%NODE%C)) THEN
               DEALLOCATE(DSLIST_NODE_LIST(J)%NODE%C)
            END IF

            DEALLOCATE(DSLIST_NODE_LIST(J)%NODE)
            DSLIST_NODE_COUNT = DSLIST_NODE_COUNT -1
         END IF
      END DO

      MAX = DSTATE_NODE_COUNT
      DO J = 1, MAX
         IF (ASSOCIATED(DSTATE_NODE_LIST(J)%NODE)) THEN
            DEALLOCATE(DSTATE_NODE_LIST(J)%NODE)
            DSTATE_NODE_COUNT = DSTATE_NODE_COUNT -1
         END IF
      END DO

      IF (ASSOCIATED(SELF%NFA)) DEALLOCATE(SELF%NFA)

      IF (ASSOCIATED(SELF%DFA)) DEALLOCATE(SELF%DFA)


   END SUBROUTINE DEALLOCATE_AUTOMATON


!=====================================================================!

   FUNCTION GENERATE_NODE(SELF)
      IMPLICIT NONE
      CLASS(AUTOMATON_T) :: SELF
      INTEGER(INT32) :: GENERATE_NODE

      IF (SELF%NFA_NSTATE >= NFA_STATE_MAX) THEN
         WRITE(STDERR, *) "Number of NFA states too large."
         ERROR STOP
      END IF

      SELF%NFA_NSTATE = SELF%NFA_NSTATE + 1
      GENERATE_NODE = SELF%NFA_NSTATE

   END FUNCTION GENERATE_NODE


   SUBROUTINE ADD_TRANSITION(SELF, FROM, TO, C)
      IMPLICIT NONE
      CLASS(AUTOMATON_T) :: SELF
      INTEGER(INT32), INTENT(IN) :: FROM, TO
      TYPE(SEGMENT_T), INTENT(IN) :: C

      TYPE(NLIST_T), POINTER :: P

      P => NULL()
      ALLOCATE(P)

      NLIST_NODE_COUNT = NLIST_NODE_COUNT + 1
      NLIST_NODE_LIST(NLIST_NODE_COUNT)%NODE => P

      P = SELF%NFA(FROM)

      SELF%NFA(FROM)%C%MIN = C%MIN
      SELF%NFA(FROM)%C%MAX = C%MAX
      SELF%NFA(FROM)%TO = TO
      SELF%NFA(FROM)%NEXT => P

   END SUBROUTINE ADD_TRANSITION


   RECURSIVE SUBROUTINE GENERATE_NFA(SELF, TREE, ENTRY, WAY_OUT)
      IMPLICIT NONE
      CLASS(AUTOMATON_T) :: SELF
      TYPE(TREE_T), INTENT(IN) :: TREE
      INTEGER(INT32), INTENT(IN) :: ENTRY, WAY_OUT

      INTEGER :: A1, A2, J

      SELECT CASE (TREE%OP)
      CASE (OP_CHAR)
         DO J = 1, SIZE(TREE%C, DIM=1)
            CALL SELF%ADD_TRANSITION(ENTRY, WAY_OUT, TREE%C(J))
         END DO

      CASE (OP_EMPTY)
         CALL SELF%ADD_TRANSITION(ENTRY, WAY_OUT, SEG_EMPTY)

      CASE (OP_UNION)
         CALL SELF%GENERATE_NFA(TREE%LEFT, ENTRY, WAY_OUT)
         CALL SELF%GENERATE_NFA(TREE%RIGHT, ENTRY, WAY_OUT)

      CASE (OP_CLOSURE)
         A1 = SELF%GENERATE_NODE()
         A2 = SELF%GENERATE_NODE()
         CALL SELF%ADD_TRANSITION(ENTRY, A1, SEG_EMPTY)
         CALL SELF%GENERATE_NFA(TREE%LEFT, A1, A2)
         CALL SELF%ADD_TRANSITION(A2, A1, SEG_EMPTY)
         CALL SELF%ADD_TRANSITION(A1, WAY_OUT, SEG_EMPTY)

      CASE (OP_CONCAT)
         A1 = SELF%GENERATE_NODE()
         CALL SELF%GENERATE_NFA(TREE%LEFT, ENTRY, A1)
         CALL SELF%GENERATE_NFA(TREE%RIGHT, A1, WAY_OUT)

      CASE DEFAULT
         WRITE(STDERR, *) "This will not happen in 'generate_nfa'."
         ERROR STOP
      END SELECT

   END SUBROUTINE GENERATE_NFA


   ! Is the arguement 'state' (set of NFA state) includes state 's'?
   LOGICAL FUNCTION CHECK_NFA_STATE(STATE, S)
      IMPLICIT NONE
      TYPE(NFA_STATE_SET_T), INTENT(IN) :: STATE
      INTEGER(INT32) :: S

      IF (S /= 0) THEN
         CHECK_NFA_STATE = STATE%VEC(S)
      ELSE
         CHECK_NFA_STATE = .FALSE.
      END IF

   END FUNCTION CHECK_NFA_STATE


   SUBROUTINE DISJOIN_NFA(SELF)
      USE :: ModLib_PQueue
      USE :: ModLib_SegmentDisjoin
      IMPLICIT NONE
      CLASS(AUTOMATON_T) :: SELF
      TYPE(NLIST_T), POINTER :: P
      INTEGER(INT32) :: I, J
      TYPE(PRIORITY_QUEUE_T) :: QUEUE
      TYPE(SEGMENT_T), ALLOCATABLE :: SEG_LIST(:)
      INTEGER :: NUM


      NUM = 0
      P => NULL()

      BLOCK ! enqueue
         DO I = 1, SELF%NFA_NSTATE
            P => SELF%NFA(I)

            DO WHILE (ASSOCIATED(P))
               IF (P%TO /= 0 ) THEN

                  IF (P%C /= SEG_EMPTY) CALL ENQUEUE(QUEUE, P%C)
               END IF
               P => P%NEXT
            END DO
         END DO
      END BLOCK ! enqueue

      NUM = QUEUE%NUMBER

      ALLOCATE(SEG_LIST(NUM))
      DO J = 1, NUM
         SEG_LIST(J) = DEQUEUE(QUEUE)
      END DO

      !-- seg_list array is sorted.

      CALL DISJOIN(SEG_LIST)

      DO I = 1, SELF%NFA_NSTATE
         P => SELF%NFA(I)

         IF (.NOT. IS_PRIME_SEMGMENT(P%C, SEG_LIST)) THEN
            CALL DISJOIN_NFA_STATE(P, SEG_LIST)
         END IF
      END DO

      DO I = 1, SELF%NFA_NSTATE

         P => SELF%NFA(I)%NEXT

         INNER: DO WHILE (ASSOCIATED(P))

            IF (.NOT. IS_PRIME_SEMGMENT(P%C, SEG_LIST)) THEN
               CALL DISJOIN_NFA_STATE(P, SEG_LIST)
            END IF

            IF (P%INDEX > 0) EXIT INNER

            P => P%NEXT

         END DO INNER
      END DO

      !-- deallocate
      CALL CLEAR(QUEUE)
      DEALLOCATE(SEG_LIST)

   END SUBROUTINE DISJOIN_NFA

   SUBROUTINE DISJOIN_NFA_STATE(STATE, SEG_LIST)
      USE :: ModLib_SegmentDisjoin
      IMPLICIT NONE
      TYPE(NLIST_T), POINTER, INTENT(INOUT) ::STATE

      TYPE(SEGMENT_T), INTENT(INOUT) :: SEG_LIST(:)

      INTEGER :: J, K, SIZ
      SIZ = SIZE(SEG_LIST, DIM=1)

      BLOCK
         LOGICAL :: FLAG(SIZ)
         FLAG = IS_OVERLAP_TO_SEG_LIST(STATE%C, SEG_LIST, SIZ)

         K = 1

         DO J = 1, SIZ
            IF (FLAG(J)) THEN
               BLOCK
                  TYPE(NLIST_T), POINTER :: PTR
                  PTR => NULL()

                  IF (J == 1) THEN
                     STATE%C = SEG_LIST(J)
                  ELSE
                     ALLOCATE(PTR)

                     NLIST_NODE_COUNT = NLIST_NODE_COUNT + 1
                     NLIST_NODE_LIST(NLIST_NODE_COUNT)%NODE => PTR

                     PTR = STATE
                     STATE%C = SEG_LIST(J)
                     STATE%TO = PTR%TO
                     STATE%NEXT => PTR
                  END IF

               END BLOCK
            END IF
         END DO
      END BLOCK



   END SUBROUTINE DISJOIN_NFA_STATE

!==============================================================================!

   SUBROUTINE ADD_NFA_STATE(STATE, S)
      IMPLICIT NONE
      TYPE(NFA_STATE_SET_T), INTENT(INOUT) :: STATE
      INTEGER(INT32), INTENT(IN):: S

      STATE%VEC(S) = .TRUE.
   END SUBROUTINE ADD_NFA_STATE


   RECURSIVE SUBROUTINE MARK_EMPTY_TRANSITION(SELF, STATE, S)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      CLASS(AUTOMATON_T) :: SELF
      TYPE(NFA_STATE_SET_T), INTENT(INOUT) :: STATE
      INTEGER(INT32), INTENT(IN) :: S

      TYPE(NLIST_T), POINTER :: P => NULL()

      CALL ADD_NFA_STATE(STATE, S)

      P => SELF%NFA(S)
      DO WHILE (ASSOCIATED(P))

         IF (P%C == SEG_EMPTY .AND. .NOT. CHECK_NFA_STATE(STATE, P%TO) ) THEN
            IF (P%TO /= 0) CALL SELF%MARK_EMPTY_TRANSITION(STATE, P%TO)
         END IF

         IF (.NOT. ASSOCIATED(P)) EXIT
         P => P%NEXT

      ENDDO

   END SUBROUTINE MARK_EMPTY_TRANSITION


   SUBROUTINE COLLECT_EMPTY_TRANSITION (SELF, STATE)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      CLASS(AUTOMATON_T) :: SELF
      TYPE(NFA_STATE_SET_T), INTENT(INOUT), TARGET :: STATE
      INTEGER(INT32) :: I

      DO I = 1, SELF%NFA_NSTATE

         IF (CHECK_NFA_STATE(STATE, I)) CALL SELF%MARK_EMPTY_TRANSITION(STATE, I)

      END DO
   END SUBROUTINE COLLECT_EMPTY_TRANSITION


   FUNCTION EQUIVALENT_NFA_STATE_SET(A, B) RESULT(RES)
      IMPLICIT NONE
      TYPE(NFA_STATE_SET_T), INTENT(IN), POINTER  :: A
      TYPE(NFA_STATE_SET_T), INTENT(IN)  :: B
      INTEGER(INT32) :: I
      LOGICAL :: RES


      DO I = 1, NFA_VECTOR_SIZE
         IF (A%VEC(I) .NEQV. B%VEC(I)) THEN
            RES = .FALSE.
            RETURN
         END IF
      END DO
      RES = .TRUE.

   END FUNCTION EQUIVALENT_NFA_STATE_SET


   FUNCTION REGISTER_D_STATE(SELF, S) RESULT(RES)
      IMPLICIT NONE
      CLASS(AUTOMATON_T) :: SELF
      TYPE(NFA_STATE_SET_T), INTENT(IN), TARGET :: S
      INTEGER(INT32) :: I, K
      TYPE(D_STATE_T), POINTER :: RES

      RES => NULL()

      DO I = 1, SELF%DFA_NSTATE

         IF (EQUIVALENT_NFA_STATE_SET(SELF%DFA(I)%STATE, S)) THEN
            RES => SELF%DFA(I)
            RETURN
         END IF

      END DO

      IF (SELF%DFA_NSTATE >= DFA_STATE_MAX) THEN
         WRITE(STDERR, '(a)') "Number of DFA states too large.."
         ERROR STOP
      END IF

      SELF%DFA_NSTATE = SELF%DFA_NSTATE + 1

      K = SELF%DFA_NSTATE

      SELF%DFA(K)%STATE => S
      SELF%DFA(K)%VISITED = .FALSE.
      SELF%DFA(K)%ACCEPTED = CHECK_NFA_STATE(S, NFA_EXIT)
      SELF%DFA(K)%NEXT => NULL()

      RES => SELF%DFA(K)

   END FUNCTION REGISTER_D_STATE


   FUNCTION FETCH_UNVISITED_D_STATE(SELF) RESULT(RES)
      IMPLICIT NONE
      CLASS(AUTOMATON_T) :: SELF
      TYPE(D_STATE_T), POINTER :: RES
      INTEGER(INT32) :: I

      RES => NULL()

      DO I = 1, SELF%DFA_NSTATE
         IF (SELF%DFA(I)%VISITED .EQV. .FALSE.) THEN
            RES => SELF%DFA(I)
            RETURN
         END IF
      END DO
   END FUNCTION FETCH_UNVISITED_D_STATE


   FUNCTION COMPUTE_REACHABLE_N_STATE(SELF, DSTATE) RESULT(RES)
      USE, INTRINSIC :: ISO_C_BINDING
      IMPLICIT NONE
      CLASS(AUTOMATON_T) :: SELF

      TYPE(D_STATE_T), INTENT(IN) :: DSTATE

      TYPE(D_LIST_T), POINTER :: RES
      INTEGER(INT32):: I, J
      TYPE(NFA_STATE_SET_T), POINTER :: STATE
      TYPE(NLIST_T), POINTER :: P
      TYPE(D_LIST_T), POINTER :: B
      TYPE(D_LIST_T), POINTER :: A

      P => NULL()
      A => NULL()
      B => NULL()
      RES => NULL()
      STATE => DSTATE%STATE

      ! Iterate the all NFA states
      OUTER: DO I = 1, SELF%NFA_NSTATE

         ! If NFA state <i> is included in DFA state <dstate>, perform the following processing.
         IF (CHECK_NFA_STATE(STATE, I)) THEN

            ! Examine all NFA states reachable from NFA state <i> and list them.
            P => SELF%NFA(I)

            MIDDLE: DO WHILE (ASSOCIATED(P))

               ! Except for ε-transition.
               IF (P%C /= SEG_EMPTY) THEN

                  A => RES
                  INNER: DO WHILE(ASSOCIATED(A))

                     DO J = 1, SIZE(A%C, DIM=1)
                        IF (A%C(J) == P%C .AND. P%TO /= 0) THEN
                           CALL ADD_NFA_STATE(A%TO, P%TO)

                           ! Move to next NFA state
                           P => P%NEXT
                           CYCLE MIDDLE

                        END IF
                     END DO
                     A => A%NEXT

                  END DO INNER

                  IF (P%TO /= 0) THEN
                     ALLOCATE(B)
                     ALLOCATE(B%C(1))

                     DLIST_NODE_COUNT = DLIST_NODE_COUNT +1
                     DLIST_NODE_LIST(DLIST_NODE_COUNT)%NODE => B

                     B%C(1) = P%C
                     CALL ADD_NFA_STATE(B%TO, P%TO)
                     B%NEXT => RES
                     RES => B
                  END IF

               END IF

               P => P%NEXT

            END DO MIDDLE

         END IF
      END DO OUTER

   END FUNCTION COMPUTE_REACHABLE_N_STATE


   SUBROUTINE CONVERT_NFA_TO_DFA(SELF)
      IMPLICIT NONE
      CLASS(AUTOMATON_T) :: SELF
      TYPE(NFA_STATE_SET_T), TARGET :: INITIAL_STATE
      TYPE(D_STATE_T), POINTER :: T
      TYPE(D_LIST_T), POINTER :: X
      TYPE(D_SLIST_T), POINTER :: P

      T => NULL()
      X => NULL()
      P => NULL()

      CALL ADD_NFA_STATE(INITIAL_STATE, NFA_ENTRY)
      CALL SELF%COLLECT_EMPTY_TRANSITION(INITIAL_STATE)

      SELF%INITIAL_DFA_STATE => SELF%REGISTER_D_STATE(INITIAL_STATE)

      T => SELF%FETCH_UNVISITED_D_STATE()
      DO WHILE (ASSOCIATED(T))


         T%VISITED = .TRUE.

         X => SELF%COMPUTE_REACHABLE_N_STATE(T)

         DO WHILE (ASSOCIATED(X))

            CALL SELF%COLLECT_EMPTY_TRANSITION(X%TO)

            ALLOCATE(P)

            DSLIST_NODE_COUNT = DSLIST_NODE_COUNT + 1
            DSLIST_NODE_LIST(DSLIST_NODE_COUNT)%NODE => P

            P%C = X%C

            P%TO => SELF%REGISTER_D_STATE(X%TO)
            P%NEXT => T%NEXT
            T%NEXT => P

            X => X%NEXT
         END DO

         T => SELF%FETCH_UNVISITED_D_STATE()
      END DO

   END SUBROUTINE CONVERT_NFA_TO_DFA

!=====================================================================!

   FUNCTION NEXT_STATE_DFA(STATE, CHARA) RESULT(RES)
      USE :: ModLib_UTF8
      IMPLICIT NONE
      TYPE(D_STATE_T), INTENT(IN) :: STATE
      CHARACTER(*), INTENT(IN) :: CHARA
      TYPE(D_SLIST_T), POINTER :: PTR
      TYPE(D_STATE_T), POINTER :: RES

      INTEGER :: INEXT, J

      PTR => STATE%NEXT
      DO WHILE (ASSOCIATED(PTR))
         INEXT = IDXUTF8(CHARA, 1) + 1

         DO J = 1, SIZE(PTR%C, DIM=1)
            IF ( PTR%C(J)%MIN <= ICHAR_UTF8(CHARA(1:INEXT)) .AND. ICHAR_UTF8(CHARA(1:INEXT)) <= PTR%C(J)%MAX) THEN
               RES => PTR%TO
               RETURN
            END IF
         END DO
         PTR => PTR%NEXT
      END DO


      RES => NULL()
   END FUNCTION NEXT_STATE_DFA


   SUBROUTINE MATCHING (SELF, STR_ARG, FROM, TO)
      USE :: ModLib_UTF8
      IMPLICIT NONE
      CLASS(AUTOMATON_T) :: SELF
      CHARACTER(*), INTENT(IN) :: STR_ARG
      CHARACTER(:), ALLOCATABLE :: STR
      INTEGER(INT32), INTENT(INOUT) :: FROM, TO
      TYPE(D_STATE_T), POINTER :: STATE

      INTEGER(INT32) :: START, NEXT
      INTEGER(INT32) :: MAX_MATCH, I

      STR = STR_ARG

      FROM = 0
      TO = 0

      IF (STR == CHAR(10)//CHAR(10)) THEN
         STR = ''
         STATE => SELF%INITIAL_DFA_STATE
         IF (STATE%ACCEPTED) THEN
            FROM = 1
            TO = 1
         END IF

         RETURN
      END IF

      ! Match the pattern by shifting one character from the begining of string str.
      ! This loop should be parallelized.
      START = 1
      DO WHILE (START < LEN(STR))

         ! Initialize DFA
         MAX_MATCH = 0
         I = START
         STATE => SELF%INITIAL_DFA_STATE

         !
         DO WHILE( ASSOCIATED(STATE))

            ! 任意の位置の空文字にはマッチさせない
            IF (STATE%ACCEPTED .AND. I /= START) THEN
               MAX_MATCH = I
            END IF

            IF (I > LEN(STR)) EXIT

            NEXT = IDXUTF8(STR, I) + 1

            STATE => NEXT_STATE_DFA(STATE, STR(I:NEXT-1))

            I = NEXT

         END DO

         IF (MAX_MATCH > 1) THEN
            FROM = START
            TO = MAX_MATCH -1
            RETURN
         END IF

         START = IDXUTF8(STR, START) + 1
      END DO

   END SUBROUTINE


   FUNCTION MATCHING_EXACTLY (SELF, STR) RESULT(RES)
      IMPLICIT NONE
      CLASS(AUTOMATON_T) :: SELF
      CHARACTER(*), INTENT(IN) :: STR
      LOGICAL :: RES

      INTEGER(INT32) :: MAX_MATCH, I, NEXT
      TYPE(D_STATE_T), POINTER :: STATE

      ! Initialize DFA
      MAX_MATCH = 0
      I = 1
      STATE => SELF%INITIAL_DFA_STATE

      IF (STR == '') THEN
         RES = STATE%ACCEPTED
         RETURN
      END IF

      DO WHILE( ASSOCIATED(STATE))

         IF (STATE%ACCEPTED) THEN
            MAX_MATCH = I
         END IF

         IF (I > LEN(STR)) EXIT

         NEXT = IDXUTF8(STR, I) + 1

         STATE => NEXT_STATE_DFA(STATE, STR(I:NEXT-1))

         I = NEXT

      END DO

      IF (MAX_MATCH == LEN(STR)+1) THEN
         RES = .TRUE.
      ELSE
         RES = .FALSE.
      END IF

   END FUNCTION MATCHING_EXACTLY

!======================================================================================!

   SUBROUTINE PRINT_NFA(SELF)
      IMPLICIT NONE
      CLASS(AUTOMATON_T) :: SELF
      INTEGER :: I
      TYPE(NLIST_T), POINTER :: P
      CHARACTER(:), ALLOCATABLE :: CACHE

      WRITE(STDERR, *) "--- PRINT NFA ---"

      DO I = 1, SELF%NFA_NSTATE
         IF (I <= SELF%NFA_NSTATE) THEN

            WRITE(STDERR, '(a, i3, a)', ADVANCE='no') "state ", I, ": "
            P => SELF%NFA(I)

            DO WHILE (ASSOCIATED(P))
               IF (P%TO /= 0 ) THEN

                  CACHE = P%C%PRINT()

                  IF (P%C == SEG_EMPTY) CACHE = '?'

                  WRITE(STDERR, "(a, a, a2, i0, a1)", ADVANCE='no') "(", TRIM(CACHE),", ", P%TO, ")"
               END IF
               P => P%NEXT
            END DO
            WRITE(STDERR, *) ''
         END IF
      END DO

   END SUBROUTINE PRINT_NFA


   SUBROUTINE PRINT_NFA_STATE_SET (SELF, P)
      IMPLICIT NONE
      CLASS(AUTOMATON_T) :: SELF
      TYPE(NFA_STATE_SET_T), INTENT(IN) :: P

      INTEGER(INT32) :: I

      DO I = 1, SELF%NFA_NSTATE
         IF (CHECK_NFA_STATE(P, I)) WRITE(STDERR, '(i0, a)', ADVANCE='no') I, ' '
      END DO
   END SUBROUTINE PRINT_NFA_STATE_SET


   SUBROUTINE PRINT_DFA(SELF)
      IMPLICIT NONE
      CLASS(AUTOMATON_T) :: SELF
      TYPE(D_SLIST_T), POINTER :: L
      INTEGER(INT32) :: I, J

      WRITE(STDERR,*) "--- PRINT DFA---"

      DO I = 1, SELF%DFA_NSTATE
         IF (SELF%DFA(I)%ACCEPTED) THEN
            WRITE(STDERR, '(i2,a, a)', ADVANCE='no') I, 'A', ": "
         ELSE
            WRITE(STDERR, '(i2,a, a)', ADVANCE='no') I, ' ', ": "
         END IF

         L => SELF%DFA(I)%NEXT
         DO WHILE (ASSOCIATED(L))
            DO J = 1, SIZE(L%C, DIM=1)
               WRITE(STDERR, '(a, a, i0, 1x)', ADVANCE='no') L%C(J)%PRINT(), '=>', L%TO%INDEX
            END DO
            L => L%NEXT
         END DO
         WRITE(STDERR, *) ""
      END DO

      DO I = 1, SELF%DFA_NSTATE
         IF (SELF%DFA(I)%ACCEPTED) THEN
            WRITE(STDERR, '(a, i2, a)', ADVANCE='no') "state ", I, 'A = ( '
         ELSE
            WRITE(STDERR, '(a, i2, a)', ADVANCE='no') "state ", I, '  = ( '
         END IF
         CALL SELF%PRINT_NFA_STATE_SET(SELF%DFA(I)%STATE)
         WRITE(STDERR,'(a)') ")"
      END DO

   END SUBROUTINE PRINT_DFA

END MODULE ModLib_Automaton

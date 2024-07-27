MODULE ModLib_SegmentDisjoin
   USE :: ModLib_Segment
   USE :: ModLib_PQueue
   USE, INTRINSIC :: ISO_FORTRAN_ENV
   PRIVATE

   PUBLIC :: DISJOIN
   PUBLIC :: IS_PRIME_SEMGMENT
   PUBLIC :: IS_OVERLAP_TO_SEG_LIST

   INTERFACE DISJOIN
      MODULE PROCEDURE :: DISJOIN_KERNEL
   END INTERFACE

CONTAINS


   SUBROUTINE DISJOIN_KERNEL(NEW_LIST)

      IMPLICIT NONE
      TYPE(SEGMENT_T), INTENT(INOUT), ALLOCATABLE :: NEW_LIST(:)
      TYPE(SEGMENT_T), ALLOCATABLE :: OLD_LIST(:)

      TYPE(PRIORITY_QUEUE_T) :: PQUEUE
      TYPE(SEGMENT_T), ALLOCATABLE :: BUFF(:)
      TYPE(SEGMENT_T), ALLOCATABLE :: CACHE(:)
      TYPE(SEGMENT_T) :: NEW
      TYPE(SEGMENT_T), PARAMETER :: SEG_UPPER = SEGMENT_T(UTF8_CODE_MAX+1, UTF8_CODE_MAX+1)

      INTEGER(INT32) :: I, J, K, COUNT, SIZ, TOP, BOTTOM, REAL_SIZE, M
      INTEGER(INT32), ALLOCATABLE :: INDEX_LIST(:)
      LOGICAL :: FLAG

      SIZ = SIZE(NEW_LIST, DIM=1)

      CALL MOVE_ALLOC(NEW_LIST, OLD_LIST)

      BLOCK ! heap sort
         DO J = 1, SIZ
            CALL ENQUEUE(PQUEUE, OLD_LIST(J))
         END DO

         ALLOCATE(BUFF(SIZ))

         DO J = 1, SIZ
            BUFF(J) = DEQUEUE(PQUEUE)
         END DO
      END BLOCK

      BLOCK ! get the bottom and top from the segment array.
         BOTTOM = BUFF(1)%MIN
         TOP = 0
         DO J = 1, SIZ
            TOP = MAX(TOP, BUFF(J)%MAX)
         END DO
      END BLOCK

      ALLOCATE(NEW_LIST(SIZ*2))
      ! allocate(cache(siz*2))

      CALL INDEX_LIST_FROM_SEGMENT_LIST(INDEX_LIST, OLD_LIST)

      NEW = SEG_UPPER

      K = 1
      M = 1
      DO WHILE(M <= SIZE(INDEX_LIST))
         I = INDEX_LIST(M)

         ! i が範囲に含まれる場合
         IF (I .IN. BUFF(1:SIZ)) THEN
            IF (I < NEW%MIN) NEW%MIN =I
         ELSE
            M = M + 1
            CYCLE
         END IF

         ! i+1がいずれかのセグメントの始端の場合
         FLAG = .FALSE.
         DO J = 1, SIZ
            IF (I+1 == BUFF(J)%MIN) FLAG = FLAG .OR. .TRUE.
         END DO
         IF (FLAG) THEN
            NEW%MAX = I
            CALL REGISTER_SEG_LIST(NEW, NEW_LIST, K)
            M = M + 1
            CYCLE
         END IF

         COUNT = 0
         DO J = 1, SIZ
            IF (BUFF(J)%MIN == I) COUNT = COUNT + 1
         END DO
         IF (COUNT > 1) THEN
            NEW%MAX = I
            CALL REGISTER_SEG_LIST(NEW, NEW_LIST, K)
         END IF

         COUNT = 0
         DO J = 1, SIZ
            IF (BUFF(J)%MAX == I) COUNT = COUNT + 1
         END DO
         IF (COUNT >0) THEN
            NEW%MAX = I
            CALL REGISTER_SEG_LIST(NEW, NEW_LIST, K)
         END IF

         M = M + 1
      END DO

      REAL_SIZE = 0
      DO I = 1, SIZE(NEW_LIST)
         IF (NEW_LIST(I) /= SEG_EMPTY) REAL_SIZE = REAL_SIZE + 1
      END DO

      CALL MOVE_ALLOC(NEW_LIST, CACHE)  ! new_list is now deallocated.

      ALLOCATE(NEW_LIST(REAL_SIZE))

      NEW_LIST(:) = CACHE(1:REAL_SIZE)

      ! deallocate
      CALL CLEAR(PQUEUE)
      DEALLOCATE(BUFF)
      DEALLOCATE(CACHE)
      DEALLOCATE(INDEX_LIST)

   CONTAINS

      SUBROUTINE REGISTER_SEG_LIST(NEW, LIST, K)
         IMPLICIT NONE
         TYPE(SEGMENT_T), INTENT(INOUT) :: NEW, LIST(:)
         INTEGER(INT32), INTENT(INOUT) :: K

         IF (NEW%VALIDATE()) THEN
            LIST(K) = NEW
            K = K + 1
         END IF
         NEW = SEG_UPPER
      END SUBROUTINE REGISTER_SEG_LIST


   END SUBROUTINE DISJOIN_KERNEL


   FUNCTION IS_PRIME_SEMGMENT(SEG, DISJOINED_LIST) RESULT(RES)
      IMPLICIT NONE
      TYPE(SEGMENT_T), INTENT(IN) :: SEG, DISJOINED_LIST(:)
      LOGICAL :: RES
      INTEGER :: J

      ! リストのうちのいずれかと一致すれば、交差していない。
      RES = .FALSE.
      DO J = 1, SIZE(DISJOINED_LIST)
         RES = RES .OR. ( DISJOINED_LIST(J)%MIN <= SEG%MIN .AND. SEG%MAX <= DISJOINED_LIST(J)%MAX)
      END DO

   END FUNCTION IS_PRIME_SEMGMENT

   FUNCTION IS_OVERLAP_TO_SEG_LIST(SEG, LIST, LEN) RESULT(RES)
      IMPLICIT NONE
      INTEGER(INT32), INTENT(IN) :: LEN
      TYPE(SEGMENT_T), INTENT(IN) :: SEG, LIST(:)
      LOGICAL :: RES(LEN)

      INTEGER :: I

      RES(:) = .FALSE.

      DO I = 1, LEN
         RES(I) = LIST(I) .IN. SEG
      END DO

   END FUNCTION IS_OVERLAP_TO_SEG_LIST


   SUBROUTINE INDEX_LIST_FROM_SEGMENT_LIST(INDEX_LIST, SEG_LIST)
      IMPLICIT NONE
      TYPE(SEGMENT_T), INTENT(IN) :: SEG_LIST(:)
      INTEGER(INT32), INTENT(OUT), ALLOCATABLE :: INDEX_LIST(:)
      INTEGER(INT32), ALLOCATABLE :: CACHE(:)

      INTEGER :: SIZ, I, K

      SIZ = SIZE(SEG_LIST, DIM=1)

      ALLOCATE(INDEX_LIST(6*SIZ))
      ALLOCATE(CACHE(6*SIZ))

      DO I = 1, SIZ
         INDEX_LIST(6*I-5) = SEG_LIST(I)%MIN - 1
         INDEX_LIST(6*I-4) = SEG_LIST(I)%MIN
         INDEX_LIST(6*I-3) = SEG_LIST(I)%MIN + 1
         INDEX_LIST(6*I-2) = SEG_LIST(I)%MAX - 1
         INDEX_LIST(6*I-1) = SEG_LIST(I)%MAX
         INDEX_LIST(6*I)   = SEG_LIST(I)%MAX + 1
      END DO

      CALL BUBBLE_SORT(INDEX_LIST)

      CACHE(1) = INDEX_LIST(1)
      K = 1
      DO I = 2, SIZ*6
         IF (INDEX_LIST(I-1) /= INDEX_LIST(I)) THEN
            K = K + 1
            CACHE(K) = INDEX_LIST(I)
         END IF
      END DO

      DEALLOCATE(INDEX_LIST)
      ALLOCATE(INDEX_LIST(K))
      INDEX_LIST(:) = CACHE(1:K)

   END SUBROUTINE INDEX_LIST_FROM_SEGMENT_LIST

   SUBROUTINE BUBBLE_SORT(LIST)
      IMPLICIT NONE
      INTEGER(INT32), INTENT(INOUT) :: LIST(:)

      INTEGER :: I, J, SIZ, TMP

      SIZ = SIZE(LIST)

      DO I = 1, SIZ-1
         DO J = I+1, SIZ
            IF (LIST(I) > LIST(J)) THEN
               TMP = LIST(I)
               LIST(I) = LIST(J)
               LIST(J) = TMP
            END IF
         END DO
      END DO

   END SUBROUTINE BUBBLE_SORT

END MODULE ModLib_SegmentDisjoin

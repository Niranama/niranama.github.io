!! Fortran Regular Expression (Forgex)
!!
!! MIT License
!!
!! (C) Amasaki Shinobu, 2023
!!     A regular expression engine for Fortran.
!!     priority_queue_m module is a part of Forgex.
!!
!! (C) ue1221, 2021
!!
!! The original Fortran implementation of priority queue is by ue1221.
!! cf. https://github.com/ue1221/fortran-utilities

MODULE ModLib_PQueue
   USE, INTRINSIC :: ISO_FORTRAN_ENV
   USE :: ModLib_Segment
   IMPLICIT NONE

   TYPE PRIORITY_QUEUE_T
      INTEGER(INT32) :: NUMBER = 0
      TYPE(SEGMENT_T), POINTER :: HEAP(:) => NULL()
   END TYPE

CONTAINS

   SUBROUTINE ENQUEUE(PQ, SEG)
      IMPLICIT NONE
      TYPE(PRIORITY_QUEUE_T), INTENT(INOUT) :: PQ
      TYPE(SEGMENT_T), INTENT(IN) :: SEG
      TYPE(SEGMENT_T) :: T
      TYPE(SEGMENT_T), ALLOCATABLE :: TMP(:)
      INTEGER(INT32) :: N, I

      IF (.NOT. ASSOCIATED(PQ%HEAP)) ALLOCATE(PQ%HEAP(1))

      N = PQ%NUMBER
      IF (N == SIZE(PQ%HEAP)) THEN
         ALLOCATE(TMP(N))
         TMP(:) = PQ%HEAP(:)
         DEALLOCATE(PQ%HEAP)
         ALLOCATE(PQ%HEAP(N*2))
         PQ%HEAP(1:N) = TMP(1:N)
      END IF

      PQ%NUMBER = PQ%NUMBER + 1
      PQ%HEAP(PQ%NUMBER) = SEG

      N = PQ%NUMBER
      DO WHILE (N > 1)
         I = N/2
         IF (PQ%HEAP(N)%MIN < PQ%HEAP(I)%MIN &
               .OR. (PQ%HEAP(N)%MIN == PQ%HEAP(I)%MIN .AND. PQ%HEAP(N)%MAX < PQ%HEAP(I)%MAX)) THEN
            T = PQ%HEAP(N)
            PQ%HEAP(N) = PQ%HEAP(I)
            PQ%HEAP(I) = T
         END IF
         N = I
      END DO

   END SUBROUTINE ENQUEUE


   FUNCTION DEQUEUE(PQ) RESULT(RES)
      IMPLICIT NONE
      TYPE(PRIORITY_QUEUE_T), INTENT(INOUT) :: PQ
      TYPE(SEGMENT_T) :: RES, TMP

      INTEGER :: N, I, J

      N = PQ%NUMBER
      RES = PQ%HEAP(1)
      PQ%HEAP(1) = PQ%HEAP(N)
      PQ%NUMBER = PQ%NUMBER - 1

      I = 1
      DO WHILE (2*I < N)
         J = 2*I
         IF (J+1 < N .AND. PQ%HEAP(J+1)%MIN < PQ%HEAP(J)%MIN) J = J + 1
         IF (PQ%HEAP(J)%MIN < PQ%HEAP(I)%MIN) THEN
            TMP = PQ%HEAP(J)
            PQ%HEAP(J) = PQ%HEAP(I)
            PQ%HEAP(I) = TMP
         END IF
         I = J
      END DO

   END FUNCTION DEQUEUE


   SUBROUTINE CLEAR(PQ)
      IMPLICIT NONE
      TYPE(PRIORITY_QUEUE_T), INTENT(INOUT) :: PQ

      IF (ASSOCIATED(PQ%HEAP)) DEALLOCATE(PQ%HEAP)
      PQ%NUMBER = 0
   END SUBROUTINE


END MODULE ModLib_PQueue

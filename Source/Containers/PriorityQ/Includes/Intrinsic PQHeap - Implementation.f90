
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           Routines for PQHeap
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE PQHeap_CreateEmpty(PQ, InitCap, MinPQ, IncSize, Shrink)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create an empty priority queue.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQHeap),      INTENT(INOUT)   :: PQ       !! PQHeap object
    tIndex,             INTENT(IN)      :: InitCap  !! initial size of priority queue
    tLogical, OPTIONAL, INTENT(IN)      :: MinPQ
    !^ true if the priority queue to be implemented as a MinPQ; default -> a MaxPQ
    tIndex,   OPTIONAL, INTENT(IN)      :: IncSize  !! incremental size of priority queue if it is full
    tLogical, OPTIONAL, INTENT(IN)      :: Shrink
    !^ true if want to reduce capacity when size is less than a quarter of the capacity;
    !  default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Capacity

! FLOW
        
    ! first, check required input data
    IF (InitCap < 1_kIndex) THEN
        CALL Handle_ErrLevel('PQHeap_CreateEmpty', ModName, ErrWarning, &
                'Invalid InitCap (< 1).  Set the initial capacity of priority queue to 16.')
        Capacity = PQ%IncSize
    ELSE
        Capacity = InitCap
    END IF
        
    ! then, allocate space for the keys in the priority queue
    CALL MemAlloc(PQ%Keys, Capacity)
        
    ! finally, check optional input data
    IF (PRESENT(MinPQ))  PQ%Min = MinPQ
    IF (PRESENT(IncSize)) THEN
        IF (IncSize > 0_kIndex) PQ%IncSize = IncSize
    END IF
    IF (PRESENT(Shrink)) PQ%Shrink  =  Shrink
        
    RETURN

END SUBROUTINE PQHeap_CreateEmpty

!******************************************************************************

SUBROUTINE PQHeap_ConstructorByArray(PQ, N, Keys, MinPQ, IncSize, Shrink)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct a priority queue from an array of key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQHeap),      INTENT(INOUT)   :: PQ       !! PQHeap object
    tIndex,             INTENT(IN)      :: N        !! number of keys
    KeyType,            INTENT(IN)      :: Keys(N)  !! key array
    tLogical, OPTIONAL, INTENT(IN)      :: MinPQ
    !^ true if the priority queue to be implemented as a MinPQ; default -> a MaxPQ
    tIndex,   OPTIONAL, INTENT(IN)      :: IncSize  !! incremental size of priority queue if it is full
    tLogical, OPTIONAL, INTENT(IN)      :: Shrink
    !^ true if want to reduce capacity when size is less than a quarter of the capacity;
    !  default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, InitCap

! FLOW
        
    ! simply return if N is less than 1
    IF (N <= 0_kIndex) RETURN
        
    ! create empty priority queue
    IF (PRESENT(IncSize)) THEN
        InitCap = N + IncSize
    ELSE
        InitCap = N + PQ%IncSize
    END IF
    CALL PQ%CreateEmpty(InitCap, MinPQ, IncSize, Shrink)
        
    ! add input keys to the priority queue
    DO I = 1_kIndex, N
        CALL PQ%Insert(Keys(I))
    END DO
       
    RETURN

END SUBROUTINE PQHeap_ConstructorByArray

!******************************************************************************

SUBROUTINE PQHeap_Destructor(PQ, Keys)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct PQHeap object and get its keys if requested.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQHeap),                  INTENT(INOUT)   :: PQ       !! PQHeap object
    KeyType, ALLOCATABLE, OPTIONAL, INTENT(OUT)     :: Keys(:)  !! array of keys

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: N, ID
    tLogical    :: Success

! FLOW
        
    IF (.NOT.PQ%IsEmpty()) THEN
        IF (PRESENT(Keys)) THEN
            ! get keys (and free memory of components of keys if applicable)
            N = PQ%Last
            CALL MemAlloc(Keys, N)
            DO ID = 1_kIndex, N
                Success = PQ%Remove(Keys(ID))
            END DO
        END IF
    END IF
        
    ! reset components
    PQ%Last    = 0_kIndex
    PQ%IncSize = 10_kIndex
    PQ%Shrink  = FalseVal
    PQ%Min     = FalseVal
        
    ! free memory of priority queue keys
    IF (ALLOCATED(PQ%Keys)) DEALLOCATE(PQ%Keys)
       
    RETURN

END SUBROUTINE PQHeap_Destructor

!******************************************************************************

SUBROUTINE PQHeap_Finalizer(PQ)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(PQHeap), INTENT(INOUT) :: PQ   !! PQHeap object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! free up memory and reset components
    CALL PQ%Destruct()
       
    RETURN

END SUBROUTINE PQHeap_Finalizer

!******************************************************************************

SUBROUTINE PQHeap_InsertKey(PQ, NewKey)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a new key to the top (or bottom) of the priority queue.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQHeap), INTENT(INOUT)    :: PQ       !! PQHeap object
    KeyType,       INTENT(IN)       :: NewKey   !! new key to be added to the priority queue

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Capacity

! FLOW
        
    ! check capacity of the priority queue
    Capacity = SIZE(PQ%Keys)
    IF (PQ%Last == Capacity) THEN
        ! increase the priority queue capacity
        Capacity = Capacity + PQ%IncSize
        ! resize the priority queue
        CALL MemResize(PQ%Keys, Capacity)
    END IF
        
    ! increment the pointer
    PQ%Last = PQ%Last + 1_kIndex
        
    ! then, add new key to the priority queue
    PQ%Keys(PQ%Last) = NewKey
        
    ! restore heap order
    CALL ReHeapify_BottomUp(PQ, PQ%Last)

#ifdef DebugMode
    ! for debugging purpose
    IF (.NOT.IsHeapOrdered(PQ, 1_kIndex)) THEN
        CALL Handle_ErrLevel('PQHeap_InsertKey', ModName, ErrWarning, &
                             'The heap is NOT in order.')
    END IF
#endif

    RETURN

END SUBROUTINE PQHeap_InsertKey

!******************************************************************************

FUNCTION PQHeap_RemoveKey(PQ, HPKey) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve and remove the highest-priority key from the priority queue.  Also, return
    !  a flag indicating whether the key-value pair is successfully removed or not.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQHeap), INTENT(INOUT)    :: PQ      !! PQHeap object
    KeyType,       INTENT(OUT)      :: HPKey   !! the highest-priority key
    !> flag indicating whether the specified key and its associated
    !  value are successfully removed or not.
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Capacity
    KeyType     :: Temp

! FLOW
    
    ! first, check whether the priority queue is empty or not
    IF (PQ%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    ELSE
        Flag = TrueVal
    END IF
        
    ! get the highest-priority key
    HPKey = PQ%Keys(1)

    !--- remove the highest-priority key from the queue ---
            
    ! swap the highest-priority key with the last one
    EXCHANGE(PQ%Keys, 1_kIndex, PQ%Last)

    ! update pointer
    PQ%Last = PQ%Last - 1_kIndex
            
    ! restore heap order
    CALL ReHeapify_TopDown(PQ, 1_kIndex, PQ%Last)
        
    ! shrink capacity of the priority queue if necessary
    IF (PQ%Shrink) THEN
        Capacity = SIZE(PQ%Keys, KIND=kIndex)
        IF ((PQ%Last > 0_kIndex).AND.(PQ%Last == Capacity/4_kIndex)) THEN
            ! reduce the priority queue capacity
            Capacity = Capacity/2_kIndex
            ! resize the priority queue
            CALL MemResize(PQ%Keys, Capacity)
        END IF
    END IF
            
#ifdef DebugMode
    ! for debugging purpose
    IF (.NOT.IsHeapOrdered(PQ, 1_kIndex)) THEN
        CALL Handle_ErrLevel('PQHeap_RemoveKey', ModName, ErrWarning, &
                             'The heap is NOT in order.')
    END IF
#endif

    RETURN

END FUNCTION PQHeap_RemoveKey

!******************************************************************************

FUNCTION PQHeap_IsEmpty(PQ) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the priority queue is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQHeap), INTENT(IN)   :: PQ   !! PQHeap object
    tLogical                    :: Flag !! true if the priority queue is empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Flag = (PQ%Last == 0)

    RETURN

END FUNCTION PQHeap_IsEmpty

!******************************************************************************

FUNCTION PQHeap_GetSize(PQ) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get size of the priority queue.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQHeap), INTENT(IN)   :: PQ   !! PQHeap object
    tIndex                      :: Size !! size (number of keys)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    Size = PQ%Last

    RETURN

END FUNCTION PQHeap_GetSize

!******************************************************************************

FUNCTION PQHeap_PeekKey(PQ, HPKey) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve the highest-priority key from the priority queue.  Also, return
    !  a flag indicating whether the key-value pair is successfully removed or not.  <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQHeap), INTENT(INOUT)    :: PQ      !! PQHeap object
    KeyType,       INTENT(OUT)      :: HPKey   !! the highest-priority key
    !> flag indicating whether the specified key and its associated
    !  value are successfully removed or not.
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! check whether the priority queue is empty or not
    IF (PQ%IsEmpty()) THEN
        Flag = FalseVal
    ELSE
        Flag = TrueVal
        ! get the highest-priority key
        HPKey = PQ%Keys(1)
    END IF

    RETURN

END FUNCTION PQHeap_PeekKey

!******************************************************************************

SUBROUTINE ReHeapify_TopDown(PQ, Start, Bottom)

!** PURPOSE OF THIS SUBROUTINE
    !^ To restore heap order by sinking down

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQHeap), INTENT(INOUT)    :: PQ       !! PQHeap object
    tIndex,        INTENT(IN)       :: Start    !! starting index
    tIndex,        INTENT(IN)       :: Bottom   !! ending index
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, J
    KeyType     :: Temp
 
!** FLOW:
        
    Temp = PQ%Keys(Start)
    I    = Start
    J    = I + I
    ! do while j <= bottom
    IF (PQ%Min) THEN
        ! for MinPQ
        DO
            IF (J > Bottom) EXIT
            IF (J < Bottom) THEN
                ! compare to the better underling
                IF (PQ%Keys(J) > PQ%Keys(J+1)) THEN
                    J = J + 1
                END IF
            END IF
            ! found key's level. Terminate the sift-down.
            IF (PQ%Keys(J) >= Temp) EXIT
            ! otherwise, demote key and continue.
            PQ%Keys(I) = PQ%Keys(J)
            I = J
            J = I + I
        END DO
    ELSE
        ! for MaxPQ
        DO
            IF (J > Bottom) EXIT
            IF (J < Bottom) THEN
                ! compare to the better underling
                IF (PQ%Keys(J) < PQ%Keys(J+1)) THEN
                    J = J + 1
                END IF
            END IF
            ! found key's level. Terminate the sift-down.
            IF (PQ%Keys(J) <= Temp) EXIT
            ! otherwise, demote key and continue.
            PQ%Keys(I) = PQ%Keys(J)
            I = J
            J = I + I
        END DO
    END IF
    ! put key into its slot.
    PQ%Keys(I) = Temp

    RETURN

END SUBROUTINE ReHeapify_TopDown

!******************************************************************************

SUBROUTINE ReHeapify_BottomUp(PQ, Start)

!** PURPOSE OF THIS SUBROUTINE
    !^ To restore heap order by swimming up.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQHeap), INTENT(INOUT)    :: PQ       !! PQHeap object
    tIndex,        INTENT(IN)       :: Start    !! starting index
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, J
    KeyType     :: Temp
 
!** FLOW:
        
    Temp = PQ%Keys(Start)
    I    = Start
    J    = I/2
    ! do while k > 1 and key(k/2) < key(k)
    IF (PQ%Min) THEN
        ! for MinPQ
        DO WHILE ((I > 1).AND.(PQ%Keys(J) > Temp))
            ! promote key and continue.
            PQ%Keys(I) = PQ%Keys(J)
            I = J
            J = I/2
        END DO
    ELSE
        ! for MaxPQ
        DO WHILE ((I > 1).AND.(PQ%Keys(J) < Temp))
            ! promote key and continue.
            PQ%Keys(I) = PQ%Keys(J)
            I = J
            J = I/2
        END DO
    END IF
    ! put key into its slot.
    PQ%Keys(I) = Temp

    RETURN

END SUBROUTINE ReHeapify_BottomUp

!******************************************************************************

RECURSIVE FUNCTION IsHeapOrdered(PQ, Start) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE
    !^ To check whether the heap is in order or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PQHeap), INTENT(IN)   :: PQ       !! PQHeap object
    tIndex,        INTENT(IN)   :: Start    !! starting index
    tLogical                    :: Flag     !! true if the heap is in order
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Left, Right
 
!** FLOW:
        
    IF (Start > PQ%Last) THEN
        Flag = TrueVal
        RETURN
    END IF
        
    Left  = Start + Start
    Right = Left + 1
        
    IF (PQ%Min) THEN
        ! for MinPQ
        IF ((Left <= PQ%Last) .AND. (PQ%Keys(Start) > PQ%Keys(Left))) THEN
            Flag = FalseVal
            RETURN
        END IF
        IF ((Right <= PQ%Last) .AND. (PQ%Keys(Start) > PQ%Keys(Right))) THEN
            Flag = FalseVal
            RETURN
        END IF
    ELSE
        ! for MaxPQ
        IF ((Left <= PQ%Last) .AND. (PQ%Keys(Start) < PQ%Keys(Left))) THEN
            Flag = FalseVal
            RETURN
        END IF
        IF ((Right <= PQ%Last) .AND. (PQ%Keys(Start) < PQ%Keys(Right))) THEN
            Flag = FalseVal
            RETURN
        END IF
    END IF
        
    Flag = (IsHeapOrdered(PQ, Left) .AND. IsHeapOrdered(PQ, Right))

    RETURN

END FUNCTION IsHeapOrdered

!******************************************************************************

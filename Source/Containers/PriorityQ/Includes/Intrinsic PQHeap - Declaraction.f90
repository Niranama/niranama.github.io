
!** DERIVED TYPE DEFINITIONS
    !> The *PQHeap* type is a container type that employs a binary heap implementation
    !  to provide common operations for a priority queue.
    TYPE PQHeap
        PRIVATE
        !% pointer to last item of the priority queue
        tIndex                  :: Last = 0_kIndex
        !% incremental size of priority queue if it is full
        tIndex                  :: IncSize = 16_kIndex
        !% flag to shrink priority queue capacity
        tLogical                :: Shrink = FalseVal
        !> flag indicating whether the priority queue is implemented as
        !  a maximum PQ or a minimum PQ. <br>
        !  default -> a maximum PQ.
        tLogical                :: Min = FalseVal
        !% stored keys in the priority queue.
        KeyType, ALLOCATABLE    :: Keys(:)
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                      Public Procedures                    -----
        ! ---------------------------------------------------------------------
        ! -----     constructor and destructor procedures   -----
        ! -------------------------------------------------------
        !> **Type-Bound Subroutine**: CreateEmpty <br>
        !  **Purpose**:  To construct an empty priority queue. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%CreateEmpty(InitCap)                 ! use default options  <br>
        !   --->    CALL Table%CreateEmpty(32, MinPQ=.TRUE.)        ! use min-priority queue <br>
        !   --->    CALL Table%CreateEmpty(32, IncSize=16)          ! specify incremental size <br>
        !   --->    CALL Table%CreateEmpty(32, Shrink=.TRUE.)       ! specify shrinking <br>
        !   --->    CALL Table%CreateEmpty(32, .TRUE., 16, .TRUE.)  ! specify all options <br>
        PROCEDURE   :: CreateEmpty  => PQHeap_CreateEmpty
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**:  To construct a priority queue from the specified key arrays. <br>
        !  **Usage**: <br>
        !   ! use default options  <br>
        !   --->    CALL PQ%Construct(40, KeyArr) <br>
        !   ! specify all options (initial capacity is array size plus incremental size) <br>
        !   --->    CALL PQ%Construct(20, KeyArr, MinPQ, IncSize, Shrink) <br>
        PROCEDURE   :: Construct    => PQHeap_ConstructorByArray
        !> **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To remove all stored keys and free all memory currently used by the priority
        !       queue.  Optionally, stored keys can be retrieved. <br>
        !  **Usage**: <br>
        !   --->    CALL PQ%Destruct() <br>
        !   --->    CALL PQ%Destruct(StoredKeys) <br>
        PROCEDURE   :: Destruct     => PQHeap_Destructor
        ! -------------------------------------------------------
        ! -----         adding and removing procedures      -----
        ! -------------------------------------------------------
        !> **Type-Bound Subroutine**: Insert <br>
        !  **Purpose**:  To insert the specified key to the priority queue. <br>
        !  **Usage**: <br>
        !   --->    CALL PQ%Insert(Key) <br>
        PROCEDURE   :: Insert       => PQHeap_InsertKey
        !> **Type-Bound Function**: Remove <br>
        !  **Purpose**:  To retrieve and remove the highest-priority key from the priority queue.  Also,
        !       return a flag indicating whether the key-value pair is successfully removed or not.  <br>
        !  **Usage**: <br>
        !   --->    Flag = PQ%Remove(Key) <br>
        !   --->    IF (.NOT.PQ%Remove(Key)) DoSomething
        PROCEDURE   :: Remove       => PQHeap_RemoveKey
        ! -------------------------------------------------------
        ! -----               inquiry procedures            -----
        ! -------------------------------------------------------
        !> **Type-Bound Function**: IsEmpty <br>
        !  **Purpose**:  To check whether the priority queue is empty or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = PQ%IsEmpty() <br>
        !   --->    IF (.NOT.PQ%IsEmpty()) DoSomeThing
        PROCEDURE   :: IsEmpty      => PQHeap_IsEmpty
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get the current size (number of stored keys) of the priority queue. <br>
        !  **Usage**: <br>
        !   --->    Size = PQ%GetSize()
        PROCEDURE   :: GetSize      => PQHeap_GetSize
        !> **Type-Bound Function**: Peek <br>
        !  **Purpose**:  To retrieve the highest-priority key from the priority queue.  Also, return
        !       a flag indicating whether the key-value pair is successfully removed or not.  <br>
        !  **Usage**: <br>
        !   --->    Flag = PQ%Peek(Key) <br>
        !   --->    IF (.NOT.PQ%Peek(Key)) DoSomething
        PROCEDURE   :: Peek         => PQHeap_PeekKey
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: PQHeap_Finalizer
        ! ---------------------------------------------------------------------
    END TYPE PQHeap

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

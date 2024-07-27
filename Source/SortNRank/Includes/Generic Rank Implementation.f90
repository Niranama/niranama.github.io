
!** SUBMODULE PARAMETERS:
    ! general parameters for quick sort
    tInteger, PARAMETER :: Quick_Insert_CutOff_Low  = 40    ! cutoff to pair insertion
    tInteger, PARAMETER :: Quick_Insert_CutOff_High = 70    ! cutoff to pair insertion

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !> *Partition* is an interface for a partitioning procedure
        !  used by a quick-index routine.
        SUBROUTINE Partition(AVal,AInd,LStart,REnd,LEnd,RStart)
            IMPORT
            tArgument, INTENT(IN)       :: AVal(:)  !! array where its ranking to be determined
            tIndex,    INTENT(INOUT)    :: AInd(:)  !! indices indicating ranking of array elements
            tIndex,    INTENT(IN)       :: LStart   !! starting position of the left pointer
            tIndex,    INTENT(IN)       :: REnd     !! ending position of the right pointer
            tIndex,    INTENT(OUT)      :: LEnd     !! ending position of the left pointer
            tIndex,    INTENT(OUT)      :: RStart   !! starting position of the right pointer
        END SUBROUTINE Partition
    END INTERFACE

!** SUBMODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** SUBMODULE SUBROUTINES OR FUNCTIONS:

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                       COMMON AUXILIARY ROUTINES
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE FUNCTION Is_Array_Ranked(AVal, AInd) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE
   !^ To check whether the given array is ranked in a desired order.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)   :: AVal(:)          !! array values to be checked
    tIndex,    INTENT(IN)   :: AInd(SIZE(AVal)) !! indices of the array to be checked
    tLogical                :: Flag             !! true if the array is ranked

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, NA

!** FLOW:

    ! get array size
    NA = SIZE(AVal, KIND=kIndex)

    ! check NA
    IF (NA <= 1_kIndex) THEN
        Flag = TrueVal
        RETURN
    ELSE
        ! initialize
        Flag = FalseVal
    END IF

    ! check whether the array elements are in order
    DO I = 1_kIndex, NA-1_kIndex
        IF (COMPARE_GLT(AVal(AInd(I+1)), AVal(AInd(I)))) RETURN
    END DO

    ! the array elements are in order
    Flag = TrueVal

    RETURN

END FUNCTION

!**********************************************************************

FUNCTION MaximumDepth(N) RESULT(Depth)

!** PURPOSE OF THIS SUBROUTINE
    !^ To compute maximum depth of the recursion.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex, INTENT(IN)  :: N
    tIndex              :: Depth

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

   Depth = 2_kIndex*INT(LOG(REAL(N, KIND=kDouble))/LOG(2.0_kDouble), KIND=kIndex)

   RETURN

END FUNCTION MaximumDepth

!******************************************************************************

SUBROUTINE Rank_3_Items(AVal, AInd, I, J, K)

!** PURPOSE OF THIS SUBROUTINE
    ! To rank three elements A(I), A(J), A(K) so that
    !   1. for ascending order,  A(I) <= A(J) <= A(K)
    !   2. for descending order, A(I) >= A(J) >= A(K)
    ! Note: comments in the routine are for a desired order.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)  ! array values
    tIndex,    INTENT(INOUT)    :: AInd(:)  ! array indices
    tIndex,    INTENT(IN)       :: I, J, K

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Temp

!** FLOW:

    IF (COMPARE_GLT(AVal(AInd(K)), AVal(AInd(I)))) THEN
        IF (COMPARE_GLT(AVal(AInd(K)), AVal(AInd(J)))) THEN
            IF (COMPARE_GLT(AVal(AInd(J)), AVal(AInd(I)))) THEN
                EXCHANGE(AInd, I, K)
            ELSE
                Temp = AInd(J)
                AInd(J) = AInd(I)
                AInd(I) = AInd(K)
                AInd(K) = Temp
            END IF
        ELSE
            Temp = AInd(I)
            AInd(I) = AInd(J)
            AInd(J) = AInd(K)
            AInd(K) = Temp
        END IF
    ELSE
        IF (COMPARE_GLT(AVal(AInd(K)), AVal(AInd(J)))) THEN
            EXCHANGE(AInd, J, K)
        ELSE
            IF (COMPARE_GLT(AVal(AInd(J)), AVal(AInd(I)))) THEN
                EXCHANGE(AInd, I, J)
            END IF
        END IF
    END IF

    RETURN

END SUBROUTINE Rank_3_Items

!*********************************************************************

SUBROUTINE Rank_3_Items_Base0(AVal, AInd, I, J, K)

!** PURPOSE OF THIS SUBROUTINE
    ! To rank three elements A(I), A(J), A(K) where I, J, K are
    !   zero-based indices so that <br>
    !   1. for ascending order,  A(I) <= A(J) <= A(K)
    !   2. for descending order, A(I) >= A(J) >= A(K)
    ! Note: comments in the routine are for a desired order.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)     :: AVal(:)    ! array values
    tIndex,    INTENT(INOUT)  :: AInd(0:)   ! array indices
    tIndex,    INTENT(IN)     :: I, J, K

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Temp

!** FLOW:

    IF (COMPARE_GLT(AVal(AInd(K)), AVal(AInd(I)))) THEN
        IF (COMPARE_GLT(AVal(AInd(K)), AVal(AInd(J)))) THEN
            IF (COMPARE_GLT(AVal(AInd(J)), AVal(AInd(I)))) THEN
                EXCHANGE(AInd, I, K)
            ELSE
                Temp = AInd(J)
                AInd(J) = AInd(I)
                AInd(I) = AInd(K)
                AInd(K) = Temp
            END IF
        ELSE
            Temp = AInd(I)
            AInd(I) = AInd(J)
            AInd(J) = AInd(K)
            AInd(K) = Temp
        END IF
    ELSE
        IF (COMPARE_GLT(AVal(AInd(K)), AVal(AInd(J)))) THEN
            EXCHANGE(AInd, J, K)
        ELSE
            IF (COMPARE_GLT(AVal(AInd(J)), AVal(AInd(I)))) THEN
                EXCHANGE(AInd, I, J)
            END IF
        END IF
    END IF

    RETURN

END SUBROUTINE Rank_3_Items_Base0

!*********************************************************************

FUNCTION Median_Of_Three(AVal, AInd, I, J, K) RESULT(M)

!** PURPOSE OF THIS SUBROUTINE
    ! To return the (one-based) index of the median element among A(I), A(J), and A(K).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)  ! array values
    tIndex,    INTENT(INOUT)    :: AInd(:)  ! array indices
    tIndex,    INTENT(IN)       :: I, J, K
    tIndex                      :: M

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    IF (COMPARE_GLT(AVal(AInd(I)), AVal(AInd(J)))) THEN
        IF (COMPARE_GLT(AVal(AInd(J)), AVal(AInd(K)))) THEN
            M = J
        ELSE
            IF (COMPARE_GLT(AVal(AInd(I)), AVal(AInd(K)))) THEN
                M = K
            ELSE
                M = I
            END IF
        END IF
    ELSE
        IF (COMPARE_GLT(AVal(AInd(K)), AVal(AInd(J)))) THEN
            M = J
        ELSE
            IF (COMPARE_GLT(AVal(AInd(K)), AVal(AInd(I)))) THEN
                M = K
            ELSE
                M = I
            END IF
        END IF
    END IF

    RETURN

END FUNCTION Median_Of_Three

!*********************************************************************

SUBROUTINE DualInsert_Guarded(AVal, AInd, Lo, Hi)

!** PURPOSE OF THIS SUBROUTINE
	! To rank array using pair-insertion sort algorithm

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)  ! array values to be ranked
    tIndex,    INTENT(INOUT)    :: AInd(:)  ! array indices indicating the ranking
    tIndex,    INTENT(IN)       :: Lo       ! starting (one-based) index (inclusive)
    tIndex,    INTENT(IN)       :: Hi       ! ending (one-based) index (inclusive)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Index1, Index2, IndexL
    tIndex      :: LPtr, RPtr, KPtr

!** FLOW:

    LPtr = Lo
    RPtr = Hi
    DO
        IF (LPtr >= RPtr) RETURN
        LPtr = LPtr + 1_kIndex
        IF (COMPARE_GLT(AVal(AInd(LPtr)), AVal(AInd(LPtr-1)))) EXIT
    END DO
    KPtr = LPtr
    LPtr = LPtr + 1_kIndex
    DO WHILE (LPtr <= RPtr)
        IF (COMPARE_GLT(AVal(AInd(KPtr)), AVal(AInd(LPtr)))) THEN
            Index1 = AInd(LPtr)
            Index2 = AInd(KPtr)
        ELSE
            Index1 = AInd(KPtr)
            Index2 = AInd(LPtr)
        END IF
        KPtr = KPtr - 1_kIndex
        DO WHILE ((KPtr >= Lo).AND.(COMPARE_GLT(AVal(Index1), AVal(AInd(KPtr)))))
            AInd(KPtr+2) = AInd(KPtr)
            KPtr = KPtr - 1_kIndex
        END DO
        AInd(KPtr+2) = Index1
        DO WHILE ((KPtr >= Lo).AND.(COMPARE_GLT(AVal(Index2), AVal(AInd(KPtr)))))
            AInd(KPtr+1) = AInd(KPtr)
            KPtr = KPtr - 1_kIndex
        END DO
        AInd(KPtr+1) = Index2
        KPtr = LPtr + 1_kIndex
        LPtr = KPtr + 1_kIndex
    END DO
    IndexL = AInd(RPtr)
    RPtr = RPtr - 1_kIndex
    DO WHILE ((RPtr >= Lo).AND.(COMPARE_GLT(AVal(IndexL), AVal(AInd(RPtr)))))
        AInd(RPtr+1) = AInd(RPtr)
        RPtr = RPtr - 1_kIndex
    END DO
    AInd(RPtr+1) = IndexL

    RETURN

END SUBROUTINE DualInsert_Guarded

!*********************************************************************

SUBROUTINE DualInsert_UnGuarded(AVal, AInd, Lo, Hi)

!** PURPOSE OF THIS SUBROUTINE
	! To rank array using pair-insertion sort algorithm without checking
    ! whether the array index is out of bound or not

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)  ! array values to be ranked
    tIndex,    INTENT(INOUT)    :: AInd(:)  ! array indices indicating the ranking
    tIndex,    INTENT(IN)       :: Lo       ! starting (one-based) index (inclusive)
    tIndex,    INTENT(IN)       :: Hi       ! ending (one-based) index (inclusive)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Index1, Index2, IndexL
    tIndex      :: LPtr, RPtr, KPtr

!** FLOW:

    LPtr = Lo
    RPtr = Hi
    DO
        IF (LPtr >= RPtr) RETURN
        LPtr = LPtr + 1_kIndex
        IF (COMPARE_GLT(AVal(AInd(LPtr)), AVal(AInd(LPtr-1)))) EXIT
    END DO
    KPtr = LPtr
    LPtr = LPtr + 1_kIndex
    DO WHILE (LPtr <= RPtr)
        IF (COMPARE_GLT(AVal(AInd(KPtr)), AVal(AInd(LPtr)))) THEN
            Index1 = AInd(LPtr)
            Index2 = AInd(KPtr)
        ELSE
            Index1 = AInd(KPtr)
            Index2 = AInd(LPtr)
        END IF
        KPtr = KPtr - 1_kIndex
        DO WHILE (COMPARE_GLT(AVal(Index1), AVal(AInd(KPtr))))
            AInd(KPtr+2) = AInd(KPtr)
            KPtr = KPtr - 1_kIndex
        END DO
        AInd(KPtr+2) = Index1
        DO WHILE (COMPARE_GLT(AVal(Index2), AVal(AInd(KPtr))))
            AInd(KPtr+1) = AInd(KPtr)
            KPtr = KPtr - 1_kIndex
        END DO
        AInd(KPtr+1) = Index2
        KPtr = LPtr + 1_kIndex
        LPtr = KPtr + 1_kIndex
    END DO
    IndexL = AInd(RPtr)
    RPtr = RPtr - 1_kIndex
    DO WHILE (COMPARE_GLT(AVal(IndexL), AVal(AInd(RPtr))))
        AInd(RPtr+1) = AInd(RPtr)
        RPtr = RPtr - 1_kIndex
    END DO
    AInd(RPtr+1) = IndexL

    RETURN

END SUBROUTINE DualInsert_UnGuarded

!*********************************************************************

SUBROUTINE Reverse_Order_Base0(AInd, IStart, IEnd)

!** PURPOSE OF THIS SUBROUTINE
    ! To reverse order of a segment of an array in place

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex, INTENT(INOUT)   :: AInd(0:) ! array indices be reverse-ordered
    tIndex, INTENT(IN)      :: IStart   ! starting index (inclusive)
    tIndex, INTENT(IN)      :: IEnd     ! ending index (inclusive)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Temp
    tIndex      :: Lo
    tIndex      :: Hi

!** FLOW:

    Lo = IStart
    Hi = IEnd
    DO WHILE (Lo < Hi)
        EXCHANGE(AInd, Lo, Hi)
        Lo = Lo + 1_kIndex
        Hi = Hi - 1_kIndex
    END DO

    RETURN

END SUBROUTINE Reverse_Order_Base0

!*********************************************************************

SUBROUTINE Insert_Guarded(AVal, AInd, Lo, Hi)

!** PURPOSE OF THIS SUBROUTINE
	! To rank array using insertion sort algorithm

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)  ! array values
    tIndex,    INTENT(INOUT)    :: AInd(:)  ! array indices
    tIndex,    INTENT(IN)       :: Lo       ! lower index
    tIndex,    INTENT(IN)       :: Hi       ! upper index

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Temp
    tIndex      :: I, J, Nxch

!** FLOW:

    ! +++ Sedgewick's Optimized Version +++

    ! initialize
    Nxch = 0_kIndex

    ! put smallest element in position to serve as sentinel
    DO I = Hi, Lo+1_kIndex, -1_kIndex
        J = I - 1_kIndex
        IF (COMPARE_GLT(AVal(AInd(I)), AVal(AInd(J)))) THEN
            EXCHANGE(AInd, I, J)
            Nxch = Nxch + 1_kIndex
        END IF
    END DO
    IF (Nxch == 0_kIndex) RETURN

    ! insertion sort with half exchanges
    DO I = Lo+2_kIndex, Hi
        Temp = AInd(I)
        J = I - 1_kIndex
        DO WHILE (COMPARE_GLT(AVal(Temp), AVal(AInd(J))))
            AInd(J+1)  = AInd(J)
            J = J - 1_kIndex
        END DO
        AInd(J+1) = Temp
    END DO

    RETURN

END SUBROUTINE Insert_Guarded

!*********************************************************************

SUBROUTINE Insert_UnGuarded(AVal, AInd, Lo, Hi)

!** PURPOSE OF THIS SUBROUTINE
	! To rank array using insertion sort algorithm without checking
    ! whether the array index is out of bound or not

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)  ! array values
    tIndex,    INTENT(INOUT)    :: AInd(:)  ! array indices
    tIndex,    INTENT(IN)       :: Lo       ! lower index
    tIndex,    INTENT(IN)       :: Hi       ! upper index

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Temp
    tIndex      :: I, J

!** FLOW:

    ! +++ Rosetta Version +++

    DO I = Lo+1_kIndex, Hi
        J = I - 1_kIndex
        Temp = AInd(I)
        DO WHILE (COMPARE_GLT(AVal(Temp), AVal(AInd(J))))
            AInd(J+1) = AInd(J)
            J = J - 1_kIndex
        END DO
        AInd(J+1) = Temp
    END DO

    RETURN

END SUBROUTINE Insert_UnGuarded

!**********************************************************************

SUBROUTINE Index_Init(N, ID)

!** PURPOSE OF THIS SUBROUTINE
	! To initialize indices for array ranking

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex, INTENT(IN)      :: N
    tIndex, INTENT(INOUT)   :: ID(N)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW

    FORALL (I=1:N) ID(I) = I

    RETURN

END SUBROUTINE Index_Init

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                       WISE SORT ROUTINES
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE SUBROUTINE Wise_Rank_Unstable(AVal, AInd)

!** PURPOSE OF THIS SUBROUTINE
    ! To determine indices such that they sort the specified array into the desired
    ! order using the *WiseSort* algorithm.

!** TECHNICAL INFORMATION
    ! See explanation of the *WiseSort* algorithm in 'Wise_Sort_Unstable' subroutine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)          ! array values
    tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal)) ! array indices

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! parameters for Algo_QuickSort partitioning scheme
    tInteger, PARAMETER     :: PressScheme = 1   ! use Press's (median of three) partitioning scheme
    tInteger, PARAMETER     :: HoareScheme = 2   ! use Hoare's partitioning scheme
    ! parameters for sorting algorithm
    tInteger, PARAMETER     :: Algo_None      = 0    ! no algorithm needed (data already sorted)
    tInteger, PARAMETER     :: Algo_MergeRun  = 1    ! Java's merging run (for highly structure data, i.e. Saw)
    tInteger, PARAMETER     :: Algo_RustSort  = 2    ! Tim sort (for long first run - more than half)
    tInteger, PARAMETER     :: Algo_QuickSort = 3    ! Quick sort (for hardly structure data)
    ! parameters for cutoffs
    tInteger, PARAMETER     :: Insertion_CutOff_Low  = 40   ! cutoff to pair insertion sort
    tInteger, PARAMETER     :: Insertion_CutOff_Mid  = 50   ! cutoff to pair insertion sort
    tInteger, PARAMETER     :: Insertion_CutOff_High = 70   ! cutoff to pair insertion sort

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex, ALLOCATABLE :: Run(:)       ! info for Java's merge runs
    tIndex              :: RunCount     ! info for Java's merge runs
    tIndex              :: Dummy        ! dummy variable
    tIndex              :: QPScheme     ! quicksort partitioning scheme
    tIndex              :: LongRunSize  ! long run size used by Timsort
    tLogical            :: FirstRun     ! long run size used by Timsort
    tIndex              :: CutOff       ! cutoff to pair insertion sort
    tIndex              :: MaxDepth     ! maximum depth used to switch partitioning routine
    tIndex              :: AlgoFlag     ! flag for sorting algorithm
    tIndex              :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(AVal, KIND=kIndex)

    ! initialize the indices
    CALL Index_Init(NA, AInd)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether the array is small
    IF (NA <= Insertion_CutOff_Low) THEN
        ! for small array, use pair insertion sort algorithm
        CALL DualInsert_Guarded(AVal, AInd, 1_kIndex, NA)
        RETURN
    END IF

    ! inspect runs to determine sorting algorithm to be used
    CALL Expert_Inspection(AVal, AInd, NA, AlgoFlag, QPScheme, LongRunSize, FirstRun, Run, RunCount)

    ! perform sorting
    SELECT CASE (AlgoFlag)
    CASE (Algo_None)
        ! do nothing since the array is already sorted
    CASE (Algo_MergeRun)
        BLOCK
            tIndex  :: BInd(NA)
            ! perform Java's merge runs
            Dummy = Java_Merge_Runs_Rank(AVal, AInd, BInd, 0_kIndex, 1_kIndex, Run, 0_kIndex, RunCount)
        END BLOCK
    CASE (Algo_RustSort)
        BLOCK
            tIndex  :: BInd(NA/2)
            ! perform Rust's merge sort algorithm
            CALL Rust_Merge_Rank(AVal, AInd, NA, LongRunSize, FirstRun, BInd)
        END BLOCK
    CASE (Algo_QuickSort)
        ! set cutoff to pair insertion sort algorithm for small arrays
        IF (NA < 500_kIndex) THEN
            CutOff = Insertion_CutOff_Low
        ELSEIF (NA < 2000_kIndex) THEN
            CutOff = Insertion_CutOff_Mid
        ELSE
            CutOff = Insertion_CutOff_High
        END IF
        ! determine maximum depth based on the size of the input array
        MaxDepth = MaximumDepth(NA)
        ! perform quick sort algorithm
        SELECT CASE (QPScheme)
        CASE (PressScheme)
            CALL Quick_Mo3(AVal, AInd, NA, 1_kIndex, NA, MaxDepth)
        CASE (HoareScheme)
            CALL Quick_Hoare(AVal, AInd, NA, 1_kIndex, NA, MaxDepth)
        END SELECT
    END SELECT

    RETURN

CONTAINS

    SUBROUTINE Expert_Inspection(AVal, AInd, ASize, AlgoFlag, QPScheme, LongRunSize, FirstRun, Run, Count)

    !** PURPOSE OF THIS SUBROUTINE
        ! To carefully inspect whether there is any pattern in the given array
        ! and determine wisely an appropriate sorting algorithm for the array.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,           INTENT(IN)     :: AVal(:)      ! array values
        tIndex,              INTENT(INOUT)  :: AInd(0:)     ! array indices
        tIndex,              INTENT(IN)     :: ASize        ! size of the arrays
        tIndex,              INTENT(OUT)    :: AlgoFlag     ! flag indicating algorithm to be used
        tIndex,              INTENT(OUT)    :: QPScheme     ! flag indicating partitioning scheme
                                                            ! for quick sort algorithm
        tIndex,              INTENT(OUT)    :: LongRunSize  ! needed by Rust's Timsort algorithm
        tLogical,            INTENT(OUT)    :: FirstRun     ! needed by Rust's Timsort algorithm
        tIndex, ALLOCATABLE, INTENT(OUT)    :: Run(:)       ! needed by Java's merge-run algorithm
        tIndex,              INTENT(OUT)    :: Count        ! needed by Java's merge-run algorithm

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tInteger, PARAMETER     :: MIN_FIRST_RUN_SIZE    = 16   ! Min size of the first run to continue with scanning
        tInteger, PARAMETER     :: MIN_FIRST_RUNS_FACTOR = 7    ! Min factor for the first runs to continue scanning
        tInteger, PARAMETER     :: MAX_RUN_CAPACITY      = 128  ! Max capacity of the index array for tracking runs
        tInteger, PARAMETER     :: QUICK_SHORT_CUTOFF    = 256  ! Cutoff to quick sort algorithm
        tInteger, PARAMETER     :: QUICK_SMALL_CUTOFF    = 1024 ! Cutoff to quick sort algorithm

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: AKInd, Temp
        tIndex      :: RunSize, NewRunSize
        tIndex      :: I, J, K, II, JJ
        tIndex      :: Low, High, Last
        tIndex      :: Start, Finish
        tIndex      :: MinFirstRun, LongRunLimit, LongRunLimit2
        tIndex      :: FirstRunSize, LastRunSize, CurrRunSize
        tLogical    :: CurrDescend, LastDescend

    !** FLOW:

        ! Initialize working variables.
        Low = 0
        High = Low + ASize
        Count = 0
        Last = Low
        LongRunSize = 0
        FirstRun = TrueVal
        CurrDescend = FalseVal
        LastDescend = FalseVal
        QPScheme = PressScheme
        LongRunLimit = INT(0.50_kFP*REAL(ASize, KIND=kFP))
        LongRunLimit2 = INT(0.55_kFP*REAL(ASize, KIND=kFP))

        ! Deterimine minimum size of the first run to continue with scanning.
        MinFirstRun = MAX(ASize/MAX_RUN_CAPACITY, MIN_FIRST_RUN_SIZE)

        ! Identify all possible runs.
        K = Low + 1
        DO WHILE (K < High)

            ! Find the end index of the current run.
            IF (COMPARE_GLT(AVal(AInd(K-1)), AVal(AInd(K)))) THEN

                ! Identify ascending sequence.
                K = K + 1
                DO WHILE ((K < High).AND.(COMPARE_GLE(AVal(AInd(K-1)), AVal(AInd(K)))))
                    K = K + 1
                END DO
                CurrDescend = FalseVal

            ELSEIF (COMPARE_GLT(AVal(AInd(K)), AVal(AInd(K-1)))) THEN

                ! Identify descending sequence.
                K = K + 1
                DO WHILE ((K < High).AND.(COMPARE_GLE(AVal(AInd(K)), AVal(AInd(K-1)))))
                    K = K + 1
                END DO
                CurrDescend = TrueVal

            ELSE

                ! Identify constant sequence.
                AKInd = AInd(K)
                K = K + 1
                DO WHILE ((K < High).AND.(AVal(AKInd) == AVal(AInd(K))))
                    K = K + 1
                END DO

                IF (K < High) CYCLE

            END IF

            IF (Count == 0) THEN

                !++++++++++++++++++++++++++++++++++++++++++++++++++
                !+++    Check special cases for the first run   +++
                !++++++++++++++++++++++++++++++++++++++++++++++++++

                IF (K == High) THEN
                    ! -------------------------------------------------------------------
                    ! Special Cases:  Totally Ordered Array (Ascending/Descending Cases)
                    ! -------------------------------------------------------------------
                    ! The array is monotonous sequence, and therefore already sorted.
                    AlgoFlag = Algo_None
                    IF (CurrDescend) THEN
                        ! Reverse into ascending order.
                        I = Last
                        J = K - 1
                        CALL Reverse_Order_Base0(AInd, I, J)
                    END IF
                    RETURN
                END IF

                ! Compute first run size.
                FirstRunSize = K - Low

                ! Check whether the array has long first run (more than half of its size).
                IF (FirstRunSize > LongRunLimit) THEN
                    ! -------------------------------------------------------------
                    ! Special Cases:  Partially Ordered Array with Long First Run
                    !                 (Ascending/Descending Tail Random Cases)
                    ! -------------------------------------------------------------
                    IF ((FirstRunSize < LongRunLimit2).AND.(ASize <= QUICK_SMALL_CUTOFF)) THEN
                        ! For a small array with the first run size between 50% and 55% of ASize,
                        ! use quick sort algorithm with Press' scheme.
                        AlgoFlag = Algo_QuickSort
                        QPScheme = PressScheme
                    ELSEIF (ASize <= QUICK_SHORT_CUTOFF) THEN
                        ! For a small array with the first run size more than 50% of ASize,
                        ! use quick sort algorithm with Hoare' scheme.
                        AlgoFlag = Algo_QuickSort
                        QPScheme = HoareScheme
                    ELSE
                        ! For a large (not so small) array with long first run,
                        ! use Rust/Tim sort.
                        AlgoFlag = Algo_RustSort
                        LongRunSize = FirstRunSize
                        FirstRun = TrueVal
                        IF (CurrDescend) THEN
                            ! Reverse into ascending order.
                            I = Last
                            J = K - 1
                            CALL Reverse_Order_Base0(AInd, I, J)
                        END IF
                    END IF
                    RETURN
                END IF

                ! Check whether the array is small and its first run is NOT too long.
                IF ((ASize <= QUICK_SHORT_CUTOFF).AND.(FirstRunSize < LongRunLimit)) THEN
                    ! -------------------------------------------------------------
                    ! Special Cases:  Small Array with Not-Too-Long First Run
                    ! -------------------------------------------------------------
                    ! For a small array, use quick sort algorithm.
                    AlgoFlag = Algo_QuickSort
                    QPScheme = PressScheme
                    RETURN
                END IF

                ! Check whether the first run is smaller than or equal to its minimum.
                IF (FirstRunSize <= MinFirstRun) THEN
                    ! Check whether the first run is very short.
                    ! ------------------------------------------------------------
                    ! IMPORTANT NOTE: Very short runs indicate that those parts of
                    ! the array is mostly randomized.
                    ! ------------------------------------------------------------
                    IF (((ASize < 40000).AND.(FirstRunSize <= 4)).OR. &
                        ((ASize >= 40000).AND.(FirstRunSize < 8))) THEN

                        ! Compute the last run and check whether it is long enough.
                        Finish = High - 1
                        Start = Finish - 1
                        DO
                            IF (COMPARE_GLT(AVal(AInd(Start)), AVal(AInd(Start+1)))) THEN
                                ! Identify ascending sequence.
                                DO WHILE (Start > 0)
                                    IF (COMPARE_GLT(AVal(AInd(Start)), AVal(AInd(Start-1)))) EXIT
                                    Start = Start - 1
                                END DO
                                LastDescend = FalseVal
                            ELSEIF (COMPARE_GLT(AVal(AInd(Start+1)), AVal(AInd(Start)))) THEN
                                ! Identify descending sequence.
                                DO WHILE (Start > 0)
                                    IF (COMPARE_GLT(AVal(AInd(Start-1)), AVal(AInd(Start)))) EXIT
                                    Start = Start - 1
                                END DO
                                LastDescend = TrueVal
                            ELSE
                                ! Identify constant sequence.
                                AKInd = AInd(Start)
                                Start = Start - 1
                                DO WHILE ((Start > 0).AND.(AVal(AKInd) == AVal(AInd(Start))))
                                    Start = Start - 1
                                END DO
                                IF (Start > 0) CYCLE
                            END IF
                            EXIT
                        END DO

                        ! Compute last run size.
                        LastRunSize = Finish - Start

                        ! Check whether the last run is long enough.
                        IF (LastRunSize > LongRunLimit) THEN
                            ! -------------------------------------------------------------
                            ! Special Cases:  Partially Ordered Array with Long Last Run
                            !                 (Ascending/Descending Head Random Cases)
                            ! -------------------------------------------------------------
                            IF ((LastRunSize < LongRunLimit2).AND.(ASize <= QUICK_SMALL_CUTOFF)) THEN
                                ! For a small array with the last run size between 50% and 55% of ASize,
                                ! use quick sort algorithm with Press' scheme.
                                AlgoFlag = Algo_QuickSort
                                QPScheme = PressScheme
                            ELSEIF (ASize <= QUICK_SHORT_CUTOFF) THEN
                                ! For a small array with the last run size more than 50% of ASize,
                                ! use quick sort algorithm with Hoare' scheme.
                                AlgoFlag = Algo_QuickSort
                                QPScheme = HoareScheme
                            ELSE
                                ! For a large (not so small) array with long last run,
                                ! use Rust/Tim sort.
                                AlgoFlag = Algo_RustSort
                                LongRunSize = LastRunSize
                                FirstRun = FalseVal
                                IF (CurrDescend) THEN
                                    ! Reverse into ascending order.
                                    I = Last
                                    J = K - 1
                                    CALL Reverse_Order_Base0(AInd, I, J)
                                END IF
                                IF (LastDescend) THEN
                                    ! Reverse into ascending order.
                                    II = Start + 1
                                    JJ = Finish
                                    CALL Reverse_Order_Base0(AInd, II, JJ)
                                END IF
                            END IF
                        ! check whether it is half the size of the array
                        ELSEIF (LastRunSize == LongRunLimit) THEN
                            ! -----------------------------------------------------------------
                            ! Very Special Cases:  Partially Ordered Array with Long Last Run
                            !                      (Ascending/Descending Head Random Cases)
                            !                      (*** Last Run is Half the Array Size ***)
                            ! -----------------------------------------------------------------
                            IF (ASize <= QUICK_SMALL_CUTOFF) THEN
                                ! For a small array, use quick sort algorithm.
                                AlgoFlag = Algo_QuickSort
                                QPScheme = PressScheme
                            ELSE
                                ! For a large (not so small) array with long last run,
                                ! use Rust/Tim sort.
                                AlgoFlag = Algo_RustSort
                                LongRunSize = LastRunSize
                                FirstRun = FalseVal
                                IF (CurrDescend) THEN
                                    ! Reverse into ascending order.
                                    I = Last
                                    J = K - 1
                                    CALL Reverse_Order_Base0(AInd, I, J)
                                END IF
                                IF (LastDescend) THEN
                                    ! Reverse into ascending order.
                                    II = Start + 1
                                    JJ = Finish
                                    CALL Reverse_Order_Base0(AInd, II, JJ)
                                END IF
                            END IF
                        ! Check whether the first and last runs are very short.
                        ELSEIF ((FirstRunSize <= 4).AND.(LastRunSize <= 4)) THEN
                            ! ------------------------------------------
                            ! Special Cases:  Totally Randomized Array
                            ! ------------------------------------------
                            ! Both first run and last run are too short
                            ! so use quick sort with Press's scheme.
                            AlgoFlag = Algo_QuickSort
                            QPScheme = PressScheme
                        ELSE
                            ! --------------------------------------------
                            ! Special Cases:  Partially Randomized Array
                            ! --------------------------------------------
                            ! Both first run and last run are NOT too short
                            ! so use quick sort with Hoare's scheme.
                            AlgoFlag = Algo_QuickSort
                            QPScheme = HoareScheme
                        END IF
                    ELSE
                        ! --------------------------------------------
                        ! Special Cases:  Partially Randomized Array
                        ! --------------------------------------------
                        ! The first run is NOT very short
                        ! so use quick sort with Hoare's scheme.
                        AlgoFlag = Algo_QuickSort
                        QPScheme = HoareScheme
                    END IF
                    RETURN
                END IF

                ! -----------------------------------------------------------------------------
                ! The first run does not provide enough information so must continue scanning
                ! -----------------------------------------------------------------------------

                ! collect information about the run (which is needed by Java's merge runs)
                RunSize = MIN(IAND(IOR(SHIFTA(ASize, 10), 127), 1023), MAX_RUN_CAPACITY)
                CALL MemAlloc(Run, RunSize, StartID=0_kIndex)
                Run(Count) = Low
                Count = Count + 1

            ELSE

                !++++++++++++++++++++++++++++++++++++++++++++++++++++
                !+++    Check special cases for the current run   +++
                !++++++++++++++++++++++++++++++++++++++++++++++++++++

                IF (K < High) THEN

                    ! Compute current run size.
                    CurrRunSize = K - Last

                    IF ((FirstRunSize == LongRunLimit).AND.(CurrRunSize*2 < FirstRunSize)) THEN
                        ! -----------------------------------------------------------------
                        ! Very Special Cases:  Partially Ordered Array with Long First Run
                        !                      (Ascending/Descending Head Random Cases)
                        !                      (*** First Run is Half the Array Size ***)
                        ! -----------------------------------------------------------------
                        IF (ASize <= QUICK_SMALL_CUTOFF) THEN
                            ! For a small array (less than 1024 elements), use quick sort algorithm.
                            AlgoFlag = Algo_QuickSort
                            QPScheme = PressScheme
                        ELSE
                            ! For a large array (more than 1024 elements), use Rust/Tim sort.
                            AlgoFlag = Algo_RustSort
                            LongRunSize = FirstRunSize
                            FirstRun = TrueVal
                            IF (CurrDescend) THEN
                                ! Reverse into ascending order
                                I = Last
                                J = K - 1
                                CALL Reverse_Order_Base0(AInd, I, J)
                            END IF
                        END IF
                        RETURN
                    END IF

                    IF ((Count > SHIFTA((K-Low), MIN_FIRST_RUNS_FACTOR)).AND. &
                        (CurrRunSize < FirstRunSize)) THEN
                        ! --------------------------------------------
                        ! Special Cases:  Partially Randomized Array
                        ! --------------------------------------------
                        ! The first runs are not long enough to continue scanning
                        ! so use quick sort with Hoare's scheme.
                        AlgoFlag = Algo_QuickSort
                        QPScheme = HoareScheme
                        RETURN
                    END IF

                    ! Check whether the current run is very long (more than half of its size).
                    IF (CurrRunSize > LongRunLimit) THEN
                        ! ------------------------------------------------------------------
                        ! Special Cases:  Partially Ordered Array with a Very Long Run
                        !                 (Ascending/Descending Tail and Head Random Cases)
                        ! ------------------------------------------------------------------
                        IF ((CurrRunSize < LongRunLimit2).AND.(ASize <= QUICK_SMALL_CUTOFF)) THEN
                            ! For a small array with the long run size between 50% and 55% of ASize,
                            ! use quick sort algorithm with Press' scheme.
                            AlgoFlag = Algo_QuickSort
                            QPScheme = PressScheme
                        ELSEIF (ASize <= QUICK_SHORT_CUTOFF) THEN
                            ! For a small array with the long run size more than 50% of ASize,
                            ! use quick sort algorithm with Hoare' scheme.
                            AlgoFlag = Algo_QuickSort
                            QPScheme = HoareScheme
                        ELSE
                            ! For a large (not so small) array with long run in the middle,
                            ! use Rust/Tim sort.
                            AlgoFlag = Algo_RustSort
                            LongRunSize = FirstRunSize
                            FirstRun = TrueVal
                            ! Reverse into ascending order if necessary.
                            IF (CurrDescend) THEN
                                ! Reverse into ascending order
                                I = Last
                                J = K - 1
                                CALL Reverse_Order_Base0(AInd, I, J)
                            END IF
                        END IF
                        RETURN
                    END IF

                    IF (Count >= MAX_RUN_CAPACITY) THEN
                        ! --------------------------------------------
                        ! Special Cases:  Partially Randomized Array
                        ! --------------------------------------------
                        ! Array is not highly structured
                        ! so use quick sort with Hoare's scheme.
                        AlgoFlag = Algo_QuickSort
                        QPScheme = HoareScheme
                        RETURN
                    END IF

                END IF

                Count = Count + 1
                IF (Count == RunSize) THEN
                    ! Increase capacity of index array.
                    NewRunSize = SHIFTL(Count, 1)
                    CALL MemResize(Run, NewRunSize)
                    RunSize = NewRunSize
                END IF

            END IF

            IF (CurrDescend) THEN
                ! Reverse into ascending order.
                I = Last
                J = K - 1
                CALL Reverse_Order_Base0(AInd, I, J)
            END IF

            ! collect information about the run (which is needed by Java's merge runs)
            Last = K
            Run(Count) = Last
            K = K + 1

        END DO

        ! ------------------------------------------------------------------
        ! Special Cases:  Partially Ordered Array with Certain Patterns
        !                 (Ascending/Descending Saw or Wave-Like Cases)
        !                 This is most suited to rarely randomized arrays.
        ! ------------------------------------------------------------------
        ! The array is highly structured so Java's merge runs of highly structured array.
        AlgoFlag = Algo_MergeRun

        RETURN

    END SUBROUTINE Expert_Inspection

    !******************************************************************

    RECURSIVE SUBROUTINE Quick_Mo3(AVal, AInd, NE, LStart, REnd, MaxDepth)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To sort the array in the range [LStart,REnd] into an ascending order
        ! using recursive quicksort algorithm.  If the number of elements in
        ! the range below the cutoff, then a pair-insertion sorting routine
        ! is used instead.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(IN)       :: AVal(:)  ! array values
        tIndex,    INTENT(INOUT)    :: AInd(:)  ! array indices
        tIndex,    INTENT(IN)       :: NE       ! number of elements in the range
        tIndex,    INTENT(IN)       :: LStart   ! the start of the range
        tIndex,    INTENT(IN)       :: REnd     ! the end of the range
        tIndex,    INTENT(IN)       :: MaxDepth ! maximum depth used to switch partitioning routine

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: LEnd     ! the end of the left sub-array
        tIndex      :: RStart   ! the start of the right sub-array
        tIndex      :: Mid      ! middle index
        tIndex      :: NL       ! number of elements in the left partition
        tIndex      :: NR       ! number of elements in the right partition

    !** FLOW:

        ! check whether to perform pair insertion sort
        IF (NE <= CutOff) THEN
            ! check whether the specified range is the left most range of the given array
            IF (LStart == 1) THEN
                CALL DualInsert_Guarded(AVal, AInd, LStart, REnd)
            ELSE
                CALL DualInsert_UnGuarded(AVal, AInd, LStart, REnd)
            END IF
            RETURN
        END IF

        ! compute center index
        Mid = LStart + (REnd-LStart) / 2

        ! check which partitioning scheme to be used
        IF ((AVal(AInd(LStart)) /= AVal(AInd(Mid))).AND.(AVal(AInd(Mid)) /= AVal(AInd(REnd))) &
            .AND.(AVal(AInd(LStart)) /= AVal(AInd(REnd)))) THEN

            IF (MaxDepth > 0) THEN
                ! partition with median of three partitioning scheme
                CALL Partition_Mo3(AVal, AInd, LStart, REnd, Mid, LEnd, RStart)
            ELSE
                ! partition with median of medians (ninther) partitioning scheme
                CALL Partition_Ninther(AVal, AInd, NE, LStart, REnd, Mid, LEnd, RStart)
            END IF

        ELSE

            ! partition with Dutch's national flag algorithm
            CALL Partition_3_Ways(AVal, AInd, LStart, REnd, LEnd, RStart)

        END IF

        ! compute NL, NR and NLimit
        NL = LEnd - LStart + 1
        NR = REnd - RStart + 1

        ! perform quick sort algorithm on the two sub-arrays
        CALL Quick_Mo3(AVal, AInd, NL, LStart, LEnd, MaxDepth-1)
        CALL Quick_Mo3(AVal, AInd, NR, RStart, REnd, MaxDepth-1)

        RETURN

    END SUBROUTINE Quick_Mo3

    !******************************************************************

    RECURSIVE SUBROUTINE Quick_Hoare(AVal, AInd, NE, LStart, REnd, MaxDepth)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To sort the array in the range [LStart,REnd] into an ascending order
        ! using recursive quicksort algorithm.  If the number of elements in
        ! the range below the cutoff, then a pair-insertion sorting routine
        ! is used instead.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(IN)     :: AVal(:)  ! array values
        tIndex,    INTENT(INOUT)  :: AInd(:)  ! array indices
        tIndex,    INTENT(IN)     :: NE       ! number of elements in the range
        tIndex,    INTENT(IN)     :: LStart   ! the start of the range
        tIndex,    INTENT(IN)     :: REnd     ! the end of the range
        tIndex,    INTENT(IN)     :: MaxDepth ! maximum depth used to switch partitioning routine

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: LEnd     ! the end of the left sub-array
        tIndex      :: RStart   ! the start of the right sub-array
        tIndex      :: Mid      ! middle index
        tIndex      :: NL       ! number of elements in the left partition
        tIndex      :: NR       ! number of elements in the right partition

    !** FLOW:

        ! check whether to perform pair insertion sort
        IF (NE <= CutOff) THEN
            ! check whether the specified range is the left most range of the given array
            IF (LStart == 1) THEN
                CALL DualInsert_Guarded(AVal, AInd, LStart, REnd)
            ELSE
                CALL DualInsert_UnGuarded(AVal, AInd, LStart, REnd)
            END IF
            RETURN
        END IF

        ! compute center index
        Mid = LStart + (REnd-LStart) / 2

        ! check which partitioning scheme to be used
        IF ((AVal(AInd(LStart)) /= AVal(AInd(Mid))).AND.(AVal(AInd(Mid)) /= AVal(AInd(REnd))) &
            .AND.(AVal(AInd(LStart)) /= AVal(AInd(REnd)))) THEN

            IF (MaxDepth > 0) THEN
                ! partition with Hoare's partitioning scheme
                CALL Partition_Hoare(AVal, AInd, LStart, REnd, Mid, LEnd, RStart)
            ELSE
                ! partition with median of medians (ninther) partitioning scheme
                CALL Partition_Ninther(AVal, AInd, NE, LStart, REnd, Mid, LEnd, RStart)
            END IF

        ELSE

            ! partition with Dutch's national flag algorithm
            CALL Partition_3_Ways(AVal, AInd, LStart, REnd, LEnd, RStart)

        END IF

        ! compute NL, NR and NLimit
        NL = LEnd - LStart + 1
        NR = REnd - RStart + 1

        ! perform quick sort algorithm on the two sub-arrays
        CALL Quick_Hoare(AVal, AInd, NL, LStart, LEnd, MaxDepth-1)
        CALL Quick_Hoare(AVal, AInd, NR, RStart, REnd, MaxDepth-1)

        RETURN

    END SUBROUTINE Quick_Hoare

    !******************************************************************

    SUBROUTINE Partition_Mo3(AVal, AInd, LStart, REnd, Mid, LEnd, RStart)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To partition an array using Press's partitioning scheme.
        ! The given array in the range [LStart,REnd] is partitioned
        ! into two sub-arrays, Left and Right, such that all elements
        ! in the Left sub-array in the range [LStart,LEnd] are less
        ! than or equal to the selected pivot, and all elements in
        ! the Right sub-array in the range [RStart,REnd] are greater
        ! than or equal to the selected pivot.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(IN)       :: AVal(:)  ! array values
        tIndex,    INTENT(INOUT)    :: AInd(:)  ! array indices
        tIndex,    INTENT(IN)       :: LStart   ! on entry, the start of the given array
                                                ! on exit,  the start of the Left sub-array
        tIndex,    INTENT(IN)       :: REnd     ! on entry, the end of the given array
                                                ! on exit,  the end of the Right sub-array
        tIndex,    INTENT(IN)       :: Mid      ! the middle between LStart and REnd
        tIndex,    INTENT(OUT)      :: LEnd     ! the end of the Left sub-array
        tIndex,    INTENT(OUT)      :: RStart   ! the start of the Right sub-array

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: PInd, Temp
        tIndex      :: LPtr, UPtr

    !** FLOW:

        ! choose median of left, center, and right elements as partitioning element (the pivot)
        ! also, rearrange so that A(LStart) < A(LStart+1) < A(REnd)
        CALL Rank_3_Items(AVal, AInd, LStart, Mid, REnd)
        EXCHANGE(AInd, Mid, LStart+1)

        ! set pivot value
        PInd = AInd(LStart+1)

        ! set working pointers
        LPtr = LStart + 1
        UPtr = REnd

        DO
            ! increment the left pointer until we find an element
            ! that is greater than the pivot
            DO
                LPtr = LPtr + 1
                IF (COMPARE_GLE(AVal(PInd), AVal(AInd(LPtr)))) EXIT
            END DO

            ! decrement the right pointer until we find an element
            ! that is less than the pivot
            DO
                UPtr = UPtr - 1
                IF (COMPARE_GLE(AVal(AInd(UPtr)), AVal(PInd))) EXIT
            END DO

            ! exit if the pointers cross
            IF (UPtr < LPtr) EXIT

            ! swap AInd(LPtr) and AInd(UPtr)
            EXCHANGE(AInd, LPtr, UPtr)
        END DO

        ! insert partitioning element
        AInd(LStart+1) = AInd(UPtr)
        AInd(UPtr)     = PInd

        ! set output indices
        LEnd   = UPtr - 1
        RStart = LPtr

        RETURN

    END SUBROUTINE Partition_Mo3

    !******************************************************************

    SUBROUTINE Partition_Hoare(AVal, AInd, LStart, REnd, Mid, LEnd, RStart)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To partition an array using Hoare's partitioning scheme.
        ! The given array in the range [LStart,REnd] is partitioned
        ! into two sub-arrays, Left and Right, such that all elements
        ! in the Left sub-array in the range [LStart,LEnd] are less
        ! than or equal to the selected pivot, and all elements in
        ! the Right sub-array in the range [RStart,REnd] are greater
        ! than or equal to the selected pivot.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(IN)       :: AVal(:)  ! array values
        tIndex,    INTENT(INOUT)    :: AInd(:)  ! array indices
        tIndex,    INTENT(IN)       :: LStart   ! on entry, the start of the given array
                                                ! on exit,  the start of the Left sub-array
        tIndex,    INTENT(IN)       :: REnd     ! on entry, the end of the given array
                                                ! on exit,  the end of the Right sub-array
        tIndex,    INTENT(IN)       :: Mid      ! the middle between LStart and REnd
        tIndex,    INTENT(OUT)      :: LEnd     ! the end of the Left sub-array
        tIndex,    INTENT(OUT)      :: RStart   ! the start of the Right sub-array

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: PInd, Temp
        tIndex      :: LPtr, RPtr

    !** FLOW:

        ! Select the pivot
        PInd = AInd(FLOOR(REAL(Mid,KIND=kFP)))

        ! Perform partitioning.
        LPtr = LStart
        RPtr = REnd
        DO WHILE (LPtr <= RPtr)

            ! Increment the left pointer until we find an element
            ! that is greater than or equal to the pivot.
            DO WHILE (COMPARE_GLT(AVal(AInd(LPtr)), AVal(PInd)))
                LPtr = LPtr + 1
            END DO

            ! Decrement the right pointer until we find an element
            ! that is less than or equal to the pivot.
            DO WHILE (COMPARE_GLT(AVal(PInd), AVal(AInd(RPtr))))
                RPtr = RPtr - 1
            END DO

            ! A pair of values have been found where A(LPtr) is greater than the pivot
            ! and A(RPtr) is less than the pivot; thus, while the left pointer is less
            ! than or equal to the right pointer, swap A(LPtr) with A(RPtr).
            IF (LPtr <= RPtr) THEN
                EXCHANGE(AInd, LPtr, RPtr)
                LPtr = LPtr + 1
                RPtr = RPtr - 1
            END IF

        END DO

        ! Set output indices
        LEnd   = LPtr-1
        RStart = LPtr

        RETURN

    END SUBROUTINE Partition_Hoare

    !******************************************************************

    SUBROUTINE Partition_Ninther(AVal, AInd, NE, LStart, REnd, Mid, LEnd, RStart)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To partition an array using Press's partitioning scheme.
        ! The given array in the range [LStart,REnd] is partitioned
        ! into two sub-arrays, Left and Right, such that all elements
        ! in the Left sub-array in the range [LStart,LEnd] are less
        ! than or equal to the selected pivot, and all elements in
        ! the Right sub-array in the range [RStart,REnd] are greater
        ! than or equal to the selected pivot.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(IN)       :: AVal(:)  ! array values
        tIndex,    INTENT(INOUT)    :: AInd(:)  ! array indices
        tIndex,    INTENT(IN)       :: NE       ! number of array elements
        tIndex,    INTENT(IN)       :: LStart   ! on entry, the start of the given array
                                                ! on exit,  the start of the Left sub-array
        tIndex,    INTENT(IN)       :: REnd     ! on entry, the end of the given array
                                                ! on exit,  the end of the Right sub-array
        tIndex,    INTENT(IN)       :: Mid      ! the middle between LStart and REnd
        tIndex,    INTENT(OUT)      :: LEnd     ! the end of the Left sub-array
        tIndex,    INTENT(OUT)      :: RStart   ! the start of the Right sub-array

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: PInd, Temp
        tIndex      :: LPtr, UPtr
        tIndex      :: M1, M2, M3
        tIndex      :: Eps, Ninther

    !** FLOW:

        ! use Tukey's ninther as partitioning element (the pivot)
        ! also, rearrange so that A(LStart) < A(LStart+1) < A(REnd)
        Eps = NE/8
        M1 = LStart + Eps
        M2 = Mid
        M3 = REnd - Eps
        M1 = Median_Of_Three(AVal, AInd, LStart, M1, M1+Eps)
        M2 = Median_Of_Three(AVal, AInd, M2-Eps, M2, M2+Eps)
        M3 = Median_Of_Three(AVal, AInd, M3-Eps, M3, REnd)
        Ninther = Median_Of_Three(AVal, AInd, M1, M2, M3)
        IF ((Ninther == LStart).OR.(Ninther == REnd)) THEN
            CALL Rank_3_Items(AVal, AInd, LStart, Mid, REnd)
            EXCHANGE(AInd, Mid, LStart+1)
        ELSE
            CALL Rank_3_Items(AVal, AInd, LStart, Ninther, REnd)
            EXCHANGE(AInd, Ninther, LStart+1)
        END IF

        ! set pivot value
        PInd  = AInd(LStart+1)

        ! set working pointers
        LPtr  = LStart + 1
        UPtr = REnd

        DO
            ! increment the left pointer until we find an element
            ! that is greater than the pivot
            DO
                LPtr = LPtr + 1
                IF (COMPARE_GLE(AVal(PInd), AVal(AInd(LPtr)))) EXIT
            END DO

            ! decrement the right pointer until we find an element
            ! that is less than the pivot
            DO
                UPtr = UPtr - 1
                IF (COMPARE_GLE(AVal(AInd(UPtr)), AVal(PInd))) EXIT
            END DO

            ! exit if the pointers cross
            IF (UPtr < LPtr) EXIT

            ! swap AInd(LPtr) and AInd(UPtr)
            EXCHANGE(AInd,LPtr,UPtr)
        END DO

        ! insert partitioning element
        AInd(LStart+1) = AInd(UPtr)
        AInd(UPtr)     = PInd

        ! set output indices
        LEnd   = UPtr - 1
        RStart = LPtr

        RETURN

    END SUBROUTINE Partition_Ninther

    !******************************************************************

    SUBROUTINE Partition_3_Ways(AVal, AInd, LStart, REnd, LEnd, RStart)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To partition an array using 3-way partitioning scheme.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(IN)       :: AVal(:)  ! array values
        tIndex,    INTENT(INOUT)    :: AInd(0:) ! array indices
        tIndex,    INTENT(IN)       :: LStart   ! on entry, the start of the given array
                                                ! on exit,  the start of the Left sub-array
        tIndex,    INTENT(IN)       :: REnd     ! on entry, the end of the given array
                                                ! on exit,  the end of the Right sub-array
        tIndex,    INTENT(OUT)      :: LEnd     ! the end of the Left sub-array
        tIndex,    INTENT(OUT)      :: RStart   ! the start of the Right sub-array

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Low, High, iEnd, Size, Last
        tIndex      :: A3Ind, AKInd, Temp
        tIndex      :: PInd, PInd1, PInd2
        tIndex      :: Step
        tIndex      :: K, NL, NM, NR
        tIndex      :: E1, E2, E3, E4, E5
        tIndex      :: Lower, Upper

    !** FLOW:

        ! initialize
        Low  = LStart - 1
        High = REnd
        iEnd = High - 1
        Size = High - Low

        ! Use an inexpensive approximation of the golden ratio
        ! to select five sample elements and determine pivots.
        Step = SHIFTA(Size, 3) * 3 + 3

        ! Five elements around (and including) the central element
        ! will be used for pivot selection as described below. The
        ! unequal choice of spacing these elements was empirically
        ! determined to work well on a wide variety of inputs.
        E1 = Low + Step
        E5 = iEnd - Step
        E3 = SHIFTR((E1 + E5), 1)
        E2 = SHIFTR((E1 + E3), 1)
        E4 = SHIFTR((E3 + E5), 1)
        A3Ind = AInd(E3)

        ! Sort these elements in place by the combination
        ! of 4-element sorting network and insertion sort.
        !    5 ------o-----------o------------
        !            |           |
        !    4 ------|-----o-----o-----o------
        !            |     |           |
        !    2 ------o-----|-----o-----o------
        !                  |     |
        !    1 ------------o-----o------------
        IF (COMPARE_GLT(AVal(AInd(E5)), AVal(AInd(E2)))) THEN
            ! t = A(E5); A(E5) = A(E2); A(E2) = t
            EXCHANGE(AInd, E5, E2)
        END IF
        IF (COMPARE_GLT(AVal(AInd(E4)), AVal(AInd(E1)))) THEN
            ! t = A(E4); A(E4) = A(E1); A(E1) = t
            EXCHANGE(AInd, E4, E1)
        END IF
        IF (COMPARE_GLT(AVal(AInd(E5)), AVal(AInd(E4)))) THEN
            ! t = A(E5); A(E5) = A(E4); A(E4) = t
            EXCHANGE(AInd, E5, E4)
        END IF
        IF (COMPARE_GLT(AVal(AInd(E2)), AVal(AInd(E1)))) THEN
            ! t = A(E2); A(E2) = A(E1); A(E1) = t
            EXCHANGE(AInd, E2, E1)
        END IF
        IF (COMPARE_GLT(AVal(AInd(E4)), AVal(AInd(E2)))) THEN
            ! t = A(E4); A(E4) = A(E2); A(E2) = t
            EXCHANGE(AInd, E4, E2)
        END IF

        IF (COMPARE_GLT(AVal(A3Ind), AVal(AInd(E2)))) THEN
            IF (COMPARE_GLT(AVal(A3Ind), AVal(AInd(E1)))) THEN
                AInd(E3) = AInd(E2)
                AInd(E2) = AInd(E1)
                AInd(E1) = A3Ind
            ELSE
                AInd(E3) = AInd(E2)
                AInd(E2) = A3Ind
            END IF
        ELSEIF (COMPARE_GLT(AVal(AInd(E4)), AVal(A3Ind))) THEN
            IF (COMPARE_GLT(AVal(AInd(E5)), AVal(A3Ind))) THEN
                AInd(E3) = AInd(E4)
                AInd(E4) = AInd(E5)
                AInd(E5) = A3Ind
            ELSE
                AInd(E3) = AInd(E4)
                AInd(E4) = A3Ind
            END IF
        END IF

        ! Pointers
        Lower = Low     ! The index of the last element of the left part
        Upper = iEnd    ! The index of the first element of the right part

        !** Use single pivot in case of many equal elements ***

        ! Use the third of the five sorted elements as the pivot.
        ! This value is inexpensive approximation of the median.
        PInd = AInd(E3)

        ! The first element to be sorted is moved to the
        ! location formerly occupied by the pivot. After
        ! completion of partitioning the pivot is swapped
        ! back into its final position, and excluded from
        ! the next subsequent sorting.
        AInd(E3) = AInd(Lower)

        ! Traditional 3-way (Dutch National Flag) partitioning
        !
        !   left part                 central part    right part
        ! +------------------------------------------------------+
        ! |   < Pivot   |     ?     |   == Pivot   |   > Pivot   |
        ! +------------------------------------------------------+
        !              ^           ^                ^
        !              |           |                |
        !            Lower         K              Upper
        ! Invariants:
        !   all in (Low, Lower) < Pivot
        !   all in (K, Upper)  == Pivot
        !   all in [Upper, iEnd) > Pivot
        ! Pointer K is the last index of ?-part
        Upper = Upper + 1
        K = Upper - 1
        DO WHILE (K > Lower)
            AKInd = AInd(K)

            IF (AVal(AKInd) /= AVal(PInd)) THEN
                AInd(K) = PInd

                IF (COMPARE_GLT(AVal(AKInd), AVal(PInd))) THEN
                    ! Move A(K) to the left side
                    Lower = Lower + 1
                    DO WHILE (COMPARE_GLT(AVal(AInd(Lower)), AVal(PInd)))
                        Lower = Lower + 1
                    END DO

                    IF (COMPARE_GLT(AVal(PInd), AVal(AInd(Lower)))) THEN
                        Upper = Upper - 1
                        AInd(Upper) = AInd(Lower)
                    END IF
                    AInd(Lower) = AKInd
                ELSE
                    ! AK > Pivot - Move A(K) to the right side
                    Upper = Upper - 1
                    AInd(Upper) = AKInd
                END IF
            END IF
             K = K - 1
        END DO

        ! Swap the pivot into its final position.
        AInd(Low) = AInd(Lower)
        AInd(Lower) = PInd

        ! set output indices
        LEnd   = Lower
        RStart = Upper + 1

        RETURN

    END SUBROUTINE Partition_3_Ways

    !******************************************************************

END SUBROUTINE Wise_Rank_Unstable

!**********************************************************************

MODULE SUBROUTINE Wise_Rank_Stable(AVal, AInd)

!** PURPOSE OF THIS SUBROUTINE
    ! To determine indices such that they sort the specified array into the desired
    ! order using the *WiseSort-Stable* algorithm.

!** TECHNICAL INFORMATION
    ! See explanation of the *WiseSort-Stable* algorithm in 'Wise_Sort_Stable' subroutine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)          ! array values
    tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal)) ! array indices

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! parameters for sorting algorithm
    tInteger, PARAMETER     :: Algo_None       = 0  ! no algorithm needed (data already sorted)
    tInteger, PARAMETER     :: Algo_MergeRun   = 1  ! Java's merging run (for highly structure data)
    tInteger, PARAMETER     :: Algo_RustSort   = 2  ! Tim sort (for long first run - more than half)
    tInteger, PARAMETER     :: Algo_MergeQuad  = 3  ! Merge sort (for totally randomized data)
    tInteger, PARAMETER     :: Algo_MergeSort  = 4  ! Merge sort (for small arrays)
    tInteger, PARAMETER     :: Algo_HybridSort = 5  ! Hybrid (Quick-Merge) sort (for partly ordered data)
    ! parameters for cutoffs
    tInteger, PARAMETER     :: Insertion_CutOff_Init      = 40  ! initial cutoff to pair insertion sort
    tInteger, PARAMETER     :: QuickInsertion_CutOff_Low  = 75  ! cutoff to pair insertion sort for low N
    tInteger, PARAMETER     :: QuickInsertion_CutOff_High = 95  ! cutoff to pair insertion sort for high N
    tInteger, PARAMETER     :: MergeInsertion_CutOff_Low  = 25  ! cutoff to pair insertion sort
    tInteger, PARAMETER     :: MergeInsertion_CutOff_High = 55  ! cutoff to pair insertion sort
    tInteger, PARAMETER     :: MergeQuad_CutOff_Low       = 35  ! cutoff to pair insertion sort
    tInteger, PARAMETER     :: MergeQuad_CutOff_High      = 40  ! cutoff to pair insertion sort

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex, ALLOCATABLE :: Run(:)       ! info for Java's merge runs
    tIndex              :: RunCount     ! info for Java's merge runs
    tIndex              :: LongRunSize  ! long run size used by Timsort
    tLogical            :: FirstRun     ! long run size used by Timsort
    tIndex              :: QCutOff      ! cutoff to pair insertion sort for quick sort
    tIndex              :: MCutOff      ! cutoff to pair insertion sort for merge sort
    tIndex              :: AlgoFlag     ! flag for sorting algorithm
    tIndex              :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(AVal, KIND=kIndex)

    ! initialize the indices
    CALL Index_Init(NA, AInd)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether the array is small
    IF (NA <= Insertion_CutOff_Init) THEN
        ! for small array, use pair insertion sort algorithm
        CALL Insert_Guarded(AVal, AInd, 1_kIndex, NA)
        RETURN
    END IF

    ! inspect runs to determine sorting algorithm to be used
    CALL Expert_Inspection(AVal, AInd, NA, AlgoFlag, LongRunSize, FirstRun, Run, RunCount)

    ! perform sorting
    SELECT CASE (AlgoFlag)
    CASE (Algo_None)
        ! do nothing since the array is already sorted
    CASE (Algo_MergeRun)
        ! Note: Java's merge runs routine appears to be unstable.
!        IF (NA <= 40960) THEN
!            BLOCK
!                tIndex  :: BInd(NA)
!                ! perform Java's merge runs
!                Dummy = Java_Merge_Runs_Rank(AVal, AInd, BInd, 0_kIndex, 1_kIndex, Run, 0_kIndex, RunCount)
!            END BLOCK
!        ELSE
            CALL Rust_Rank(AVal, AInd)
!        END IF
    CASE (Algo_RustSort)
        BLOCK
            tIndex  :: BInd(NA/2)
            ! perform Rust's merge sort algorithm
            CALL Rust_Merge_Stable(AVal, AInd, NA, LongRunSize, FirstRun, BInd)
        END BLOCK
    CASE (Algo_MergeQuad)
        ! set cutoff to pair insertion sort algorithm for small arrays
        IF (NA < 2000_kIndex) THEN
            QCutOff = MergeQuad_CutOff_Low
        ELSE
            QCutOff = MergeQuad_CutOff_High
        END IF
        BLOCK
            tIndex  :: BInd(NA)
            ! perform merge sort
            CALL Quad_Split_Merge(AVal, AInd, 0_kIndex, NA, BInd, QCutOff)
        END BLOCK
    CASE (Algo_MergeSort)
        ! set cutoff depending on the size of the input array
        IF (NA < 2000_kIndex) THEN
            MCutOff = MergeInsertion_CutOff_Low
        ELSE
            MCutOff = MergeInsertion_CutOff_High
        END IF
        BLOCK
            tIndex  :: BInd(NA-NA/2)
            ! perform merge sort
            CALL MergeIndex_HalfCopy(AVal, AInd, 0_kIndex, NA, BInd)
        END BLOCK
    CASE (Algo_HybridSort)
        SELECT CASE (NA)
        CASE (:1000)
            ! set cutoff depending on the size of the input array
            MCutOff = MergeInsertion_CutOff_Low
            BLOCK
                tIndex  :: BInd(NA-NA/2)
                ! perform merge sort
                CALL MergeIndex_HalfCopy(AVal, AInd, 0_kIndex, NA, BInd)
            END BLOCK
        CASE (1001:100000)
            IF (NA < 2000_kIndex) THEN
                QCutOff = QuickInsertion_CutOff_Low
            ELSE
                QCutOff = QuickInsertion_CutOff_High
            END IF
            BLOCK
                tIndex  :: BInd(NA)
                ! perform quick-merge sort
                CALL Quick_Recur_Sort(AVal, AInd, BInd, 1_kIndex, NA)
            END BLOCK
        CASE (100001:)
            ! set cutoff depending on the size of the input array
            QCutOff = QuickInsertion_CutOff_High
            BLOCK
                tIndex  :: BInd(NA)
                ! perform quick-merge sort
                CALL Quick_Iter_Sort(AVal, AInd, BInd, 1_kIndex, NA)
            END BLOCK
        END SELECT
    END SELECT

    RETURN

CONTAINS

    SUBROUTINE Expert_Inspection(AVal, AInd, ASize, AlgoFlag, LongRunSize, FirstRun, Run, Count)

    !** PURPOSE OF THIS SUBROUTINE
        ! To carefully inspect whether there is any pattern in the given array
        ! and determine wisely an appropriate stable sorting algorithm for the array.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,           INTENT(IN)     :: AVal(:)      ! array values
        tIndex,              INTENT(INOUT)  :: AInd(0:)     ! array indices
        tIndex,              INTENT(IN)     :: ASize        ! size of the arrays
        tIndex,              INTENT(OUT)    :: AlgoFlag     ! flag indicating algorithm to be used
        tIndex,              INTENT(OUT)    :: LongRunSize  ! needed by Rust's Timsort algorithm
        tLogical,            INTENT(OUT)    :: FirstRun     ! needed by Rust's Timsort algorithm
        tIndex, ALLOCATABLE, INTENT(OUT)    :: Run(:)       ! needed by Java's merge-run algorithm
        tIndex,              INTENT(OUT)    :: Count        ! needed by Java's merge-run algorithm

    !** SUBROUTINE PARAMETER DECLARATIONS:
        ! parameters for expert inspection
        tInteger, PARAMETER     :: MIN_FIRST_RUN_SIZE    = 16   ! Min size of the first run to continue with scanning
        tInteger, PARAMETER     :: MIN_FIRST_RUNS_FACTOR = 7    ! Min factor for the first runs to continue scanning
        tInteger, PARAMETER     :: MAX_RUN_CAPACITY      = 128  ! Max capacity of the index array for tracking runs
        tInteger, PARAMETER     :: SMALL_CUTOFF          = 1024 ! Cutoff to merge sort algorithm

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex, ALLOCATABLE :: WorkSpace(:)
        tIndex              :: Temp, AKInd
        tIndex              :: RunSize, NewRunSize
        tIndex              :: Low, High, Last
        tIndex              :: I, J, K
        tIndex              :: II, JJ
        tIndex              :: KFirstEqual, TrueFirstDescendRunSize
        tIndex              :: KLastEqual, TrueLastDescendRunSize
        tIndex              :: Start, Finish
        tIndex              :: MinFirstRun
        tIndex              :: FirstRunSize, LastRunSize, CurrRunSize
        tIndex              :: AscendRunLimit, DescendRunLimit
        tLogical            :: CurrDescend, FirstDescend, LastDescend
        tLogical            :: EqualCase, ShortSecondRun, ReturnNow
        tLogical            :: FirstEqualCase, LastEqualCase, CurrEqualCase
        tIndex              :: ID(5)

    !** FLOW:

        ! Initialize working variables.
        Low = 0
        High = Low + ASize
        Count = 0
        Last = Low
        LongRunSize = 0
        KFirstEqual = High
        KLastEqual = Low
        FirstRun = TrueVal
        CurrDescend = FalseVal
        LastDescend = FalseVal
        EqualCase = FalseVal
        CurrEqualCase = FalseVal
        FirstEqualCase = FalseVal
        LastEqualCase = FalseVal
        ShortSecondRun = FalseVal
        AscendRunLimit  = INT(0.65_kFP*REAL(ASize, KIND=kFP))
        DescendRunLimit = INT(0.25_kFP*REAL(ASize, KIND=kFP))

        ! Determine minimum size of the first run to continue with scanning.
        MinFirstRun = MAX(ASize/MAX_RUN_CAPACITY, MIN_FIRST_RUN_SIZE)

        ! Identify all possible runs.
        K = Low + 1
        DO WHILE (K < High)

            ! Find the end index of the current run.
            IF (COMPARE_GLT(AVal(AInd(K-1)), AVal(AInd(K)))) THEN

                ! Identify ascending sequence.
                K = K + 1
                DO WHILE ((K < High).AND.(COMPARE_GLE(AVal(AInd(K-1)), AVal(AInd(K)))))
                    IF (AVal(AInd(K-1)) == AVal(AInd(K))) EqualCase = TrueVal
                    K = K + 1
                END DO
                CurrDescend = FalseVal

            ELSEIF (COMPARE_GLT(AVal(AInd(K)), AVal(AInd(K-1)))) THEN

                ! Identify descending sequence.
                K = K + 1
                DO WHILE ((K < High).AND.(COMPARE_GLE(AVal(AInd(K)), AVal(AInd(K-1)))))
                    IF (AVal(AInd(K-1)) == AVal(AInd(K))) THEN
                        EqualCase = TrueVal
                        IF (KFirstEqual == High) KFirstEqual = K
                    END IF
                    K = K + 1
                END DO
                CurrDescend = TrueVal

            ELSE

                EqualCase = TrueVal
                IF (KFirstEqual == High) KFirstEqual = K
                ! Identify constant sequence.
                AKInd = AInd(K)
                K = K + 1
                DO WHILE ((K < High).AND.(AVal(AKInd) == AVal(AInd(K))))
                    K = K + 1
                END DO

                IF (K < High) CYCLE

            END IF

            ! Compute current run size.
            CurrRunSize = K - Last

            IF (Count == 0) THEN

                !++++++++++++++++++++++++++++++++++++++++++++++++++
                !+++    Check special cases for the first run   +++
                !++++++++++++++++++++++++++++++++++++++++++++++++++

                ! Compute first run size.
                FirstRunSize = CurrRunSize
                FirstDescend = CurrDescend
                FirstEqualCase = EqualCase
                CurrEqualCase = EqualCase

                IF (FirstEqualCase) THEN
                    TrueFirstDescendRunSize = KFirstEqual - Low
                END IF

                IF (K == High) THEN
                    ! -------------------------------------------------------------------
                    ! Special Cases:  Totally Ordered Array (Ascending/Descending Cases)
                    ! -------------------------------------------------------------------
                    ! The array is monotonous sequence, and therefore already sorted.
                    AlgoFlag = Algo_None
                    IF (FirstDescend) THEN
                        IF (FirstEqualCase) THEN
                            ! Reverse into ascending order.
                            I = Last
                            J = KFirstEqual - 1
                            CALL Reverse_Order_Base0(AInd, I, J)
                            ! Allocate a buffer to use as scratch memory.
                            ALLOCATE(WorkSpace(1:FirstRunSize/2))
                            ! perform Rust's merge sort algorithm
                            CALL Rust_Merge_Stable(AVal, AInd, FirstRunSize, TrueFirstDescendRunSize, TrueVal, WorkSpace)
                            DEALLOCATE(WorkSpace)
                        ELSE
                            ! Reverse into ascending order.
                            I = Last
                            J = K - 1
                            CALL Reverse_Order_Base0(AInd, I, J)
                        END IF
                    END IF
                    RETURN
                END IF

                ! Check whether the array has long first run.
                IF (((FirstRunSize >= AscendRunLimit).AND.(.NOT.FirstDescend)).OR. &
                    ((FirstRunSize >= DescendRunLimit).AND.FirstDescend))THEN
                    ! -------------------------------------------------------------
                    ! Special Cases:  Partially Ordered Array with Long First Run
                    !                 (Ascending/Descending Tail Random Cases)
                    ! -------------------------------------------------------------
                    ! For an array with long first run, use Rust/Tim sort.
                    AlgoFlag = Algo_RustSort
                    LongRunSize = FirstRunSize
                    FirstRun = TrueVal
                    IF ((((FirstRunSize == AscendRunLimit).AND.(.NOT.FirstDescend)).OR. &
                        ((FirstRunSize == DescendRunLimit).AND.FirstDescend)).AND.      &
                        (ASize <= 2000)) THEN
                        ! Compute the last run and check whether it is long enough.
                        Finish = High - 1
                        Start = Finish - 1
                        DO
                            IF (COMPARE_GLT(AVal(AInd(Start)), AVal(AInd(Start+1)))) THEN
                                ! Identify ascending sequence.
                                DO WHILE (Start > 0)
                                    IF (AVal(AInd(Start)) == AVal(AInd(Start-1))) THEN
                                        EqualCase = TrueVal
                                        LastEqualCase = TrueVal
                                    END IF
                                    IF (COMPARE_GLT(AVal(AInd(Start)), AVal(AInd(Start-1)))) EXIT
                                    Start = Start - 1
                                END DO
                                LastDescend = FalseVal
                            ELSEIF (COMPARE_GLT(AVal(AInd(Start+1)), AVal(AInd(Start)))) THEN
                                ! Identify descending sequence.
                                DO WHILE (Start > 0)
                                    IF (AVal(AInd(Start)) == AVal(AInd(Start-1))) THEN
                                        EqualCase = TrueVal
                                        LastEqualCase = TrueVal
                                        IF (KLastEqual == Low) KLastEqual = Start
                                    END IF
                                    IF (COMPARE_GLT(AVal(AInd(Start-1)), AVal(AInd(Start)))) EXIT
                                    Start = Start - 1
                                END DO
                                LastDescend = TrueVal
                            ELSE
                                EqualCase = TrueVal
                                LastEqualCase = TrueVal
                                IF (KLastEqual == Low) KLastEqual = Start
                                ! Identify constant sequence.
                                AKInd = AInd(Start)
                                Start = Start - 1
                                DO WHILE ((Start > 0).AND.(AVal(AKInd)) == AVal(AInd(Start)))
                                    Start = Start - 1
                                END DO
                                IF (Start > 0) CYCLE
                            END IF
                            EXIT
                        END DO
                        ! Compute last run size.
                        LastRunSize = Finish - Start
                        IF (LastEqualCase) THEN
                            TrueLastDescendRunSize = Finish - KLastEqual
                        END IF

                        IF (LastRunSize*2 < FirstRunSize) THEN
                            ! use merge sort for small arrays.
                            AlgoFlag = Algo_MergeSort
                        END IF
                        IF (LastDescend) THEN
                            IF (LastEqualCase) THEN
                                ! Reverse into ascending order.
                                II = KLastEqual + 1
                                JJ = Finish
                                CALL Reverse_Order_Base0(AInd, II, JJ)
                                ! Allocate a buffer to use as scratch memory.
                                ALLOCATE(WorkSpace(1:LastRunSize/2))
                                ! perform Rust's merge sort algorithm
                                CALL Rust_Merge_Stable(AVal, AInd(Start+1:Finish), LastRunSize, TrueLastDescendRunSize, FalseVal, WorkSpace)
                                DEALLOCATE(WorkSpace)
                            ELSE
                                ! Reverse into ascending order.
                                II = Start + 1
                                JJ = Finish
                                CALL Reverse_Order_Base0(AInd, II, JJ)
                            END IF
                        END IF
                    END IF
                    IF (FirstDescend) THEN
                        IF (FirstEqualCase) THEN
                            ! Reverse into ascending order.
                            I = Last
                            J = KFirstEqual - 1
                            CALL Reverse_Order_Base0(AInd, I, J)
                            ! Allocate a buffer to use as scratch memory.
                            ALLOCATE(WorkSpace(1:FirstRunSize/2))
                            ! perform Rust's merge sort algorithm
                            CALL Rust_Merge_Stable(AVal, AInd, FirstRunSize, TrueFirstDescendRunSize, TrueVal, WorkSpace)
                            DEALLOCATE(WorkSpace)
                        ELSE
                            ! Reverse into ascending order.
                            I = Last
                            J = K - 1
                            CALL Reverse_Order_Base0(AInd, I, J)
                        END IF
                    END IF
                    RETURN
                END IF

                ! Check whether the first run is very short.
                ! ------------------------------------------------------------
                ! IMPORTANT NOTE: Very short runs indicate that those parts of
                ! the array is mostly randomized.
                ! ------------------------------------------------------------
                IF (((ASize < 40000).AND.(FirstRunSize <= 4)).OR. &
                    ((ASize >= 40000).AND.(FirstRunSize < 8))) THEN

                    ! Compute the last run and check whether it is long enough.
                    Finish = High - 1
                    Start = Finish - 1
                    DO
                        IF (COMPARE_GLT(AVal(AInd(Start)), AVal(AInd(Start+1)))) THEN
                            ! Identify ascending sequence.
                            DO WHILE (Start > 0)
                                IF (AVal(AInd(Start)) == AVal(AInd(Start-1))) THEN
                                    EqualCase = TrueVal
                                    LastEqualCase = TrueVal
                                END IF
                                IF (COMPARE_GLT(AVal(AInd(Start)), AVal(AInd(Start-1)))) EXIT
                                Start = Start - 1
                            END DO
                            LastDescend = FalseVal
                        ELSEIF (COMPARE_GLT(AVal(AInd(Start+1)), AVal(AInd(Start)))) THEN
                            ! Identify descending sequence.
                            DO WHILE (Start > 0)
                                IF (AVal(AInd(Start)) == AVal(AInd(Start-1))) THEN
                                    EqualCase = TrueVal
                                    LastEqualCase = TrueVal
                                    IF (KLastEqual == Low) KLastEqual = Start
                                END IF
                                IF (COMPARE_GLT(AVal(AInd(Start-1)), AVal(AInd(Start)))) EXIT
                                Start = Start - 1
                            END DO
                            LastDescend = TrueVal
                        ELSE
                            EqualCase = TrueVal
                            LastEqualCase = TrueVal
                            IF (KLastEqual == Low) KLastEqual = Start
                            ! Identify constant sequence.
                            AKInd = AInd(Start)
                            Start = Start - 1
                            DO WHILE ((Start > 0).AND.(AVal(AKInd) == AVal(AInd(Start))))
                                Start = Start - 1
                            END DO
                            IF (Start > 0) CYCLE
                        END IF
                        EXIT
                    END DO
                    ! Compute last run size.
                    LastRunSize = Finish - Start
                    IF (LastEqualCase) THEN
                        TrueLastDescendRunSize = Finish - KLastEqual
                    END IF

                    ! Check whether the last run is long enough.
                    IF (((LastRunSize >= AscendRunLimit).AND.(.NOT.LastDescend)).OR. &
                        ((LastRunSize >= DescendRunLimit).AND.LastDescend))THEN
                        ! -------------------------------------------------------------
                        ! Special Cases:  Partially Ordered Array with Long Last Run
                        !                 (Ascending/Descending Head Random Cases)
                        ! -------------------------------------------------------------
                        ! For a large (not so small) array with long last run,
                        ! use Rust/Tim sort.
                        AlgoFlag = Algo_RustSort
                        LongRunSize = LastRunSize
                        FirstRun = FalseVal
                        IF ((((LastRunSize == AscendRunLimit).AND.(.NOT.LastDescend)).OR. &
                            ((LastRunSize == DescendRunLimit).AND.LastDescend)).AND.      &
                            (ASize <= 2000).AND.(FirstRunSize*2 < LastRunSize)) THEN
                            ! use merge sort for small arrays.
                            AlgoFlag = Algo_MergeSort
                        END IF
                        IF (FirstDescend) THEN
                            IF (FirstEqualCase) THEN
                                ! Reverse into ascending order.
                                I = Last
                                J = KFirstEqual - 1
                                CALL Reverse_Order_Base0(AInd, I, J)
                                ! Allocate a buffer to use as scratch memory.
                                ALLOCATE(WorkSpace(1:FirstRunSize/2))
                                ! perform Rust's merge sort algorithm
                                CALL Rust_Merge_Stable(AVal, AInd, FirstRunSize, TrueFirstDescendRunSize, TrueVal, WorkSpace)
                                DEALLOCATE(WorkSpace)
                            ELSE
                                ! Reverse into ascending order.
                                I = Last
                                J = K - 1
                                CALL Reverse_Order_Base0(AInd, I, J)
                            END IF
                        END IF
                        IF (LastDescend) THEN
                            IF (LastEqualCase) THEN
                                ! Reverse into ascending order.
                                II = KLastEqual + 1
                                JJ = Finish
                                CALL Reverse_Order_Base0(AInd, II, JJ)
                                ! Allocate a buffer to use as scratch memory.
                                ALLOCATE(WorkSpace(1:LastRunSize/2))
                                ! perform Rust's merge sort algorithm
                                CALL Rust_Merge_Stable(AVal, AInd(Start+1:Finish), LastRunSize, TrueLastDescendRunSize, FalseVal, WorkSpace)
                                DEALLOCATE(WorkSpace)
                            ELSE
                                ! Reverse into ascending order.
                                II = Start + 1
                                JJ = Finish
                                CALL Reverse_Order_Base0(AInd, II, JJ)
                            END IF
                        END IF
                    ! Check whether the first and last runs are very short.
                    ELSEIF ((FirstRunSize <= 4).AND.(LastRunSize <= 4)) THEN
                        ! ------------------------------------------
                        ! Special Cases:  Totally Randomized Array
                        ! ------------------------------------------
                        ! Both first run and last run are too short
                        IF (EqualCase) THEN
                            ! so use quick sort for equal cases.
                            !AlgoFlag = Algo_QuickSort
                            AlgoFlag = Algo_HybridSort
                        ELSE
                            IF (ASize <= 10000) THEN
                                ID(1) = Low
                                ID(2) = ASize/4
                                ID(3) = ID(2) + ID(2)
                                ID(4) = ID(2) + ID(3)
                                ID(5) = ASize - 1
                                OUTLOOP: DO I = 1, 4
                                    DO J = I+1, 5
                                        IF (AVal(AInd(I)) == AVal(AInd(J))) THEN
                                            EqualCase = TrueVal
                                            EXIT OUTLOOP
                                        END IF
                                    END DO
                                END DO OUTLOOP
                                IF (EqualCase) THEN
                                    ! so use quick sort for equal cases.
                                    !AlgoFlag = Algo_QuickSort
                                    AlgoFlag = Algo_HybridSort
                                ELSE
                                    ! so use merge sort for small arrays.
                                    AlgoFlag = Algo_MergeSort
                                END IF
                            ELSE
                                ! so use quick sort for large arrays.
                                !AlgoFlag = Algo_QuickSort
                                AlgoFlag = Algo_MergeQuad
                            END IF
                        END IF
                    ELSEIF (LastRunSize > MinFirstRun) THEN
                        ! -------------------------------------------------------------
                        ! Special Cases:  Partially Randomized Array
                        ! -------------------------------------------------------------
                        ! For an array with not-too-short last run and very short first run,
                        IF (FirstDescend.AND.LastDescend) THEN
                            ! use Rust/Tim sort for descending cases.
                            AlgoFlag = Algo_RustSort
                            LongRunSize = LastRunSize
                            FirstRun = FalseVal
                        ELSE
                            ! use merge sort algorithm.
                            AlgoFlag = Algo_MergeSort
                        END IF
                        IF (LastDescend) THEN
                            IF (LastEqualCase) THEN
                                ! Reverse into ascending order.
                                II = KLastEqual + 1
                                JJ = Finish
                                CALL Reverse_Order_Base0(AInd, II, JJ)
                                ! Allocate a buffer to use as scratch memory.
                                ALLOCATE(WorkSpace(1:LastRunSize/2))
                                ! perform Rust's merge sort algorithm
                                CALL Rust_Merge_Stable(AVal, AInd(Start+1:Finish), LastRunSize, TrueLastDescendRunSize, FalseVal, WorkSpace)
                                DEALLOCATE(WorkSpace)
                            ELSE
                                ! Reverse into ascending order.
                                II = Start + 1
                                JJ = Finish
                                CALL Reverse_Order_Base0(AInd, II, JJ)
                            END IF
                        END IF
                        IF (FirstDescend) THEN
                            IF (FirstEqualCase) THEN
                                ! Reverse into ascending order.
                                I = Last
                                J = KFirstEqual - 1
                                CALL Reverse_Order_Base0(AInd, I, J)
                                ! Allocate a buffer to use as scratch memory.
                                ALLOCATE(WorkSpace(1:FirstRunSize/2))
                                ! perform Rust's merge sort algorithm
                                CALL Rust_Merge_Stable(AVal, AInd, FirstRunSize, TrueFirstDescendRunSize, TrueVal, WorkSpace)
                                DEALLOCATE(WorkSpace)
                            ELSE
                                ! Reverse into ascending order.
                                I = Last
                                J = K - 1
                                CALL Reverse_Order_Base0(AInd, I, J)
                            END IF
                        END IF
                    ELSE
                        ! --------------------------------------------
                        ! Special Cases:  Partially Randomized Array
                        ! --------------------------------------------
                        ! Both first run and last run are NOT too short
                        IF (EqualCase) THEN
                            ! so use quick sort for equal cases.
                            !AlgoFlag = Algo_QuickSort
                            AlgoFlag = Algo_HybridSort
                        ELSE
                            ! so use hybrid sort in general.
                            AlgoFlag = Algo_HybridSort
                            IF (((ASize < 40000).AND.(LastRunSize > 4)).OR. &
                                ((ASize >= 40000).AND.(LastRunSize >= 8))) THEN
                                ! However, the last run is somewhat long enough to use Rust/Time sort.
                                ! If descending cases, so use Rust/Tim sort.
                                IF (FirstRunSize >= LastRunSize) THEN
                                    IF (FirstDescend) THEN
                                        AlgoFlag = Algo_RustSort
                                        LongRunSize = FirstRunSize
                                        FirstRun = TrueVal
                                    END IF
                                ELSE
                                    IF (LastDescend) THEN
                                        AlgoFlag = Algo_RustSort
                                        LongRunSize = LastRunSize
                                        FirstRun = FalseVal
                                    END IF
                                END IF
                            END IF
                        END IF
                        IF (FirstDescend) THEN
                            IF (FirstEqualCase) THEN
                                ! Reverse into ascending order.
                                I = Last
                                J = KFirstEqual - 1
                                CALL Reverse_Order_Base0(AInd, I, J)
                                ! Allocate a buffer to use as scratch memory.
                                ALLOCATE(WorkSpace(1:FirstRunSize/2))
                                ! perform Rust's merge sort algorithm
                                CALL Rust_Merge_Stable(AVal, AInd, FirstRunSize, TrueFirstDescendRunSize, TrueVal, WorkSpace)
                                DEALLOCATE(WorkSpace)
                            ELSE
                                ! Reverse into ascending order.
                                I = Last
                                J = K - 1
                                CALL Reverse_Order_Base0(AInd, I, J)
                            END IF
                        END IF
                        IF (LastDescend) THEN
                            IF (LastEqualCase) THEN
                                ! Reverse into ascending order.
                                II = KLastEqual + 1
                                JJ = Finish + 1
                                CALL Reverse_Order_Base0(AInd, II, JJ)
                                ! Allocate a buffer to use as scratch memory.
                                ALLOCATE(WorkSpace(1:LastRunSize/2))
                                ! perform Rust's merge sort algorithm
                                CALL Rust_Merge_Stable(AVal, AInd(Start+1:Finish), LastRunSize, TrueLastDescendRunSize, FalseVal, WorkSpace)
                                DEALLOCATE(WorkSpace)
                            ELSE
                                ! Reverse into ascending order.
                                II = Start
                                JJ = Finish
                                CALL Reverse_Order_Base0(AInd, II, JJ)
                            END IF
                        END IF
                    END IF
                    RETURN
                END IF

                ! -----------------------------------------------------------------------------
                ! The first run does not provide enough information so must continue scanning
                ! -----------------------------------------------------------------------------

                ! collect information about the run (which is needed by Java's merge runs)
                RunSize = MIN(IAND(IOR(SHIFTA(ASize, 10), 127), 1023), MAX_RUN_CAPACITY)
                CALL MemAlloc(Run, RunSize, StartID=0_kIndex)
                Run(Count) = Low
                Count = Count + 1

            ELSE

                !++++++++++++++++++++++++++++++++++++++++++++++++++++
                !+++    Check special cases for the current run   +++
                !++++++++++++++++++++++++++++++++++++++++++++++++++++

                IF (KFirstEqual /= High) THEN
                    CurrEqualCase = TrueVal
                    TrueFirstDescendRunSize = KFirstEqual - Last
                END IF

                IF (K < High) THEN

                    IF (FirstRunSize > MinFirstRun) THEN
                        IF (((ASize < 40000).AND.(CurrRunSize <= 4)).OR. &
                            ((ASize >= 40000).AND.(CurrRunSize < 8))) THEN
                            ! -------------------------------------------------------------
                            ! Special Cases:  Partially Randomized Array
                            ! -------------------------------------------------------------
                            ReturnNow = TrueVal
                            ! For an array with not-too-short first run and very short later run(s),
                            IF (FirstDescend) THEN
                                IF (Count == 1) THEN
                                    ShortSecondRun = TrueVal
                                    ReturnNow = FalseVal
                                ELSE
                                    IF ((Count == 2).AND.(ShortSecondRun)) THEN
                                        ! first run not too short, second and third runs are short
                                        ! so merge sort algorithm.
                                        AlgoFlag = Algo_MergeSort
                                    ELSE
                                        ! otherwise, use Rust/Tim sort.
                                        AlgoFlag = Algo_RustSort
                                        LongRunSize = FirstRunSize
                                        FirstRun = TrueVal
                                    END IF
                                END IF
                            ELSE
                                ! use merge sort algorithm.
                                AlgoFlag = Algo_MergeSort
                            END IF
                            IF (CurrDescend) THEN
                                IF (CurrEqualCase) THEN
                                    ! Reverse into ascending order.
                                    I = Last
                                    J = KFirstEqual - 1
                                    CALL Reverse_Order_Base0(AInd, I, J)
                                    ! Allocate a buffer to use as scratch memory.
                                    ALLOCATE(WorkSpace(1:CurrRunSize/2))
                                    ! perform Rust's merge sort algorithm
                                    CALL Rust_Merge_Stable(AVal, AInd(Last:K-1), CurrRunSize, TrueFirstDescendRunSize, TrueVal, WorkSpace)
                                    DEALLOCATE(WorkSpace)
                                ELSE
                                    ! Reverse into ascending order.
                                    I = Last
                                    J = K - 1
                                    CALL Reverse_Order_Base0(AInd, I, J)
                                END IF
                            END IF
                            IF (ReturnNow) RETURN
                        END IF
                    END IF

                    IF ((Count > SHIFTA((K-Low), MIN_FIRST_RUNS_FACTOR)).AND. &
                        (CurrRunSize < FirstRunSize)) THEN
                        ! --------------------------------------------
                        ! Special Cases:  Partially Randomized Array
                        ! --------------------------------------------
                        ReturnNow = TrueVal
                        ! The first runs are not long enough to continue scanning
                        IF (FirstDescend) THEN
                            IF (Count == 1) THEN
                                IF (((ASize < 40000).AND.(CurrRunSize > 4)).OR. &
                                    ((ASize >= 40000).AND.(CurrRunSize >= 8))) THEN
                                    ! so use Rust/Tim sort for descending cases.
                                    AlgoFlag = Algo_RustSort
                                    LongRunSize = FirstRunSize
                                    FirstRun = TrueVal
                                ELSE
                                    IF (EqualCase) THEN
                                        ! so use quick sort for equal cases.
                                        !AlgoFlag = Algo_QuickSort
                                        AlgoFlag = Algo_HybridSort
                                    ELSE
                                        ShortSecondRun = TrueVal
                                        ReturnNow = FalseVal
                                    END IF
                                END IF
                            ELSEIF (Count == 2) THEN
                                IF (((ASize < 40000).AND.(CurrRunSize > 4)).OR. &
                                    ((ASize >= 40000).AND.(CurrRunSize >= 8))) THEN
                                    ! so use Rust/Tim sort for descending cases.
                                    AlgoFlag = Algo_RustSort
                                    LongRunSize = FirstRunSize
                                    FirstRun = TrueVal
                                ELSE
                                    IF (EqualCase) THEN
                                        ! so use quick sort for equal cases.
                                        !AlgoFlag = Algo_QuickSort
                                        AlgoFlag = Algo_HybridSort
                                    ELSE
                                        IF (ShortSecondRun) THEN
                                            ! so use hybrid sort for ascending random cases.
                                            AlgoFlag = Algo_HybridSort
                                        ELSE
                                            ! so use Rust/Tim sort for descending cases.
                                            AlgoFlag = Algo_RustSort
                                            LongRunSize = FirstRunSize
                                            FirstRun = TrueVal
                                        END IF
                                    END IF
                                END IF
                            ELSE
                                ! so use Rust/Tim sort for descending cases.
                                AlgoFlag = Algo_RustSort
                                LongRunSize = FirstRunSize
                                FirstRun = TrueVal
                            END IF
                            IF (CurrDescend) THEN
                                IF (CurrEqualCase) THEN
                                    ! Reverse into ascending order.
                                    I = Last
                                    J = KFirstEqual - 1
                                    CALL Reverse_Order_Base0(AInd, I, J)
                                    ! Allocate a buffer to use as scratch memory.
                                    ALLOCATE(WorkSpace(1:CurrRunSize/2))
                                    ! perform Rust's merge sort algorithm
                                    CALL Rust_Merge_Stable(AVal, AInd(Last:K-1), CurrRunSize, TrueFirstDescendRunSize, TrueVal, WorkSpace)
                                    DEALLOCATE(WorkSpace)
                                ELSE
                                    ! Reverse into ascending order.
                                    I = Last
                                    J = K - 1
                                    CALL Reverse_Order_Base0(AInd, I, J)
                                END IF
                            END IF
                        ELSE
                            IF (EqualCase) THEN
                                ! so use quick sort for equal cases.
                                !AlgoFlag = Algo_QuickSort
                                AlgoFlag = Algo_HybridSort
                            ELSE
                                ! so use hybrid sort for ascending cases.
                                AlgoFlag = Algo_HybridSort
                            END IF
                        END IF
                        IF (ReturnNow) RETURN
                    END IF

                    ! Check whether the current run is long enough.
                    IF (((CurrRunSize >= AscendRunLimit).AND.(.NOT.CurrDescend)).OR. &
                        ((CurrRunSize >= DescendRunLimit).AND.CurrDescend))THEN
                        ! --------------------------------------------------------------
                        ! Special Cases:  Partially Ordered Array with Long Middle Run
                        !                 (Ascending/Descending Head/Tail Random Cases)
                        ! --------------------------------------------------------------
                        ! For an array with long first run, use Rust/Tim sort.
                        AlgoFlag = Algo_RustSort
                        LongRunSize = FirstRunSize
                        FirstRun = TrueVal
                        IF ((((CurrRunSize == AscendRunLimit).AND.(.NOT.CurrDescend)).OR. &
                            ((CurrRunSize == DescendRunLimit).AND.CurrDescend)).AND.      &
                            (ASize <= 2000).AND.(FirstRunSize*2 < CurrRunSize)) THEN
                            ! use merge sort for small arrays.
                            AlgoFlag = Algo_MergeSort
                        END IF
                        IF (CurrDescend) THEN
                            IF (CurrEqualCase) THEN
                                ! Reverse into ascending order.
                                I = Last
                                J = KFirstEqual - 1
                                CALL Reverse_Order_Base0(AInd, I, J)
                                ! Allocate a buffer to use as scratch memory.
                                ALLOCATE(WorkSpace(1:CurrRunSize/2))
                                ! perform Rust's merge sort algorithm
                                CALL Rust_Merge_Stable(AVal, AInd(Last:K-1), CurrRunSize, TrueFirstDescendRunSize, TrueVal, WorkSpace)
                                DEALLOCATE(WorkSpace)
                            ELSE
                                ! Reverse into ascending order.
                                I = Last
                                J = K - 1
                                CALL Reverse_Order_Base0(AInd, I, J)
                            END IF
                        END IF
                        RETURN
                    END IF

                    IF ((Count >= MAX_RUN_CAPACITY).AND.(CurrRunSize < FirstRunSize)) THEN
                        ! --------------------------------------------
                        ! Special Cases:  Partially Randomized Array
                        ! --------------------------------------------
                        ! Array is not highly structured
                        IF (FirstDescend) THEN
                            IF (((ASize < 40000).AND.(CurrRunSize > 4)).OR. &
                                ((ASize >= 40000).AND.(CurrRunSize >= 8))) THEN
                                ! so use Rust/Tim sort for descending cases.
                                AlgoFlag = Algo_RustSort
                                LongRunSize = FirstRunSize
                                FirstRun = TrueVal
                            ELSE
                                ! so use hybrid sort for ascending cases.
                                AlgoFlag = Algo_HybridSort
                            END IF
                            IF (CurrDescend) THEN
                                IF (CurrEqualCase) THEN
                                    ! Reverse into ascending order.
                                    I = Last
                                    J = KFirstEqual - 1
                                    CALL Reverse_Order_Base0(AInd, I, J)
                                    ! Allocate a buffer to use as scratch memory.
                                    ALLOCATE(WorkSpace(1:CurrRunSize/2))
                                    ! perform Rust's merge sort algorithm
                                    CALL Rust_Merge_Stable(AVal, AInd(Last:K-1), CurrRunSize, TrueFirstDescendRunSize, TrueVal, WorkSpace)
                                    DEALLOCATE(WorkSpace)
                                ELSE
                                    ! Reverse into ascending order.
                                    I = Last
                                    J = K - 1
                                    CALL Reverse_Order_Base0(AInd, I, J)
                                END IF
                            END IF
                        ELSE
                            ! so use hybrid sort for ascending cases.
                            AlgoFlag = Algo_HybridSort
                        END IF
                        RETURN
                    END IF
                END IF

                Count = Count + 1
                IF (Count == RunSize) THEN
                    ! Increase capacity of index array.
                    NewRunSize = SHIFTL(Count, 1)
                    CALL MemResize(Run, NewRunSize)
                    RunSize = NewRunSize
                END IF

            END IF

            IF (CurrDescend) THEN
                IF (CurrEqualCase) THEN
                    ! Reverse into ascending order.
                    I = Last
                    J = KFirstEqual - 1
                    CALL Reverse_Order_Base0(AInd, I, J)
                    ! Allocate a buffer to use as scratch memory.
                    ALLOCATE(WorkSpace(1:CurrRunSize/2))
                    ! perform Rust's merge sort algorithm
                    CALL Rust_Merge_Stable(AVal, AInd(Last:K-1), CurrRunSize, TrueFirstDescendRunSize, TrueVal, WorkSpace)
                    DEALLOCATE(WorkSpace)
                ELSE
                    ! Reverse into ascending order.
                    I = Last
                    J = K - 1
                    CALL Reverse_Order_Base0(AInd, I, J)
                END IF
            END IF

            ! collect information about the run (which is needed by Java's merge runs)
            ! and reset working variables
            Last = K
            Run(Count) = Last
            K = K + 1
            KFirstEqual = High
            CurrEqualCase = FalseVal

        END DO

        ! ------------------------------------------------------------------
        ! Special Cases:  Partially Ordered Array with Certain Patterns
        !                 (Ascending/Descending Saw or Wave-Like Cases)
        !                 This is most suited to rarely randomized arrays.
        ! ------------------------------------------------------------------
        ! The array is highly structured so Java's merge runs of highly structured array.
        AlgoFlag = Algo_MergeRun

        RETURN

    END SUBROUTINE Expert_Inspection

    !**********************************************************************

    RECURSIVE SUBROUTINE Quick_Stable(AVal, AInd, BInd, LStart, REnd)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To sort the array in the range [LStart,REnd] into an ascending order
        ! using recursive quicksort algorithm.  If the number of elements in
        ! the range below the cutoff, then a pair-insertion sorting routine
        ! is used instead.  If maximum depth is zero, the ninther is used to
        ! select the pivot instead of the median of three.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(IN)       :: AVal(:)  ! values of array to be partitioned
        tIndex,    INTENT(INOUT)    :: AInd(:)  ! array indices
        tIndex,    INTENT(INOUT)    :: BInd(:)  ! auxiliary array indices
        tIndex,    INTENT(IN)       :: LStart   ! the start of the range
        tIndex,    INTENT(IN)       :: REnd     ! the end of the range

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: LEnd         ! the end of the left sub-array
        tIndex      :: RStart       ! the start of the right sub-array
        tIndex      :: NE           ! number of elements in the range

    !** FLOW:

        ! determine the number of elements in the specified range
        NE = REnd - LStart + 1

        ! check whether to perform pair insertion sort
        IF (NE <= QCutOff) THEN
            CALL Insert_Guarded(AVal, AInd, LStart, REnd)
            RETURN
        END IF

        ! perform partitioning
        CALL Partition_Stable(AVal, AInd, BInd, LStart, REnd, LEnd, RStart)

        ! perform quick sort on both sub-arrays
        CALL Quick_Stable(AVal, AInd, BInd, LStart, LEnd)
        CALL Quick_Stable(AVal, AInd, BInd, RStart, REnd)

        RETURN

    END SUBROUTINE Quick_Stable

    !**********************************************************************

    RECURSIVE SUBROUTINE Quick_Recur_Sort(AVal, AInd, BInd, LStart, REnd)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To sort the array in the range [LStart,REnd] into an ascending order
        ! using recursive quick-merge sort algorithm.  If the number of elements in
        ! the range below the cutoff, then a pair-insertion sorting routine
        ! is used instead.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(IN)       :: AVal(:)  ! values of array to be partitioned
        tIndex,    INTENT(INOUT)    :: AInd(:)  ! array indices
        tIndex,    INTENT(INOUT)    :: BInd(:)  ! auxiliary array indices
        tIndex,    INTENT(IN)       :: LStart   ! the start of the range
        tIndex,    INTENT(IN)       :: REnd     ! the end of the range

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: LEnd         ! the end of the left sub-array
        tIndex      :: RStart       ! the start of the right sub-array
        tIndex      :: LenL         ! the length of the left sub-array
        tIndex      :: LenR         ! the length of the right sub-array
        tIndex      :: NE           ! number of elements in the range

    !** FLOW:

        ! determine the number of elements in the specified range
        NE = REnd - LStart + 1

        ! check whether to perform pair insertion sort
        IF (NE <= QCutOff) THEN
            CALL Insert_Guarded(AVal, AInd, LStart, REnd)
            RETURN
        END IF

        ! perform stable partitioning
        CALL Partition_Stable(AVal, AInd, BInd, LStart, REnd, LEnd, RStart)

        ! compute lengths of sub-arrays
        LenL = LEnd - LStart
        LenR = REnd - RStart

        IF (LenL < LenR) THEN

            ! set cutoff for merge sort
            IF (LenL < 2000) THEN
                MCutOff = MergeInsertion_CutOff_Low
            ELSE
                MCutOff = MergeInsertion_CutOff_High
            END IF

            ! perform merge sort on left (smaller) sub-array
            CALL MergeIndex_HalfCopy(AVal, AInd, LStart-1, LEnd, BInd)

            ! perform quick sort on right (larger) sub-array
            CALL Quick_Recur_Sort(AVal, AInd, BInd, RStart, REnd)

        ELSE

            ! set cutoff for merge sort
            IF (LenR < 2000) THEN
                MCutOff = MergeInsertion_CutOff_Low
            ELSE
                MCutOff = MergeInsertion_CutOff_High
            END IF

            ! perform merge sort on right (smaller) sub-array
            CALL MergeIndex_HalfCopy(AVal, AInd, RStart-1, REnd, BInd)

            ! perform quick sort on left (larger) sub-array
            CALL Quick_Recur_Sort(AVal, AInd, BInd, LStart, LEnd)

        END IF

        RETURN

    END SUBROUTINE Quick_Recur_Sort

    !**********************************************************************

    SUBROUTINE Quick_Iter_Sort(AVal, AInd, BInd, Left, Right)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To sort the array in the range [LStart,REnd] into an ascending order
        ! using iterative quick-merge sort algorithm.  If the number of elements in
        ! the range below the cutoff, then a pair-insertion sorting routine
        ! is used instead.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(IN)       :: AVal(:)  ! values of array to be partitioned
        tIndex,    INTENT(INOUT)    :: AInd(:)  ! array indices
        tIndex,    INTENT(INOUT)    :: BInd(:)  ! auxiliary array indices
        tIndex,    INTENT(IN)       :: Left     ! the start of the range
        tIndex,    INTENT(IN)       :: Right    ! the end of the range

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: LStart       ! the start of the left sub-array
        tIndex      :: LEnd         ! the end of the left sub-array
        tIndex      :: RStart       ! the start of the right sub-array
        tIndex      :: REnd         ! the end of the right sub-array
        tIndex      :: NE           ! number of elements in the range
        tIndex      :: LenL         ! length of left partition
        tIndex      :: LenR         ! length of right partition

    !** FLOW:

        ! initialize
        LStart = Left
        REnd   = Right

        DO WHILE (LStart < REnd)

            ! determine the number of elements in the current range
            NE = REnd - LStart + 1

            ! check whether to perform pair insertion sort
            IF (NE <= QCutOff) THEN
                CALL Insert_Guarded(AVal, AInd, LStart, REnd)
                EXIT
            END IF

            ! perform partitioning
            CALL Partition_Stable(AVal, AInd, BInd, LStart, REnd, LEnd, RStart)

            ! compute lengths of sub-arrays
            LenL = LEnd - LStart
            LenR = REnd - RStart

            IF (LenL < LenR) THEN
                ! set cutoff for merge sort
                IF (LenL < 2000) THEN
                    MCutOff = MergeInsertion_CutOff_Low
                ELSE
                    MCutOff = MergeInsertion_CutOff_High
                END IF
                ! perform merge sort on left (smaller) sub-array
                CALL MergeIndex_HalfCopy(AVal, AInd, LStart-1, LEnd, BInd)
                LStart = RStart
            ELSE
                ! set cutoff for merge sort
                IF (LenR < 2000) THEN
                    MCutOff = MergeInsertion_CutOff_Low
                ELSE
                    MCutOff = MergeInsertion_CutOff_High
                END IF
                ! perform merge sort on right (smaller) sub-array
                CALL MergeIndex_HalfCopy(AVal, AInd, RStart-1, REnd, BInd)
                REnd = LEnd
            END IF

        END DO

        RETURN

    END SUBROUTINE Quick_Iter_Sort

    !******************************************************************

    SUBROUTINE Partition_Stable(AVal, AInd, BInd, LStart, REnd, LEnd, RStart)

    !** PURPOSE OF THIS SUBROUTINE
        ! To partition an array using stable partitioning scheme.
        ! The given array in the range [LStart,REnd] is partitioned
        ! into two sub-arrays, Left and Right, such that all elements
        ! in the Left sub-array in the range [LStart,LEnd] are less
        ! than or equal to the selected pivot, and all elements in
        ! the Right sub-array in the range [RStart,REnd] are greater
        ! than or equal to the selected pivot.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(IN)       :: AVal(:)  ! values of array to be partitioned
        tIndex,    INTENT(INOUT)    :: AInd(:)  ! array indices
        tIndex,    INTENT(INOUT)    :: BInd(:)  ! auxiliary array indices
        tIndex,    INTENT(IN)       :: LStart   ! on entry, the start of the given array
                                                ! on exit,  the start of the Left sub-array
        tIndex,    INTENT(IN)       :: REnd     ! on entry, the end of the given array
                                                ! on exit,  the end of the Right sub-array
        tIndex,    INTENT(OUT)      :: LEnd     ! the end of the Left sub-array
        tIndex,    INTENT(OUT)      :: RStart   ! the start of the Right sub-array

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: PInd         ! index of the pivot used to partition the array
        tIndex      :: Less         ! current pointer for value less than the pivot
        tIndex      :: Great        ! current pointer for value greater than the pivot
        tIndex      :: Equal        ! current pointer for value equal to the pivot
        tIndex      :: Indx         ! index
        tIndex      :: BSize        ! size of BInd array
        tIndex      :: N, M1, M2, M3
        tIndex      :: Eps, Ninther

    !** FLOW:

        N = REnd - LStart + 1

        ! use Tukey's ninther as partitioning element (the pivot)
        ! also, rearrange so that A(LStart) < A(LStart+1) < A(REnd)
        Eps = N/8
        M1 = LStart + Eps
        M2 = LStart + N/2
        M3 = REnd - Eps
        M1 = Median_Of_Three(AVal, AInd, LStart, M1, M1+Eps)
        M2 = Median_Of_Three(AVal, AInd, M2-Eps, M2, M2+Eps)
        M3 = Median_Of_Three(AVal, AInd, M3-Eps, M3, REnd)
        Ninther = Median_Of_Three(AVal, AInd, M1, M2, M3)
        PInd = AInd(Ninther)

        ! initialize greater index
        BSize = SIZE(BInd)
        Less  = LStart - 1
        Equal = BSize + 1
        Great = 0

        ! perform supposedly-stable partition of the array
        DO Indx = LStart, REnd
            ! check whether the current element is greater in value than our pivot value.
            ! If so, save it to the buffer.  Otherwise, move it up front
            IF (COMPARE_GLT(AVal(PInd), AVal(AInd(Indx)))) THEN
                ! save greater value to the front of the buffer
                Great = Great + 1
                BInd(Great) = AInd(Indx)
            ELSEIF (COMPARE_GLT(AVal(AInd(Indx)), AVal(PInd))) THEN
                ! move less value to the front of the array
                Less = Less + 1
                IF (Less < Indx) AInd(Less) = AInd(Indx)
            ELSE
                ! save equal value to the back of the buffer
                Equal = Equal - 1
                BInd(Equal) = AInd(Indx)
            END IF
        END DO

        ! set maximum index of sub-arrays with less values
        LEnd = Less

        ! transfer data back from the buffer
        IF (Equal <= BSize) THEN
            ! first, transfer data with equal values
            DO Indx = BSize, Equal, -1
                Less = Less + 1
                AInd(Less) = BInd(Indx)
            END DO
        END IF
        Less = Less + 1
        IF (Great >= 1) THEN
            ! next, transfer data with greater values
            AInd(Less:REnd) = BInd(1:Great)
        END IF
        ! set mimimum index of sub-arrays with greater values
        RStart = Less

        RETURN

    END SUBROUTINE Partition_Stable

    !**********************************************************************

    SUBROUTINE MergeIndex_HalfCopy(AVal, AInd, P, K, BInd)

    !** PURPOSE OF THIS SUBROUTINE
        ! For small arrays, the pair-insertion sort is used as it is the
        ! fastest sorting algorithm for small N. This also ensures that
        ! the merge is never called with empty regions [P1,K1) or [P2,K2).
        ! The routine calls the 'HalfCopy_Split_Merge' routine. It sorts the
        ! input range [P,K) 'in place' but requires a buffer to temporarily
        ! store N/2 array elements.

    !** REFERENCES
        ! Adapted from routine 'MergeSort' in the following reference:
        ! Juszczak, C. 2007.  "Fast Mergesort Implementation Based on Half-Copying Merge Algorithm"

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(IN)       :: AVal(:)  ! array values
        tIndex,    INTENT(INOUT)    :: AInd(0:) ! working array indices
        tIndex,    INTENT(IN)       :: P        ! the start (inclusive) of the range of array to be sorted
        tIndex,    INTENT(IN)       :: K        ! the end (exclusive) of the range of array to be sorted
        tIndex,    INTENT(INOUT)    :: BInd(0:) ! working array indices

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: NE, S, T, KMS

    !** FLOW:

        NE = K - P
        IF (NE > MCutOff) THEN
            ! split the array into halves.
            S = P + NE/2
            T = K-(S-P)
            ! CopyMerge(A2, B)
            CALL HalfCopy_Split_Merge(AVal, AInd, S, K, BInd, 0)
            ! CopyMerge(A1, A2)
            CALL HalfCopy_Split_Merge(AVal, AInd, P, S, AInd, T)

            KMS = K - S
            IF (COMPARE_GLE(AVal(BInd(KMS-1)), AVal(AInd(T)))) THEN
                ! just copy array B to the first half of array A
                AInd(T-KMS:T-1) = BInd(0:KMS-1)
            ELSE
                ! merge the resulting sub-arrays
                ! MergeR(B, A2, A)
                CALL MergingR_HalfCopy(AVal, BInd, 0, KMS, AInd, T, K)
            END IF
        ELSE
            CALL Insert_Guarded(AVal, AInd, P+1, K)
        END IF

        RETURN

    END SUBROUTINE MergeIndex_HalfCopy

    !*****************************************************************

    RECURSIVE SUBROUTINE HalfCopy_Split_Merge(AVal, AInd, P, K, BInd, T)

    !** PURPOSE OF THIS SUBROUTINE
        ! Based on the half-copying algorithm, a very effcient copying verion
        ! of mergesort may be written sorting the elements form the range [P,K)
        ! and placing the result in the range [T,T+(K-P)). The right half of the
        ! input is sorted (by using recursively the same algorithm) into the right
        ! half of the output range. Then, the left half of the input is range
        ! sorted (also recursively) into the right half of the input range. Then,
        ! it is merged with our half copying merge with the numbers already present
        ! in the output.

    !** REFERENCES
        ! Adapted from routine 'Copying_MergeSort' in the following reference:
        ! Juszczak, C. 2007.  "Fast Mergesort Implementation Based on Half-Copying Merge Algorithm"

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(IN)       :: AVal(:)  ! array values
        tIndex,    INTENT(INOUT)    :: AInd(0:) ! working array indices
        tIndex,    INTENT(IN)       :: P        ! the start (inclusive) of the range of array to be sorted
        tIndex,    INTENT(IN)       :: K        ! the end (exclusive) of the range of array to be sorted
        tIndex,    INTENT(INOUT)    :: BInd(0:) ! working array indices
        tIndex,    INTENT(IN)       :: T        ! the start (inclusive) of the range of array to be sorted

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: NE, S, S1, T1

    !** FLOW:

        NE = K - P
        IF (NE > MCutOff) THEN
            ! split the array into halves.
            S = P + (K-P)/2
            ! CopyMerge(A2, B2)
            CALL HalfCopy_Split_Merge(AVal, AInd, S, K, BInd, T+(S-P))
            ! CopyMerge(A1, B2)
            CALL HalfCopy_Split_Merge(AVal, AInd, P, S, AInd, S)

            S1 = S + (S-P)
            T1 = T+(S-P)
            IF (COMPARE_GLE(AVal(AInd(S1-1)), AVal(BInd(T1)))) THEN
                ! just copy array A to the first half of array B
                BInd(T1-(S1-S):T1-1) = AInd(S:S1-1)
            ELSE
                ! merge the resulting sub-arrays
                ! Merge(A2, B2, B)
                CALL Merging_HalfCopy(AVal, AInd, S, S1, BInd, T1, T+(K-P))
            END IF
        ELSE
            ! copy data from A to B
            BInd(T:T+NE-1) = AInd(P:K-1)

            ! perform pair-insertion sort on B
            CALL Insert_Guarded(AVal, BInd, T+1, T+NE)
        END IF

        RETURN

    END SUBROUTINE HalfCopy_Split_Merge

    !*****************************************************************

    RECURSIVE SUBROUTINE Quad_Split_Merge(AVal, AInd, Left, Right, BInd, CutOff)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To split A into 4 sub-arrays and sort them into B, and
        ! then merge the four sub-arrays from B to A.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(IN)       :: AVal(:)  ! array values
        tIndex,    INTENT(INOUT)    :: AInd(0:) ! array indices
        tIndex,    INTENT(IN)       :: Left     ! the start of the range
        tIndex,    INTENT(IN)       :: Right    ! the end of the range
        tIndex,    INTENT(INOUT)    :: BInd(0:) ! array indices (auxialiary array)
        tIndex,    INTENT(IN)       :: CutOff   ! cutoff to pair insertion sort

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: NE, NOver4, Mid, LeftMid, RightMid

    !** FLOW:

        NE = Right - Left
        IF (NE > CutOff) THEN

            ! split the array into four parts.
            NOver4   = NE / 4
            LeftMid  = Left + NOver4
            Mid      = LeftMid + NOver4
            RightMid = Mid + NOver4

            ! recursively split into four sub-arrays
            CALL Quad_Split_Merge(AVal, AInd, Left,     LeftMid,  BInd, CutOff)
            CALL Quad_Split_Merge(AVal, AInd, LeftMid,  Mid,      BInd, CutOff)
            CALL Quad_Split_Merge(AVal, AInd, Mid,      RightMid, BInd, CutOff)
            CALL Quad_Split_Merge(AVal, AInd, RightMid, Right,    BInd, CutOff)

            IF (COMPARE_GLE(AVal(AInd(LeftMid-1)), AVal(AInd(LeftMid)))) THEN
                ! merely copy the resulting sub-arrays into array B
                BInd(Left:Mid-1) = AInd(Left:Mid-1)
            ELSE
                ! merge the resulting sub-arrays into array B
                CALL Merging_Wiki(AVal, AInd, Left,  LeftMid,  Mid,   BInd)
            END IF
            IF (COMPARE_GLE(AVal(AInd(RightMid-1)), AVal(AInd(RightMid)))) THEN
                ! merely copy the resulting sub-arrays into array B
                BInd(Mid:Right-1) = AInd(Mid:Right-1)
            ELSE
                ! merge the resulting sub-arrays into array B
                CALL Merging_Wiki(AVal, AInd, Mid, RightMid, Right, BInd)
            END IF

            IF (COMPARE_GLE(AVal(BInd(Mid-1)), AVal(BInd(Mid)))) THEN
                ! array B has already been sorted so just copy it to array A
                AInd(Left:Right-1) = BInd(Left:Right-1)
            ELSE
                ! merge the sub-arrays of array B into array A
                CALL Merging_Wiki(AVal, BInd, Left, Mid, Right, AInd)
            END IF
        ELSE
            CALL Insert_Guarded(AVal, AInd, Left+1, Right)
        END IF

        RETURN

    END SUBROUTINE Quad_Split_Merge

    !******************************************************************

END SUBROUTINE Wise_Rank_Stable

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   INDEXING ENGINE ROUTINES
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

RECURSIVE FUNCTION Java_Merge_Runs_Rank(AVal, AInd, BInd, Offset, Aim, Run, Low, High) RESULT(DstPtr)

!** PURPOSE OF THIS SUBROUTINE
    ! To merge the specified runs.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)  ! the source array
    tIndex,    INTENT(INOUT)    :: AInd(0:) ! indices of the source array
    tIndex,    INTENT(INOUT)    :: BInd(0:) ! temporary buffer indices used in merging
    tIndex,    INTENT(IN)       :: Offset   ! the start index in the source, inclusive
    tIndex,    INTENT(IN)       :: Aim      ! aim specifies merging: to source ( > 0), buffer ( < 0) or any ( == 0)
    tIndex,    INTENT(IN)       :: Run(0:)  ! the start indices of the runs, inclusive
    tIndex,    INTENT(IN)       :: Low      ! the start index of the first run, inclusive
    tIndex,    INTENT(IN)       :: High     ! the start index of the last run, inclusive
    tIndex                      :: DstPtr   ! destination pointer
    ! DstPtr =  1, A is the destination
    !        = -1, B is the destination
    !        =  0,  neither A nor B is the destination (should never happen, except a bug)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: A1Ptr
    tIndex      :: A2Ptr
    tIndex      :: Lo, Hi
    tIndex      :: Lo1, Hi1
    tIndex      :: Lo2, Hi2
    tIndex      :: I, J, K
    tIndex      :: Mi, Rmi
    tLogical    :: A1_Assoc
    tLogical    :: A2_Assoc

!** FLOW:

    ! initialize
    Lo = Low
    Hi = High
    DstPtr = 0

    IF ((Hi-Lo)== 1) THEN
        IF (Aim >= 0) THEN
            DstPtr = 1
        ELSE
            I = Run(Hi)
            J = I - OffSet
            DO WHILE (I > Run(Lo))
                J = J - 1
                I = I - 1
                BInd(J) = AInd(I)
            END DO
            DstPtr = -1
        END IF
        RETURN
    END IF

    ! Split into approximately equal parts.
    Mi = Lo
    Rmi = SHIFTR((Run(Lo) + Run(Hi)), 1)
    Mi = Mi + 1
    DO WHILE (Run(Mi+1) <= Rmi)
        Mi = Mi + 1
    END DO

    ! Merge the left and right parts.
    A1Ptr = Java_Merge_Runs_Rank(AVal, AInd, BInd, Offset, -Aim, Run, Lo, Mi)
    A2Ptr = Java_Merge_Runs_Rank(AVal, AInd, BInd, Offset,    0, Run, Mi, Hi)

    ! set destination pointer
    IF (A1Ptr == 1) THEN
        DstPtr = -1
    ELSE
        DstPtr = 1
    END IF

    !  set indices
    IF (A1Ptr == 1) THEN
        K = Run(Lo) - Offset
    ELSE
        K = Run(Lo)
    END IF
    IF (A1Ptr == -1) THEN
        Lo1 = Run(Lo) - Offset
        Hi1 = Run(Mi) - Offset
    ELSE
        Lo1 = Run(Lo)
        Hi1 = Run(Mi)
    END IF
    IF (A2Ptr == -1) THEN
        Lo2 = Run(Mi) - Offset
        Hi2 = Run(Hi) - Offset
    ELSE
        Lo2 = Run(Mi)
        Hi2 = Run(Hi)
    END IF

    ! set flags
    A1_Assoc = (DstPtr == A1Ptr)
    A2_Assoc = (DstPtr == A2Ptr)

    ! merge parts
    IF (DstPtr == 1) THEN
        IF ((A1Ptr == 1).AND.(A2Ptr == 1)) THEN
            CALL MergeParts(AVal, AInd, K, AInd, Lo1, Hi1, AInd, Lo2, Hi2, A1_Assoc, A2_Assoc)
        ELSEIF ((A1Ptr == 1).AND.(A2Ptr == -1)) THEN
            CALL MergeParts(AVal, AInd, K, AInd, Lo1, Hi1, BInd, Lo2, Hi2, A1_Assoc, A2_Assoc)
        ELSEIF ((A1Ptr == -1).AND.(A2Ptr == 1)) THEN
            CALL MergeParts(AVal, AInd, K, BInd, Lo1, Hi1, AInd, Lo2, Hi2, A1_Assoc, A2_Assoc)
        ELSEIF ((A1Ptr == -1).AND.(A2Ptr == -1)) THEN
            CALL MergeParts(AVal, AInd, K, BInd, Lo1, Hi1, BInd, Lo2, Hi2, A1_Assoc, A2_Assoc)
        ELSE
            CALL Handle_ErrLevel('Java_Merge_Runs_Rank', ModName, ErrWarning, 'A1 and A2 are NULL.')
        END IF
    ELSEIF (DstPtr == -1) THEN
        IF ((A1Ptr == 1).AND.(A2Ptr == 1)) THEN
            CALL MergeParts(AVal, BInd, K, AInd, Lo1, Hi1, AInd, Lo2, Hi2, A1_Assoc, A2_Assoc)
        ELSEIF ((A1Ptr == 1).AND.(A2Ptr == -1)) THEN
            CALL MergeParts(AVal, BInd, K, AInd, Lo1, Hi1, BInd, Lo2, Hi2, A1_Assoc, A2_Assoc)
        ELSEIF ((A1Ptr == -1).AND.(A2Ptr == 1)) THEN
            CALL MergeParts(AVal, BInd, K, BInd, Lo1, Hi1, AInd, Lo2, Hi2, A1_Assoc, A2_Assoc)
        ELSEIF ((A1Ptr == -1).AND.(A2Ptr == -1)) THEN
            CALL MergeParts(AVal, BInd, K, BInd, Lo1, Hi1, BInd, Lo2, Hi2, A1_Assoc, A2_Assoc)
        ELSE
            CALL Handle_ErrLevel('Java_Merge_Runs_Rank', ModName, ErrWarning, 'A1 and A2 are NULL.')
        END IF
    ELSE
        CALL Handle_ErrLevel('Java_Merge_Runs_Rank', ModName, ErrWarning, 'DstPtr = 0.  This should not happen.')
    END IF

    RETURN

    CONTAINS

    SUBROUTINE MergeParts(AVal, Dest, Kin, A1, Low1, High1, A2, Low2, High2, A1_Assoc, A2_Assoc)

    !** PURPOSE OF THIS SUBROUTINE
        ! To merge the sorted parts.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(IN)       :: AVal(:)  ! array values
        tIndex,    INTENT(INOUT)    :: Dest(0:) ! the destination where parts are merged
        tIndex,    INTENT(IN)       :: Kin      ! the start index of the destination, inclusive
        tIndex,    INTENT(IN)       :: A1(0:)   ! the first part
        tIndex,    INTENT(IN)       :: Low1     ! the start index of the first part, inclusive
        tIndex,    INTENT(IN)       :: High1    ! the end index of the first part, exclusive
        tIndex,    INTENT(IN)       :: A2(0:)   ! the second part
        tIndex,    INTENT(IN)       :: Low2     ! the start index of the second part, inclusive
        tIndex,    INTENT(IN)       :: High2    ! the end index of the second part, exclusive
        tLogical,  INTENT(IN)       :: A1_Assoc ! true if Dest is associated with A1
        tLogical,  INTENT(IN)       :: A2_Assoc ! true if Dest is associated with A2

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Lo1, Lo2, Hi1, Hi2, K

    !** FLOW:

        ! initialize
        K = Kin
        Lo1 = Low1
        Hi1 = High1
        Lo2 = Low2
        Hi2 = High2

        ! Merge parts sequentially.
        DO WHILE ((Lo1 < Hi1).AND.(Lo2 < Hi2))
            IF (COMPARE_GLT(AVal(A1(Lo1)), AVal(A2(Lo2)))) THEN
                Dest(K) = A1(Lo1)
                Lo1 = Lo1 + 1
            ELSE
                Dest(K) = A2(Lo2)
                Lo2 = Lo2 + 1
            END IF
            K = K + 1
        END DO

        IF ((.NOT.A1_Assoc).OR.(K < Lo1)) THEN
            DO WHILE (Lo1 < Hi1)
                Dest(K) =  A1(Lo1)
                K = K + 1
                Lo1 = Lo1 + 1
            END DO
        END IF

        IF ((.NOT.A2_Assoc).OR.(K < Lo2)) THEN
            DO WHILE (Lo2 < Hi2)
                Dest(K) =  A2(Lo2)
                K = K + 1
                Lo2 = Lo2 + 1
            END DO
        END IF

        RETURN

    END SUBROUTINE MergeParts

    !*****************************************************************

END FUNCTION Java_Merge_Runs_Rank

!**********************************************************************

SUBROUTINE Rust_Merge_Rank(AVal, AInd, ALen, LongRunSize, FirstRun, BInd)

!** PURPOSE OF THIS SUBROUTINE
    ! To sort an array into ascending order using Rust's merge sort
    ! that borrows some (but not all) of the ideas from TimSort.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)
    tIndex,    INTENT(INOUT)    :: AInd(0:)
    tIndex,    INTENT(IN)       :: ALen
    tIndex,    INTENT(IN)       :: LongRunSize
    tLogical,  INTENT(IN)       :: FirstRun     ! true if long run size is of the first run
                                                ! otherwise, long run size is of the last run
    tIndex,    INTENT(INOUT)    :: BInd(0:)

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! The maximum number of entries in a run stack, good for an array of 2**64 elements.
    tInteger, PARAMETER     :: Max_Merge_Stack = INT(CEILING(LOG(2.0_kFP**64) / &
                                                     LOG(1.6180339887_kFP)))

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Min_Run, Num, NR

    !** FLOW:

    ! Very short runs are extended using insertion sort to span at least Min_Run elements.
    ! Slices of up to this length are sorted using insertion sort.
    Num = ALen
    NR = 0
    DO WHILE(Num >= 64)
        NR = IOR(NR, IAND(Num, 1))
        Num = SHIFTA(Num, 1)
    END DO
    Min_Run = Num + NR

    ! perform merge sort algorithm
    IF (FirstRun) THEN
        CALL Rust_Merge_Sort_LongFirst(AVal, AInd, ALen, LongRunSize, Min_Run, BInd)
    ELSE
        CALL Rust_Merge_Sort_LongLast(AVal, AInd, ALen, LongRunSize, Min_Run, BInd)
    END IF

    RETURN

    CONTAINS

    SUBROUTINE Rust_Merge_Sort_LongFirst(AVal, AInd, ALen, FirstRunSize, Min_Run, BInd)

    !** PURPOSE OF THIS SUBROUTINE
        ! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
        ! which is described in detail at
        ! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
        !
        ! The algorithm identifies strictly descending and non-descending
        ! subsequences, which are called natural runs.  Where these runs are less
        ! than a minimum run size they are padded by adding additional samples
        ! using a pair-insertion sort.  The merge process is driven by a stack of
        ! pending unmerged runs.  Each newly found run is pushed onto the stack,
        ! and then pairs of adjacent runs are merged until these two invariants
        ! are satisfied:
        !
        ! 1. for every 'i' in '1..size(runs)-1': 'runlen(i - 1) > runlen(i)'
        ! 2. for every 'i' in '2..size(runs)-1': 'runlen(i - 2) > runlen(i - 1) + runlen(i)'
        !
        ! The invariants ensure that the total running time is 'O(n log n)' worst-case.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)
        tIndex,      INTENT(INOUT)  :: AInd(0:)
        tIndex,      INTENT(IN)     :: ALen
        tIndex,      INTENT(IN)     :: FirstRunSize
        tIndex,      INTENT(IN)     :: Min_Run
        tIndex,      INTENT(INOUT)  :: BInd(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Finish, nRun, R, RCount, Start
        tIndex      :: RunLen(0:Max_Merge_Stack-1)
        tIndex      :: RunBase(0:Max_Merge_Stack-1)
        tIndex      :: LeftLen, LeftBase
        tIndex      :: RightLen, RightBase
        tIndex      :: Lo, Hi, I
        tIndex      :: Temp

    !** FLOW:

        ! Following Rust sort, natural runs in 'A' are identified by traversing
        ! it backwards.  By traversing it backward, merges more often go in the
        ! opposite direction (forwards).  According to developers of Rust sort,
        ! merging forwards is slightly faster than merging backwards.  Therefore
        ! identifying runs by traversing backwards should improve performance.
        RCount = 0
        Finish = ALen - 1
        DO WHILE (Finish >= 0)

            ! Find the next natural run, and reverse it if it's strictly descending.
            Start = Finish
            IF (Start > FirstRunSize) THEN
                Start = Start - 1
                IF (COMPARE_GLT(AVal(AInd(Start+1)), AVal(AInd(Start)))) THEN
                    Descending: DO WHILE (Start > 0)
                        IF (COMPARE_GLE(AVal(AInd(Start-1)), AVal(AInd(Start)))) EXIT Descending
                        Start = Start - 1
                    END DO Descending
                    ! reverse the segment into ascending order
                    Lo = Start
                    Hi = Finish
                    CALL Reverse_Order_Base0(AInd, Lo, Hi)
                ELSE
                    Ascending: DO WHILE(Start > 0)
                        IF (COMPARE_GLT(AVal(AInd(Start)), AVal(AInd(Start-1)))) EXIT Ascending
                        Start = Start - 1
                    END DO Ascending
                END IF
            ELSE
                Start = 0
            END IF

            ! compute run length
            nRun = Finish - Start

            IF (nRun < Min_Run) THEN
                ! If run is short, extend to MinRun
                IF (nRun < Min_Run/8) THEN
                    ! If nRun is too short, use pair insertion sort
                    Start = Finish - Min_Run + 1
                    IF (Start < 0) Start = 0
                    CALL DualInsert_Guarded(AVal, AInd, Start+1, Finish+1)
                ELSE
                    ! nRun is long enough, use insertion sort
                    Insert_Loop: DO WHILE (Start > 0)
                        IF ((Finish-Start) >= (Min_Run-1)) EXIT Insert_Loop
                        Start = Start - 1
                        ! perform insertion sort by inserting AInd(Start) into the pre-sorted
                        ! sequence AInd(Start+1:Finish) so that the whole AInd(Start:Finish)
                        ! becomes sorted.
                        Temp = AInd(Start)
                        Find_Hole: DO I = Start+1, Finish
                            IF (COMPARE_GLE(AVal(Temp), AVal(AInd(I)))) EXIT Find_Hole
                            AInd(I-1) = AInd(I)
                        END DO Find_Hole
                        AInd(I-1) = Temp
                    END DO Insert_Loop
                    IF ((Start == 0).AND.(Finish == ALen-1)) RETURN
                END IF
            END IF

            ! initialize the stack
            RunBase(RCount) = Start
            RunLen(RCount)  = Finish - Start + 1

            Finish = Start - 1
            RCount = RCount + 1

            ! Determine whether pairs of adjacent runs need to be merged to satisfy
            ! the invariants, and, if so, merge them.
            Merge_Loop: DO

                ! examine the stack of runs waiting to be merged
                R = Collapse(RunBase(0:RCount-1), RunLen(0:RCount-1))

                IF ((R < 0).OR.(RCount <= 1)) EXIT Merge_Loop
                LeftLen   = RunLen(R+1)
                LeftBase  = RunBase(R+1)
                RightLen  = RunLen(R)
                RightBase = RunBase(R)

                ! merge adjacent runs
                CALL Merging(AVal, AInd(LeftBase:RightBase+RightLen-1), LeftLen, BInd)

                ! set the stack
                RunBase(R) = LeftBase
                RunLen(R)  = LeftLen + RightLen
                IF (R == RCount-3) THEN
                    RunBase(R+1) = RunBase(R+2)
                    RunLen(R+1)  = RunLen(R+2)
                END IF
                RCount = RCount - 1
            END DO Merge_Loop
        END DO

        RETURN

    END SUBROUTINE Rust_Merge_Sort_LongFirst

    !******************************************************************

    SUBROUTINE Rust_Merge_Sort_LongLast(AVal, AInd, ALen, LastRunSize, Min_Run, BInd)

    !** PURPOSE OF THIS SUBROUTINE
        ! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
        ! which is described in detail at
        ! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
        !
        ! The algorithm identifies strictly descending and non-descending
        ! subsequences, which are called natural runs.  Where these runs are less
        ! than a minimum run size they are padded by adding additional samples
        ! using a pair-insertion sort.  The merge process is driven by a stack of
        ! pending unmerged runs.  Each newly found run is pushed onto the stack,
        ! and then pairs of adjacent runs are merged until these two invariants
        ! are satisfied:
        !
        ! 1. for every 'i' in '1..size(runs)-1': 'runlen(i - 1) > runlen(i)'
        ! 2. for every 'i' in '2..size(runs)-1': 'runlen(i - 2) > runlen(i - 1) + runlen(i)'
        !
        ! The invariants ensure that the total running time is 'O(n log n)' worst-case.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)
        tIndex,      INTENT(INOUT)  :: AInd(0:)
        tIndex,      INTENT(IN)     :: ALen
        tIndex,      INTENT(IN)     :: LastRunSize
        tIndex,      INTENT(IN)     :: Min_Run
        tIndex,      INTENT(INOUT)  :: BInd(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Finish, nRun, R, RCount, Start
        tIndex      :: RunLen(0:Max_Merge_Stack-1)
        tIndex      :: RunBase(0:Max_Merge_Stack-1)
        tIndex      :: LeftLen, LeftBase
        tIndex      :: RightLen, RightBase
        tIndex      :: Lo, Hi, I
        tIndex      :: Temp

    !** FLOW:

        ! Following Rust sort, natural runs in 'A' are identified by traversing
        ! it backwards.  By traversing it backward, merges more often go in the
        ! opposite direction (forwards).  According to developers of Rust sort,
        ! merging forwards is slightly faster than merging backwards.  Therefore
        ! identifying runs by traversing backwards should improve performance.
        RCount = 0
        Finish = ALen - 1

        ! initialize the stack with the long last run
        Start = Finish - LastRunSize + 1
        RunBase(RCount) = Start
        RunLen(RCount)  = LastRunSize
        Finish = Start - 1
        RCount = RCount + 1

        DO WHILE (Finish >= 0)

            ! Find the next natural run, and reverse it if it's strictly descending.
            Start = Finish
            IF (Start > 0) THEN
                Start = Start - 1
                IF (COMPARE_GLT(AVal(AInd(Start+1)), AVal(AInd(Start)))) THEN
                    Descending: DO WHILE (Start > 0)
                        IF (COMPARE_GLE(AVal(AInd(Start-1)), AVal(AInd(Start)))) EXIT Descending
                        Start = Start - 1
                    END DO Descending
                    ! reverse the segment into ascending order
                    Lo = Start
                    Hi = Finish
                    CALL Reverse_Order_Base0(AInd, Lo, Hi)
                ELSE
                    Ascending: DO WHILE(Start > 0)
                        IF (COMPARE_GLT(AVal(AInd(Start)), AVal(AInd(Start-1)))) EXIT Ascending
                        Start = Start - 1
                    END DO Ascending
                END IF
            END IF

            ! compute run length
            nRun = Finish - Start

            IF (nRun < Min_Run) THEN
                ! If run is short, extend to MinRun
                IF (nRun < Min_Run/8) THEN
                    ! If nRun is too short, use pair insertion sort
                    Start = Finish - Min_Run + 1
                    IF (Start < 0) Start = 0
                    CALL DualInsert_Guarded(AVal, AInd, Start+1, Finish+1)
                ELSE
                    ! nRun is long enough, use insertion sort
                    Insert_Loop: DO WHILE (Start > 0)
                        IF ((Finish-Start) >= (Min_Run-1)) EXIT Insert_Loop
                        Start = Start - 1
                        ! perform insertion sort by inserting AInd(Start) into the pre-sorted
                        ! sequence AInd(Start+1:Finish) so that the whole AInd(Start:Finish)
                        ! becomes sorted.
                        Temp = AInd(Start)
                        Find_Hole: DO I = Start+1, Finish
                            IF (COMPARE_GLE(AVal(Temp), AVal(AInd(I)))) EXIT Find_Hole
                            AInd(I-1) = AInd(I)
                        END DO Find_Hole
                        AInd(I-1) = Temp
                    END DO Insert_Loop
                    IF ((Start == 0).AND.(Finish == ALen-1)) RETURN
                END IF
            END IF

            ! initialize the stack
            RunBase(RCount) = Start
            RunLen(RCount)  = Finish - Start + 1

            Finish = Start - 1
            RCount = RCount + 1

            ! Determine whether pairs of adjacent runs need to be merged to satisfy
            ! the invariants, and, if so, merge them.
            Merge_Loop: DO

                ! examine the stack of runs waiting to be merged
                R = Collapse(RunBase(0:RCount-1), RunLen(0:RCount-1))

                IF ((R < 0).OR.(RCount <= 1)) EXIT Merge_Loop
                LeftLen   = RunLen(R+1)
                LeftBase  = RunBase(R+1)
                RightLen  = RunLen(R)
                RightBase = RunBase(R)

                ! merge adjacent runs
                CALL Merging(AVal, AInd(LeftBase:RightBase+RightLen-1), LeftLen, BInd)

                ! set the stack
                RunBase(R) = LeftBase
                RunLen(R)  = LeftLen + RightLen
                IF (R == RCount-3) THEN
                    RunBase(R+1) = RunBase(R+2)
                    RunLen(R+1)  = RunLen(R+2)
                END IF
                RCount = RCount - 1
            END DO Merge_Loop
        END DO

        RETURN

    END SUBROUTINE Rust_Merge_Sort_LongLast

    !******************************************************************

    FUNCTION Collapse(Base, Length) RESULT (R)

    !** PURPOSE OF THIS SUBROUTINE
        ! To examine the stack of runs waiting to be merged, identifying adjacent runs
        ! to be merged until the stack invariants are re-established:
        ! 1. len(-3) > len(-2) + len(-1)
        ! 2. len(-2) > len(-1)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex, INTENT(IN)  :: Base(0:)
        tIndex, INTENT(IN)  :: Length(0:)
        tIndex              :: R

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: N
        tLogical    :: Test

    !** FLOW:

        N = MIN(SIZE(Base),SIZE(Length))
        Test = FalseVal
        IF (N >= 2) THEN
            IF ((Base(N-1) == 0).OR.(Length(N-2) <= Length(N-1))) THEN
                Test = TrueVal
            ELSEIF (N >= 3) THEN
                ! X exists
                IF (Length(N-3) <= (Length(N-2)+Length(N-1))) THEN
                    Test = TrueVal
                    ! |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
                ELSEIF(N >= 4) THEN
                    IF (Length(N-4) <= (Length(N-3)+Length(N-2))) THEN
                        Test = TrueVal
                        ! |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                    END IF
                END IF
            END IF
        END IF
        IF (Test) THEN
            ! By default merge Y & Z, rho2 or rho3
            IF (N >= 3) THEN
                IF (Length(N-3) < Length(N-1)) THEN
                    R = N - 3
                    ! |X| < |Z| => merge X & Y, rho1
                    RETURN
                END IF
            END IF
            R = N - 2
            ! |Y| <= |Z| => merge Y & Z, rho4
            RETURN
        ELSE
            R = -1
        END IF

        RETURN

    END FUNCTION Collapse

    !******************************************************************

    SUBROUTINE Merging(AVal, AInd, Mid, BInd)

    !** PURPOSE OF THIS SUBROUTINE
        ! To merge the two non-decreasing runs 'A(0:Mid-1)' and 'A(Mid:)'
        ! using 'B' as a temporary storage, and stores the merged runs into
        ! 'A(0:)'.  'Mid' must be > 0, and < 'SIZE(A)-1'. The buffer 'B'
        ! must be long enough to hold the shorter of the two runs.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)
        tIndex,      INTENT(INOUT)  :: AInd(0:)
        tIndex,      INTENT(IN)     :: Mid
        tIndex,      INTENT(INOUT)  :: BInd(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: ALen, I, J, K
        tIndex      :: Temp

    !** FLOW:

        ALen = SIZE(AInd)

        ! Merge first copies the shorter run into 'B'. Then, depending on which
        ! run was shorter, it traces the copied run and the longer run forwards
        ! (or backwards), comparing their next unprocessed elements and then
        ! copying the lesser (or greater) one into 'A'.

        IF (Mid <= (ALen - Mid)) THEN
            ! The left run is shorter.
            BInd(0:Mid-1) = AInd(0:Mid-1)
            I = 0
            J = Mid
            Merge_Lower: DO K = 0, ALen-1
                IF (COMPARE_GLE(AVal(BInd(I)), AVal(AInd(J)))) THEN
                    AInd(K) = BInd(I)
                    I = I + 1
                    IF (I >= Mid) EXIT Merge_Lower
                ELSE
                    AInd(K) = AInd(J)
                    J = J + 1
                    IF (J >= ALen) THEN
                        AInd(K+1:) = BInd(I:Mid-1)
                        EXIT Merge_Lower
                    END IF
                END IF
            END DO Merge_Lower
        ELSE
            ! The right run is shorter so check that it is stable
            BInd(0:ALen-Mid-1) = AInd(Mid:ALen-1)
            I = Mid - 1
            J = ALen - Mid -1
            Merge_Upper: DO K = ALen-1, 0, -1
                IF (COMPARE_GLE(AVal(AInd(I)), AVal(BInd(J)))) THEN
                    AInd(K) = BInd(J)
                    J = J - 1
                    IF (J < 0) EXIT Merge_Upper
                ELSE
                    AInd(K) = AInd(I)
                    I = I - 1
                    IF (I < 0) THEN
                        AInd(0:K-1) = BInd(0:J)
                        EXIT Merge_Upper
                    END IF
                END IF
            END DO Merge_Upper
        END IF

        RETURN

    END SUBROUTINE Merging

    !******************************************************************

END SUBROUTINE Rust_Merge_Rank

!**********************************************************************

SUBROUTINE Rust_Merge_Stable(AVal, AInd, ALen, LongRunSize, FirstRun, BInd)

!** PURPOSE OF THIS SUBROUTINE
    ! To sort an array into ascending order using Rust's merge sort
    ! that borrows some (but not all) of the ideas from TimSort.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)
    tIndex,    INTENT(INOUT)    :: AInd(0:)
    tIndex,    INTENT(IN)       :: ALen
    tIndex,    INTENT(IN)       :: LongRunSize
    tLogical,  INTENT(IN)       :: FirstRun     ! true if long run size is of the first run
                                                ! otherwise, long run size is of the last run
    tIndex,    INTENT(INOUT)    :: BInd(0:)

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! The maximum number of entries in a run stack, good for an array of 2**64 elements.
    tInteger, PARAMETER     :: Max_Merge_Stack = INT(CEILING(LOG(2.0_kFP**64) / &
                                                     LOG(1.6180339887_kFP)))

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Min_Run, Num, NR

    !** FLOW:

    ! Very short runs are extended using insertion sort to span at least Min_Run elements.
    ! Slices of up to this length are sorted using insertion sort.
    Num = ALen
    NR = 0
    DO WHILE(Num >= 64)
        NR = IOR(NR, IAND(Num, 1))
        Num = SHIFTA(Num, 1)
    END DO
    Min_Run = Num + NR

    ! perform merge sort algorithm
    IF (FirstRun) THEN
        CALL Rust_Merge_Sort_LongFirst(AVal, AInd, ALen, LongRunSize, Min_Run, BInd)
    ELSE
        CALL Rust_Merge_Sort_LongLast(AVal, AInd, ALen, LongRunSize, Min_Run, BInd)
    END IF

    RETURN

    CONTAINS

    SUBROUTINE Rust_Merge_Sort_LongFirst(AVal, AInd, ALen, FirstRunSize, Min_Run, BInd)

    !** PURPOSE OF THIS SUBROUTINE
        ! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
        ! which is described in detail at
        ! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
        !
        ! The algorithm identifies strictly descending and non-descending
        ! subsequences, which are called natural runs.  Where these runs are less
        ! than a minimum run size they are padded by adding additional samples
        ! using a pair-insertion sort.  The merge process is driven by a stack of
        ! pending unmerged runs.  Each newly found run is pushed onto the stack,
        ! and then pairs of adjacent runs are merged until these two invariants
        ! are satisfied:
        !
        ! 1. for every 'i' in '1..size(runs)-1': 'runlen(i - 1) > runlen(i)'
        ! 2. for every 'i' in '2..size(runs)-1': 'runlen(i - 2) > runlen(i - 1) + runlen(i)'
        !
        ! The invariants ensure that the total running time is 'O(n log n)' worst-case.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)
        tIndex,      INTENT(INOUT)  :: AInd(0:)
        tIndex,      INTENT(IN)     :: ALen
        tIndex,      INTENT(IN)     :: FirstRunSize
        tIndex,      INTENT(IN)     :: Min_Run
        tIndex,      INTENT(INOUT)  :: BInd(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Finish, nRun, R, RCount, Start
        tIndex      :: RunLen(0:Max_Merge_Stack-1)
        tIndex      :: RunBase(0:Max_Merge_Stack-1)
        tIndex      :: LeftLen, LeftBase
        tIndex      :: RightLen, RightBase
        tIndex      :: Lo, Hi, I
        tIndex      :: Temp

    !** FLOW:

        ! Following Rust sort, natural runs in 'A' are identified by traversing
        ! it backwards.  By traversing it backward, merges more often go in the
        ! opposite direction (forwards).  According to developers of Rust sort,
        ! merging forwards is slightly faster than merging backwards.  Therefore
        ! identifying runs by traversing backwards should improve performance.
        RCount = 0
        Finish = ALen - 1
        DO WHILE (Finish >= 0)

            ! Find the next natural run, and reverse it if it's strictly descending.
            Start = Finish
            IF (Start > FirstRunSize) THEN
                Start = Start - 1
                IF (COMPARE_GLT(AVal(AInd(Start+1)), AVal(AInd(Start)))) THEN
                    Descending: DO WHILE (Start > 0)
                        IF (COMPARE_GLE(AVal(AInd(Start-1)), AVal(AInd(Start)))) EXIT Descending
                        Start = Start - 1
                    END DO Descending
                    ! reverse the segment into ascending order
                    Lo = Start
                    Hi = Finish
                    CALL Reverse_Order_Base0(AInd, Lo, Hi)
                ELSE
                    Ascending: DO WHILE(Start > 0)
                        IF (COMPARE_GLT(AVal(AInd(Start)), AVal(AInd(Start-1)))) EXIT Ascending
                        Start = Start - 1
                    END DO Ascending
                END IF
            ELSE
                Start = 0
            END IF

            ! compute run length
            nRun = Finish - Start

            IF (nRun < Min_Run) THEN
                ! If run is short, extend to MinRun
                IF (nRun < Min_Run/8) THEN
                    ! If nRun is too short, use pair insertion sort
                    Start = Finish - Min_Run + 1
                    IF (Start < 0) Start = 0
                    CALL Insert_Guarded(AVal, AInd, Start+1, Finish+1)
                ELSE
                    ! nRun is long enough, use insertion sort
                    Insert_Loop: DO WHILE (Start > 0)
                        IF ((Finish-Start) >= (Min_Run-1)) EXIT Insert_Loop
                        Start = Start - 1
                        ! perform insertion sort by inserting AInd(Start) into the pre-sorted
                        ! sequence AInd(Start+1:Finish) so that the whole AInd(Start:Finish)
                        ! becomes sorted.
                        Temp = AInd(Start)
                        Find_Hole: DO I = Start+1, Finish
                            IF (COMPARE_GLE(AVal(Temp), AVal(AInd(I)))) EXIT Find_Hole
                            AInd(I-1) = AInd(I)
                        END DO Find_Hole
                        AInd(I-1) = Temp
                    END DO Insert_Loop
                    IF ((Start == 0).AND.(Finish == ALen-1)) RETURN
                END IF
            END IF

            ! initialize the stack
            RunBase(RCount) = Start
            RunLen(RCount)  = Finish - Start + 1

            Finish = Start - 1
            RCount = RCount + 1

            ! Determine whether pairs of adjacent runs need to be merged to satisfy
            ! the invariants, and, if so, merge them.
            Merge_Loop: DO

                ! examine the stack of runs waiting to be merged
                R = Collapse(RunBase(0:RCount-1), RunLen(0:RCount-1))

                IF ((R < 0).OR.(RCount <= 1)) EXIT Merge_Loop
                LeftLen   = RunLen(R+1)
                LeftBase  = RunBase(R+1)
                RightLen  = RunLen(R)
                RightBase = RunBase(R)

                ! merge adjacent runs
                CALL Merging(AVal, AInd(LeftBase:RightBase+RightLen-1), LeftLen, BInd)

                ! set the stack
                RunBase(R) = LeftBase
                RunLen(R)  = LeftLen + RightLen
                IF (R == RCount-3) THEN
                    RunBase(R+1) = RunBase(R+2)
                    RunLen(R+1)  = RunLen(R+2)
                END IF
                RCount = RCount - 1
            END DO Merge_Loop
        END DO

        RETURN

    END SUBROUTINE Rust_Merge_Sort_LongFirst

    !******************************************************************

    SUBROUTINE Rust_Merge_Sort_LongLast(AVal, AInd, ALen, LastRunSize, Min_Run, BInd)

    !** PURPOSE OF THIS SUBROUTINE
        ! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
        ! which is described in detail at
        ! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
        !
        ! The algorithm identifies strictly descending and non-descending
        ! subsequences, which are called natural runs.  Where these runs are less
        ! than a minimum run size they are padded by adding additional samples
        ! using a pair-insertion sort.  The merge process is driven by a stack of
        ! pending unmerged runs.  Each newly found run is pushed onto the stack,
        ! and then pairs of adjacent runs are merged until these two invariants
        ! are satisfied:
        !
        ! 1. for every 'i' in '1..size(runs)-1': 'runlen(i - 1) > runlen(i)'
        ! 2. for every 'i' in '2..size(runs)-1': 'runlen(i - 2) > runlen(i - 1) + runlen(i)'
        !
        ! The invariants ensure that the total running time is 'O(n log n)' worst-case.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)
        tIndex,      INTENT(INOUT)  :: AInd(0:)
        tIndex,      INTENT(IN)     :: ALen
        tIndex,      INTENT(IN)     :: LastRunSize
        tIndex,      INTENT(IN)     :: Min_Run
        tIndex,      INTENT(INOUT)  :: BInd(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Finish, nRun, R, RCount, Start
        tIndex      :: RunLen(0:Max_Merge_Stack-1)
        tIndex      :: RunBase(0:Max_Merge_Stack-1)
        tIndex      :: LeftLen, LeftBase
        tIndex      :: RightLen, RightBase
        tIndex      :: Lo, Hi, I
        tIndex      :: Temp

    !** FLOW:

        ! Following Rust sort, natural runs in 'A' are identified by traversing
        ! it backwards.  By traversing it backward, merges more often go in the
        ! opposite direction (forwards).  According to developers of Rust sort,
        ! merging forwards is slightly faster than merging backwards.  Therefore
        ! identifying runs by traversing backwards should improve performance.
        RCount = 0
        Finish = ALen - 1

        ! initialize the stack with the long last run
        Start = Finish - LastRunSize + 1
        RunBase(RCount) = Start
        RunLen(RCount)  = LastRunSize
        Finish = Start - 1
        RCount = RCount + 1

        DO WHILE (Finish >= 0)

            ! Find the next natural run, and reverse it if it's strictly descending.
            Start = Finish
            IF (Start > 0) THEN
                Start = Start - 1
                IF (COMPARE_GLT(AVal(AInd(Start+1)), AVal(AInd(Start)))) THEN
                    Descending: DO WHILE (Start > 0)
                        IF (COMPARE_GLE(AVal(AInd(Start-1)), AVal(AInd(Start)))) EXIT Descending
                        Start = Start - 1
                    END DO Descending
                    ! reverse the segment into ascending order
                    Lo = Start
                    Hi = Finish
                    CALL Reverse_Order_Base0(AInd, Lo, Hi)
                ELSE
                    Ascending: DO WHILE(Start > 0)
                        IF (COMPARE_GLT(AVal(AInd(Start)), AVal(AInd(Start-1)))) EXIT Ascending
                        Start = Start - 1
                    END DO Ascending
                END IF
            END IF

            ! compute run length
            nRun = Finish - Start

            IF (nRun < Min_Run) THEN
                ! If run is short, extend to MinRun
                IF (nRun < Min_Run/8) THEN
                    ! If nRun is too short, use pair insertion sort
                    Start = Finish - Min_Run + 1
                    IF (Start < 0) Start = 0
                    CALL Insert_Guarded(AVal, AInd, Start+1, Finish+1)
                ELSE
                    ! nRun is long enough, use insertion sort
                    Insert_Loop: DO WHILE (Start > 0)
                        IF ((Finish-Start) >= (Min_Run-1)) EXIT Insert_Loop
                        Start = Start - 1
                        ! perform insertion sort by inserting AInd(Start) into the pre-sorted
                        ! sequence AInd(Start+1:Finish) so that the whole AInd(Start:Finish)
                        ! becomes sorted.
                        Temp = AInd(Start)
                        Find_Hole: DO I = Start+1, Finish
                            IF (COMPARE_GLE(AVal(Temp), AVal(AInd(I)))) EXIT Find_Hole
                            AInd(I-1) = AInd(I)
                        END DO Find_Hole
                        AInd(I-1) = Temp
                    END DO Insert_Loop
                    IF ((Start == 0).AND.(Finish == ALen-1)) RETURN
                END IF
            END IF

            ! initialize the stack
            RunBase(RCount) = Start
            RunLen(RCount)  = Finish - Start + 1

            Finish = Start - 1
            RCount = RCount + 1

            ! Determine whether pairs of adjacent runs need to be merged to satisfy
            ! the invariants, and, if so, merge them.
            Merge_Loop: DO

                ! examine the stack of runs waiting to be merged
                R = Collapse(RunBase(0:RCount-1), RunLen(0:RCount-1))

                IF ((R < 0).OR.(RCount <= 1)) EXIT Merge_Loop
                LeftLen   = RunLen(R+1)
                LeftBase  = RunBase(R+1)
                RightLen  = RunLen(R)
                RightBase = RunBase(R)

                ! merge adjacent runs
                CALL Merging(AVal, AInd(LeftBase:RightBase+RightLen-1), LeftLen, BInd)

                ! set the stack
                RunBase(R) = LeftBase
                RunLen(R)  = LeftLen + RightLen
                IF (R == RCount-3) THEN
                    RunBase(R+1) = RunBase(R+2)
                    RunLen(R+1)  = RunLen(R+2)
                END IF
                RCount = RCount - 1
            END DO Merge_Loop
        END DO

        RETURN

    END SUBROUTINE Rust_Merge_Sort_LongLast

    !******************************************************************

    FUNCTION Collapse(Base, Length) RESULT (R)

    !** PURPOSE OF THIS SUBROUTINE
        ! To examine the stack of runs waiting to be merged, identifying adjacent runs
        ! to be merged until the stack invariants are re-established:
        ! 1. len(-3) > len(-2) + len(-1)
        ! 2. len(-2) > len(-1)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex, INTENT(IN)  :: Base(0:)
        tIndex, INTENT(IN)  :: Length(0:)
        tIndex              :: R

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: N
        tLogical    :: Test

    !** FLOW:

        N = MIN(SIZE(Base),SIZE(Length))
        Test = FalseVal
        IF (N >= 2) THEN
            IF ((Base(N-1) == 0).OR.(Length(N-2) <= Length(N-1))) THEN
                Test = TrueVal
            ELSEIF (N >= 3) THEN
                ! X exists
                IF (Length(N-3) <= (Length(N-2)+Length(N-1))) THEN
                    Test = TrueVal
                    ! |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
                ELSEIF(N >= 4) THEN
                    IF (Length(N-4) <= (Length(N-3)+Length(N-2))) THEN
                        Test = TrueVal
                        ! |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                    END IF
                END IF
            END IF
        END IF
        IF (Test) THEN
            ! By default merge Y & Z, rho2 or rho3
            IF (N >= 3) THEN
                IF (Length(N-3) < Length(N-1)) THEN
                    R = N - 3
                    ! |X| < |Z| => merge X & Y, rho1
                    RETURN
                END IF
            END IF
            R = N - 2
            ! |Y| <= |Z| => merge Y & Z, rho4
            RETURN
        ELSE
            R = -1
        END IF

        RETURN

    END FUNCTION Collapse

    !******************************************************************

    SUBROUTINE Merging(AVal, AInd, Mid, BInd)

    !** PURPOSE OF THIS SUBROUTINE
        ! To merge the two non-decreasing runs 'A(0:Mid-1)' and 'A(Mid:)'
        ! using 'B' as a temporary storage, and stores the merged runs into
        ! 'A(0:)'.  'Mid' must be > 0, and < 'SIZE(A)-1'. The buffer 'B'
        ! must be long enough to hold the shorter of the two runs.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)
        tIndex,      INTENT(INOUT)  :: AInd(0:)
        tIndex,      INTENT(IN)     :: Mid
        tIndex,      INTENT(INOUT)  :: BInd(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: ALen, I, J, K
        tIndex      :: Temp

    !** FLOW:

        ALen = SIZE(AInd)

        ! Merge first copies the shorter run into 'B'. Then, depending on which
        ! run was shorter, it traces the copied run and the longer run forwards
        ! (or backwards), comparing their next unprocessed elements and then
        ! copying the lesser (or greater) one into 'A'.

        IF (Mid <= (ALen - Mid)) THEN
            ! The left run is shorter.
            BInd(0:Mid-1) = AInd(0:Mid-1)
            I = 0
            J = Mid
            Merge_Lower: DO K = 0, ALen-1
                IF (COMPARE_GLE(AVal(BInd(I)), AVal(AInd(J)))) THEN
                    AInd(K) = BInd(I)
                    I = I + 1
                    IF (I >= Mid) EXIT Merge_Lower
                ELSE
                    AInd(K) = AInd(J)
                    J = J + 1
                    IF (J >= ALen) THEN
                        AInd(K+1:) = BInd(I:Mid-1)
                        EXIT Merge_Lower
                    END IF
                END IF
            END DO Merge_Lower
        ELSE
            ! The right run is shorter so check that it is stable
            BInd(0:ALen-Mid-1) = AInd(Mid:ALen-1)
            I = Mid - 1
            J = ALen - Mid -1
            Merge_Upper: DO K = ALen-1, 0, -1
                IF (COMPARE_GLE(AVal(AInd(I)), AVal(BInd(J)))) THEN
                    AInd(K) = BInd(J)
                    J = J - 1
                    IF (J < 0) EXIT Merge_Upper
                ELSE
                    AInd(K) = AInd(I)
                    I = I - 1
                    IF (I < 0) THEN
                        AInd(0:K-1) = BInd(0:J)
                        EXIT Merge_Upper
                    END IF
                END IF
            END DO Merge_Upper
        END IF

        RETURN

    END SUBROUTINE Merging

    !******************************************************************

END SUBROUTINE Rust_Merge_Stable

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                      HYBRID SORT ROUTINES
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   NON-STABLE DRIVER ROUTINES
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE SUBROUTINE Intro_Rank(AVal, AInd)

!** PURPOSE OF THIS SUBROUTINE
    ! To determine indices such that they sort the specified array into the desired
    ! order using the *IntroSort* algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)          ! array values
    tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal)) ! array indices

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER     :: Insertion_CutOff_Low  = 40   ! cutoff to pair insertion sort
    tInteger, PARAMETER     :: Insertion_CutOff_Mid  = 50   ! cutoff to pair insertion sort
    tInteger, PARAMETER     :: Insertion_CutOff_High = 70   ! cutoff to pair insertion sort

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: CutOff       ! cutoff to pair insertion sort
    tIndex          :: MaxDepth     ! maximum depth used to switch partitioning routine
    tIndex          :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(AVal, KIND=kIndex)

    ! initialize the indices
    CALL Index_Init(NA, AInd)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Insertion_CutOff_Low) THEN
        CALL DualInsert_Guarded(AVal, AInd, 1, NA)
        RETURN
    END IF

    ! set cutoff depending on the size of the input array
    IF (NA < 500) THEN
        CutOff = Insertion_CutOff_Low
    ELSEIF (NA < 2000) THEN
        CutOff = Insertion_CutOff_Mid
    ELSE
        CutOff = Insertion_CutOff_High
    END IF

    ! determine maximum depth based on the size of the input array
    MaxDepth = MaximumDepth(NA)

    ! perform quick sort
    CALL QuickIndex(AVal, AInd, 1, NA, CutOff, TrueVal, MaxDepth)

    RETURN

    CONTAINS

    RECURSIVE SUBROUTINE QuickIndex(AVal,AInd,LStart,REnd,CutOff,LeftMost,MaxDepth)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To sort the array in the range [LStart,REnd] into an ascending order
        ! using recursive quicksort algorithm.  If the number of elements in
        ! the range below the cutoff, then a pair-insertion sorting routine
        ! is used instead.  If maximum depth is zero, the ninther is used to
        ! select the pivot instead of the median of three.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)  ! array values
        tIndex,      INTENT(INOUT)  :: AInd(:)  ! array indices
        tIndex,      INTENT(IN)     :: LStart   ! the start of the range
        tIndex,      INTENT(IN)     :: REnd     ! the end of the range
        tIndex,      INTENT(IN)     :: CutOff   ! cutoff to pair insertion sort
        tLogical,    INTENT(IN)     :: LeftMost ! flag indicating whether the specified range
                                                ! is the left most of the given array
                                                ! If false, an unguarded pair insertion sorting
                                                ! algorithm is used instead of the guarded one.
        tIndex,      INTENT(IN)     :: MaxDepth ! maximum depth used to switch partitioning routine

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: LEnd     ! the end of the left sub-array
        tIndex          :: RStart   ! the start of the right sub-array
        tIndex          :: NE       ! number of elements in the range

    !** FLOW:

        ! determine the number of elements in the specified range
        NE = REnd - LStart + 1

        ! check whether to perform pair insertion sort
        IF (NE <= CutOff) THEN
            ! check whether the specified range is the left most range of the given array
            IF (LeftMost) THEN
                CALL DualInsert_Guarded(AVal, AInd, LStart, REnd)
            ELSE
                CALL DualInsert_UnGuarded(AVal, AInd, LStart, REnd)
            END IF
            RETURN
        END IF

        IF (MaxDepth > 0) THEN
            ! perform partitioning
            CALL Partition_Mo3(AVal, AInd, LStart, REnd, LEnd, RStart)
        ELSE
            ! for worst-case situations, use heap sort instead
            CALL Heap_Rank(AVal, AInd(LStart:REnd))
            RETURN
        END IF

        ! perform quick sort on the two sub-arrays
        CALL QuickIndex(AVal, AInd, LStart, LEnd, CutOff, LeftMost, MaxDepth-1)
        CALL QuickIndex(AVal, AInd, RStart, REnd, CutOff, FalseVal,  MaxDepth-1)

        RETURN

    END SUBROUTINE QuickIndex

    !**********************************************************************

    SUBROUTINE Partition_Mo3(AVal,AInd,LStart,REnd,LEnd,RStart)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To partition an array using Press's partitioning scheme.
        ! The given array in the range [LStart,REnd] is partitioned
        ! into two sub-arrays, Left and Right, such that all elements
        ! in the Left sub-array in the range [LStart,LEnd] are less
        ! than or equal to the selected pivot, and all elements in
        ! the Right sub-array in the range [RStart,REnd] are greater
        ! than or equal to the selected pivot.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)  ! array values
        tIndex,      INTENT(INOUT)  :: AInd(:)  ! array indices
        tIndex,      INTENT(IN)     :: LStart   ! on entry, the start of the given array
                                                ! on exit,  the start of the Left sub-array
        tIndex,      INTENT(IN)     :: REnd     ! on entry, the end of the given array
                                                ! on exit,  the end of the Right sub-array
        tIndex,      INTENT(OUT)    :: LEnd     ! the end of the Left sub-array
        tIndex,      INTENT(OUT)    :: RStart   ! the start of the Right sub-array

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: PInd, Temp
        tIndex          :: Mid, LPtr, UPtr
        tIndex          :: I, J, K

    !** FLOW:

        ! choose median of left, center, and right elements as partitioning element (the pivot)
        ! also, rearrange so that A(LStart) < A(LStart+1) < A(REnd)
        Mid = LStart + (REnd-LStart) / 2
        CALL Rank_3_Items(AVal, AInd, LStart, Mid, REnd)
        EXCHANGE(AInd, Mid, LStart+1)

        ! set pivot value
        PInd = AInd(LStart+1)

        ! set working pointers
        LPtr = LStart + 1
        UPtr = REnd

        DO
            ! increment the left pointer until we find an element
            ! that is greater than the pivot
            DO
                LPtr = LPtr + 1
                IF (COMPARE_GLE(AVal(PInd), AVal(AInd(LPtr)))) EXIT
            END DO

            ! decrement the right pointer until we find an element
            ! that is less than the pivot
            DO
                UPtr = UPtr - 1
                IF (COMPARE_GLE(AVal(AInd(UPtr)), AVal(PInd))) EXIT
            END DO

            ! exit if the pointers cross
            IF (UPtr < LPtr) EXIT

            ! swap A(LPtr) and A(UPtr)
            EXCHANGE(AInd, LPtr, UPtr)
        END DO

        ! insert partitioning element
        AInd(LStart+1) = AInd(UPtr)
        AInd(UPtr)     = PInd

        ! set output indices
        LEnd   = UPtr - 1
        RStart = LPtr

        RETURN

    END SUBROUTINE Partition_Mo3

    !**********************************************************************

    SUBROUTINE Heap_Rank(AVal,AInd)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To sort array in ascending order using heap sort

    !** REFERENCES
        ! Adapted from Routines 'HeapSort' of Numerical Recipe F90 and Rosetta Code

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(IN)       :: AVal(:)  ! array values
        tIndex,    INTENT(INOUT)    :: AInd(:)  ! array indices

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: N
        tIndex          :: Start
        tIndex          :: Bottom
        tIndex          :: Temp

    !** FLOW:

        ! initialize
        N = SIZE(AInd)

        ! perform heap sort ranking
        DO Start = N/2, 1, -1
            CALL Siftdown(AVal, AInd, Start, N);
        END DO

        DO Bottom = N, 2, -1
            SWAP_SCALAR(AInd(1), AInd(Bottom))
            CALL Siftdown(AVal, AInd, 1, Bottom-1)
        END DO

        RETURN


    END SUBROUTINE Heap_Rank

    !******************************************************************

    SUBROUTINE Siftdown(AVal, AInd, Start, Bottom)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To assist heap sort ranking

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)  ! array values
        tIndex,      INTENT(INOUT)  :: AInd(:)  ! array indices
        tIndex,      INTENT(IN)     :: Start
        tIndex,      INTENT(IN)     :: Bottom

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: J, JOld
        tIndex          :: Temp

    !** FLOW:

        Temp = AInd(Start)
        Jold = Start
        J    = Start + Start
        ! do while j <= bottom
        DO
            IF (J > Bottom) EXIT
            IF (J < Bottom) THEN
                ! compare to the better underling
                IF (COMPARE_GLT(AVal(AInd(J)), AVal(AInd(J+1)))) THEN
                    J = J + 1
                END IF
            END IF
            ! found a's level. Terminate the sift-down.
            IF (COMPARE_GLE(AVal(AInd(J)), AVal(Temp))) EXIT
            ! otherwise, demote A and continue.
            AInd(Jold) = AInd(J)
            Jold = J
            J = J + J
        END DO
        ! put A into its slot.
        AInd(Jold) = Temp

        RETURN

    END SUBROUTINE Siftdown

    !******************************************************************

END SUBROUTINE Intro_Rank

!**********************************************************************

MODULE SUBROUTINE Java_Rank(AVal, AInd)

!** PURPOSE OF THIS SUBROUTINE
    ! To determine indices such that they sort the specified array into the desired
    ! order using the Java's sorting algorithm [1].

!** REFEENCE
!   [1] <a href="https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/util/DualPivotQuicksort.java">
!       Java's DualPivotQuicksort class. </a> <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)          ! array values
    tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal)) ! array indices

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(AVal, KIND=kIndex)

    ! initialize the indices
    CALL Index_Init(NA, AInd)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! perform quick sort
    CALL Quick_DualPivot(AVal, AInd, 0, 0, NA)

    RETURN

CONTAINS

    RECURSIVE SUBROUTINE Quick_DualPivot(AVal, AInd, iBit, Lo, Hi)

    !** PURPOSE OF THIS SUBROUTINE
        ! To sort the specified array using the Dual-Pivot Quicksort and/or
        ! other sorts in special-cases, possibly with parallel partitions.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)  ! array values
        tIndex,      INTENT(INOUT)  :: AInd(0:) ! array indices
        tIndex,      INTENT(IN)     :: iBit     ! the combination of recursion depth and bit flag, where
                                                ! the right bit "0" indicates that array is the leftmost part
        tIndex,      INTENT(IN)     :: Lo       ! the index of the first element, inclusive, to be sorted
        tIndex,      INTENT(IN)     :: Hi       ! the index of the last element, exclusive, to be sorted

    !** SUBROUTINE PARAMETER DECLARATIONS:
        ! Max array size to use mixed insertion sort.
        tInteger, PARAMETER     :: MAX_MIXED_INSERTION_SORT_SIZE = 65
        ! Max array size to use insertion sort.
        tInteger, PARAMETER     :: MAX_INSERTION_SORT_SIZE = 44
        ! Min array size to try merging of runs.
        tInteger, PARAMETER     :: MIN_TRY_MERGE_SIZE = ISHFT(4, 10)
        ! Threshold of mixed insertion sort is incremented by this value.
        tInteger, PARAMETER     :: DELTA = ISHFT(3, 1)
        ! Max recursive partitioning depth before using heap sort.
        tInteger, PARAMETER     :: MAX_RECURSION_DEPTH = 64 * DELTA

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: Bits, Low, High, iEnd, Size, Last
        tIndex          :: AInd3, AIndK, Temp
        tIndex          :: PInd, PInd1, PInd2
        tIndex          :: Step
        tIndex          :: K
        tIndex          :: E1, E2, E3, E4, E5
        tIndex          :: Lower, Upper

    !** FLOW:

        ! initialize
        Bits = iBit
        Low  = Lo
        High = Hi

        DO WHILE (TrueVal)

            ! initialize
            iEnd = High - 1
            Size = High - Low

            ! Run mixed insertion sort on small non-leftmost parts.
            IF ((Size < (MAX_MIXED_INSERTION_SORT_SIZE+Bits)).AND.(IAND(Bits, 1) > 0)) THEN
                Last = High - 3 * SHIFTL(SHIFTA(Size, 5), 3)
                CALL Insertion_Mixed(AVal, AInd, Low, Last, High)
                RETURN
            END IF

            ! Invoke insertion sort on small leftmost part.
            IF (Size < MAX_INSERTION_SORT_SIZE) THEN
                CALL Insertion_Java(AVal, AInd, Low, High)
                RETURN
            END IF

            ! Check if the whole array or large non-leftmost
            ! parts are nearly sorted and then merge runs.
            IF ((Bits == 0).OR.(Size > MIN_TRY_MERGE_SIZE).AND.(IAND(Bits, 1) > 0)) THEN
                IF (TryMergeRuns(AVal, AInd, Low, Size)) THEN
                    RETURN
                END IF
            END IF

            ! Switch to heap sort if execution
            ! time is becoming quadratic.
            Bits = Bits + DELTA
            IF (Bits > MAX_RECURSION_DEPTH) THEN
                CALL HeapIndex_Java(AVal, AInd, Low, High)
                RETURN
            END IF

            ! Use an inexpensive approximation of the golden ratio
            ! to select five sample elements and determine pivots.
            Step = SHIFTA(Size, 3) * 3 + 3

            ! Five elements around (and including) the central element
            ! will be used for pivot selection as described below. The
            ! unequal choice of spacing these elements was empirically
            ! determined to work well on a wide variety of inputs.
            E1 = Low + Step
            E5 = iEnd - Step
            E3 = SHIFTR((E1 + E5), 1)
            E2 = SHIFTR((E1 + E3), 1)
            E4 = SHIFTR((E3 + E5), 1)
            AInd3 = AInd(E3)

            ! Sort these elements in place by the combination
            ! of 4-element sorting network and insertion sort.
            !
            !    5 ------o-----------o------------
            !            |           |
            !    4 ------|-----o-----o-----o------
            !            |     |           |
            !    2 ------o-----|-----o-----o------
            !                  |     |
            !    1 ------------o-----o------------
            IF (COMPARE_GLT(AVal(AInd(E5)), AVal(AInd(E2)))) THEN
                ! t = AInd(E5); AInd(E5) = AInd(E2); AInd(E2) = t
                EXCHANGE(AInd, E5, E2)
            END IF
            IF (COMPARE_GLT(AVal(AInd(E4)), AVal(AInd(E1)))) THEN
                ! t = AInd(E4); AInd(E4) = AInd(E1); AInd(E1) = t
                EXCHANGE(AInd, E4, E1)
            END IF
            IF (COMPARE_GLT(AVal(AInd(E5)), AVal(AInd(E4)))) THEN
                ! t = AInd(E5); AInd(E5) = AInd(E4); AInd(E4) = t
                EXCHANGE(AInd, E5, E4)
            END IF
            IF (COMPARE_GLT(AVal(AInd(E2)), AVal(AInd(E1)))) THEN
                ! t = AInd(E2); AInd(E2) = AInd(E1); AInd(E1) = t
                EXCHANGE(AInd, E2, E1)
            END IF
            IF (COMPARE_GLT(AVal(AInd(E4)), AVal(AInd(E2)))) THEN
                ! t = AInd(E4); AInd(E4) = AInd(E2); AInd(E2) = t
                EXCHANGE(AInd, E4, E2)
            END IF

            IF (COMPARE_GLT(AVal(AInd3), AVal(AInd(E2)))) THEN
                IF (COMPARE_GLT(AVal(AInd3), AVal(AInd(E1)))) THEN
                    AInd(E3) = AInd(E2)
                    AInd(E2) = AInd(E1)
                    AInd(E1) = AInd3
                ELSE
                    AInd(E3) = AInd(E2)
                    AInd(E2) = AInd3
                END IF
            ELSEIF (COMPARE_GLT(AVal(AInd(E4)), AVal(AInd3))) THEN
                IF (COMPARE_GLT(AVal(AInd(E5)), AVal(AInd3))) THEN
                    AInd(E3) = AInd(E4)
                    AInd(E4) = AInd(E5)
                    AInd(E5) = AInd3
                ELSE
                    AInd(E3) = AInd(E4)
                    AInd(E4) = AInd3
                END IF
            END IF

            ! Pointers
            Lower = Low     ! The index of the last element of the left part
            Upper = iEnd    ! The index of the first element of the right part

            ! Partitioning with 2 pivots in case of different elements.
            IF (COMPARE_GLT(AVal(AInd(E1)), AVal(AInd(E2))) .AND. &
                COMPARE_GLT(AVal(AInd(E2)), AVal(AInd(E3))) .AND. &
                COMPARE_GLT(AVal(AInd(E3)), AVal(AInd(E4))) .AND. &
                COMPARE_GLT(AVal(AInd(E4)), AVal(AInd(E5)))) THEN

                ! Use the first and fifth of the five sorted elements as
                ! the pivots. These values are inexpensive approximation
                ! of tertiles. Note, that Pivot1 < Pivot2.
                PInd1 = AInd(E1)
                PInd2 = AInd(E5)

                ! The first and the last elements to be sorted are moved
                ! to the locations formerly occupied by the pivots. When
                ! partitioning is completed, the pivots are swapped back
                ! into their final positions, and excluded from the next
                ! subsequent sorting.
                AInd(E1) = AInd(Lower)
                AInd(E5) = AInd(Upper)

                ! Skip elements, which are less or greater than the pivots.
                Lower = Lower + 1
                DO WHILE (COMPARE_GLT(AVal(AInd(Lower)), AVal(PInd1)))
                    Lower = Lower + 1
                END DO
                Upper = Upper - 1
                DO WHILE (COMPARE_GLT(AVal(PInd2), AVal(AInd(Upper))))
                    Upper = Upper - 1
                END DO

                ! Backward 3-interval partitioning
                !
                !   left part                 central part          right part
                ! +------------------------------------------------------------+
                ! |  < Pivot1  |   ?   |  Pivot1 <= && <= Pivot2  |  > Pivot2  |
                ! +------------------------------------------------------------+
                !             ^       ^                            ^
                !             |       |                            |
                !           Lower     K                          Upper
                !
                ! Invariants:
                !
                !              all in (Low, Lower) < Pivot1
                !    Pivot1 <= all in (K, Upper)  <= Pivot2
                !              all in [Upper, iEnd) > Pivot2
                !
                ! Pointer K is the last index of ?-part
                Lower = Lower - 1
                Upper = Upper + 1
                K = Upper - 1
                DO WHILE (K > Lower)
                    AIndK = AInd(K)
                    IF (COMPARE_GLT(AVal(AIndK), AVal(PInd1))) THEN
                        ! Move AInd(K) to the left side
                        DO WHILE (Lower < K)
                            Lower = Lower + 1
                            IF (COMPARE_GLE(AVal(PInd1), AVal(AInd(Lower)))) THEN
                                IF (COMPARE_GLT(AVal(PInd2), AVal(AInd(Lower)))) THEN
                                    Upper = Upper - 1
                                    AInd(K) = AInd(Upper)
                                    AInd(Upper) = AInd(Lower)
                                ELSE
                                    AInd(K) = AInd(Lower)
                                END IF
                                AInd(Lower) = AIndK
                                EXIT
                            END IF
                        END DO
                    ELSEIF (COMPARE_GLT(AVal(PInd2), AVal(AIndK))) THEN
                        ! Move AInd(K) to the right side
                        Upper = Upper - 1
                        AInd(K) = AInd(Upper)
                        AInd(Upper) = AIndK
                    END IF
                    K = K - 1
                END DO

                ! Swap the pivots into their final positions.
                AInd(Low) = AInd(Lower)
                AInd(Lower) = PInd1
                AInd(iEnd) = AInd(Upper)
                AInd(Upper) = PInd2

                ! Sort non-left parts recursively ,
                ! excluding known pivots.
                CALL Quick_DualPivot(AVal, AInd, IOR(Bits, 1), Lower + 1, Upper)
                CALL Quick_DualPivot(AVal, AInd, IOR(Bits, 1), Upper + 1, High)

            ELSE    ! Use single pivot in case of many equal elements

                ! Use the third of the five sorted elements as the pivot.
                ! This value is inexpensive approximation of the median.
                PInd = AInd(E3)

                ! The first element to be sorted is moved to the
                ! location formerly occupied by the pivot. After
                ! completion of partitioning the pivot is swapped
                ! back into its final position, and excluded from
                ! the next subsequent sorting.
                AInd(E3) = AInd(Lower)

                ! Traditional 3-way (Dutch National Flag) partitioning
                !
                !   left part                 central part    right part
                ! +------------------------------------------------------+
                ! |   < Pivot   |     ?     |   == Pivot   |   > Pivot   |
                ! +------------------------------------------------------+
                !              ^           ^                ^
                !              |           |                |
                !            Lower         K              Upper
                !
                ! Invariants:
                !
                !   all in (Low, Lower) < Pivot
                !   all in (K, Upper)  == Pivot
                !   all in [Upper, iEnd) > Pivot
                !
                ! Pointer K is the last index of ?-part
                Upper = Upper + 1
                K = Upper - 1
                DO WHILE (K > Lower)
                    AIndK = AInd(K)

                    IF (AVal(AIndK) /= AVal(PInd)) THEN
                        AInd(K) = PInd

                        IF (COMPARE_GLT(AVal(AIndK), AVal(PInd))) THEN
                            ! Move AInd(K) to the left side
                            Lower = Lower + 1
                            DO WHILE (COMPARE_GLT(AVal(AInd(Lower)), AVal(PInd)))
                                Lower = Lower + 1
                            END DO

                            IF (COMPARE_GLT(AVal(PInd), AVal(AInd(Lower)))) THEN
                                Upper = Upper - 1
                                AInd(Upper) = AInd(Lower)
                            END IF
                            AInd(Lower) = AIndK
                        ELSE
                            ! AK > Pivot - Move AInd(K) to the right side
                            Upper = Upper - 1
                            AInd(Upper) = AIndK
                        END IF
                    END IF
                    K = K - 1
                END DO

                ! Swap the pivot into its final position.
                AInd(Low) = AInd(Lower)
                AInd(Lower) = PInd

                ! Sort the right part (possibly in parallel), excluding
                ! known pivot. All elements from the central part are
                ! equal and therefore already sorted.
                CALL Quick_DualPivot(AVal, AInd, IOR(Bits, 1), Upper, High)
            END IF
            High = Lower    ! Iterate along the left part

        END DO

        RETURN

    END SUBROUTINE Quick_DualPivot

    !******************************************************************

    SUBROUTINE Insertion_Java(AVal, AInd, Low, High)

    !** PURPOSE OF THIS SUBROUTINE
        ! To Sort the specified range of the array using insertion sort.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)  ! array values
        tIndex,      INTENT(INOUT)  :: AInd(0:) ! array indices
        tIndex,      INTENT(IN)     :: Low      ! the index of the first element, inclusive, to be sorted
        tIndex,      INTENT(IN)     :: High     ! the index of the last element, exclusive, to be sorted

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: I, K
        tIndex          :: AIndI

    !** FLOW:

        K = Low + 1
        DO WHILE (K < High)
            I = K
            AIndI = AInd(I)
            IF (COMPARE_GLT(AVal(AIndI), AVal(AInd(I-1)))) THEN
                I = I - 1
                DO WHILE ((I >= Low).AND.(COMPARE_GLT(AVal(AIndI), AVal(AInd(I)))))
                    AInd(I+1) = AInd(I)
                    I = I - 1
                END DO
                AInd(I+1) = AIndI
            END IF
            K = K + 1
        END DO

        RETURN

    END SUBROUTINE Insertion_Java

    !******************************************************************

    SUBROUTINE Insertion_Mixed(AVal, AInd, Left, Last, Right)

    !** PURPOSE OF THIS SUBROUTINE
        ! To sort the specified range of the array using mixed insertion sort.
        ! Mixed insertion sort is combination of simple insertion sort,
        ! pin insertion sort and pair insertion sort.
        ! In the context of Dual-Pivot Quicksort, the pivot element
        ! from the left part plays the role of sentinel, because it
        ! is less than any elements from the given part. Therefore,
        ! expensive check of the left range can be skipped on each
        ! iteration unless it is the leftmost call.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)  ! array values
        tIndex,      INTENT(INOUT)  :: AInd(0:) ! array indices
        tIndex,      INTENT(IN)     :: Left     ! the index of the first element, inclusive, to be sorted
        tIndex,      INTENT(IN)     :: Last     ! the index of the last element for simple insertion sort
        tIndex,      INTENT(IN)     :: Right    ! the index of the last element, exclusive, to be sorted

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: AIndI, AInd1, AInd2, Pin
        tIndex          :: Low, End, High
        tIndex          :: I, P

    !** FLOW:

        ! initialize
        Low = Left
        End = Last
        High = Right

        IF (End == High) THEN

            ! Invoke simple insertion sort on tiny array.
            Low = Low + 1
            DO WHILE (Low < End)
                I = Low
                AIndI = AInd(I)

                I = I - 1
                DO WHILE (COMPARE_GLT(AVal(AIndI), AVal(AInd(I))))
                    AInd(I+1) = AInd(I)
                    I = I - 1
                END DO

                AInd(I+1) = AIndI
                Low = Low + 1
            END DO

        ELSE

            ! Start with pin insertion sort on small part.
            !
            ! Pin insertion sort is extended simple insertion sort.
            ! The main idea of this sort is to put elements larger
            ! than an element called pin to the end of array (the
            ! proper area for such elements). It avoids expensive
            ! movements of these elements through the whole array.
            Pin = AInd(End)

            P = High
            Low = Low + 1
            DO WHILE (Low < End)

                I = Low
                AIndI = AInd(I)

                IF (COMPARE_GLT(AVal(AIndI), AVal(AInd(I-1)))) THEN   ! Small element

                    ! Insert small element into sorted part.
                    AInd(I) = AInd(I-1)
                    I = I - 2

                    DO WHILE (COMPARE_GLT(AVal(AIndI), AVal(AInd(I))))
                        AInd(I+1) = AInd(I)
                        I = I - 1
                    END DO
                    AInd(I+1) = AIndI

                ELSEIF ((P > I).AND.(COMPARE_GLT(AVal(Pin), AVal(AIndI)))) THEN    ! Large element

                    ! Find element smaller than pin.
                    P = P - 1
                    DO WHILE (COMPARE_GLT(AVal(Pin), AVal(AInd(P))))
                        P = P - 1
                    END DO

                    ! Swap it with large element.
                    IF (P > I) THEN
                        AIndI = AInd(P)
                        AInd(P) = AInd(I)
                    END IF

                    ! Insert small element into sorted part.
                    I = I - 1
                    DO WHILE (COMPARE_GLT(AVal(AIndI), AVal(AInd(I))))
                        AInd(I+1) = AInd(I)
                        I = I - 1
                    END DO
                    AInd(I+1) = AIndI
                END IF
                Low = Low + 1
            END DO

            ! Continue with pair insertion sort on remain part.
            DO WHILE (Low < High)

                I = Low
                AInd1 = AInd(I)
                Low = Low + 1
                AInd2 = AInd(Low)

                ! Insert two elements per iteration: at first, insert the
                ! larger element and then insert the smaller element, but
                ! from the position where the larger element was inserted.
                IF (COMPARE_GLT(AVal(AInd2), AVal(AInd1))) THEN

                    I = I - 1
                    DO WHILE (COMPARE_GLT(AVal(AInd1), AVal(AInd(I))))
                        AInd(I+2) = AInd(I)
                        I = I - 1
                    END DO
                    AInd(I+2) = AInd1
                    DO WHILE (COMPARE_GLT(AVal(AInd2), AVal(AInd(I))))
                        AInd(I+1) = AInd(I)
                        I = I - 1
                    END DO
                    AInd(I+1) = AInd2

                ELSEIF (COMPARE_GLT(AVal(AInd1), AVal(AInd(I-1)))) THEN

                    I = I - 1
                    DO WHILE (COMPARE_GLT(AVal(AInd2), AVal(AInd(I))))
                        AInd(I+2) = AInd(I)
                        I = I - 1
                    END DO
                    AInd(I+2) = AInd2
                    DO WHILE (COMPARE_GLT(AVal(AInd1), AVal(AInd(I))))
                        AInd(I+1) = AInd(I)
                        I = I - 1
                    END DO
                    AInd(I+1) = AInd1

                END IF
                Low = Low + 1

            END DO

        END IF

        RETURN

    END SUBROUTINE Insertion_Mixed

    !******************************************************************

    FUNCTION TryMergeRuns(AVal, AInd, Low, Size) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE
        ! To try to sort the specified range of the array.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)  ! array values
        tIndex,      INTENT(INOUT)  :: AInd(0:) ! array indices
        tIndex,      INTENT(IN)     :: Low      ! the index of the first element to be sorted
        tIndex,      INTENT(IN)     :: Size     ! the array size
        tLogical                    :: Flag     ! true if finally sorted, false otherwise

    !** SUBROUTINE PARAMETER DECLARATIONS:
        ! Min size of the first run to continue with scanning.
        tInteger, PARAMETER     :: MIN_FIRST_RUN_SIZE = 16
        ! Min factor for the first runs to continue scanning.
        tInteger, PARAMETER     :: MIN_FIRST_RUNS_FACTOR = 7
        ! Max capacity of the index array for tracking runs.
        tInteger, PARAMETER     :: MAX_RUN_CAPACITY = ISHFT(5, 10)
        ! Min number of runs, required by parallel merging.
        tInteger, PARAMETER     :: MIN_RUN_COUNT = 4

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! The run array is constructed only if initial runs are
        ! long enough to continue, run[i] then holds start index
        ! of the i-th sequence of elements in non-descending order.
        tIndex, ALLOCATABLE :: Run(:)
        tIndex, ALLOCATABLE :: BInd(:)              ! work space
        tIndex              :: Temp, AIndK
        tIndex              :: RunSize, NewRunSize
        tIndex              :: High, Last, Count, Offset
        tIndex              :: I, J, K, Dummy

    !** FLOW:

        ! initialize
        High = Low + Size
        Count = 1
        Last = Low

        ! Identify all possible runs.
        K = Low + 1
        DO WHILE (K < High)

            ! Find the end index of the current run.
            IF (COMPARE_GLT(AVal(AInd(K-1)), AVal(AInd(K)))) THEN

                ! Identify ascending sequence
                K = K + 1
                DO WHILE ((K < High).AND.(COMPARE_GLE(AVal(AInd(K-1)), AVal(AInd(K)))))
                    K = K + 1
                END DO

            ELSEIF (COMPARE_GLT(AVal(AInd(K)), AVal(AInd(K-1)))) THEN

                ! Identify descending sequence
                K = K + 1
                DO WHILE ((K < High).AND.(COMPARE_GLE(AVal(AInd(K)), AVal(AInd(K-1)))))
                    K = K + 1
                END DO

                ! Reverse into ascending order
                I = Last
                J = K - 1
                DO WHILE ((I < J).AND.(COMPARE_GLT(AVal(AInd(J)), AVal(AInd(I)))))
                    ! AIndI = AInd(I); AInd(I) = AInd(J); AInd(J) = AIndI
                    EXCHANGE(AInd, I, J)
                    I = I + 1
                    J = J - 1
                END DO
            ELSE
                ! Identify constant sequence
                AIndK = AInd(K)
                K = K + 1
                DO WHILE ((K < High).AND.(AVal(AIndK) == AVal(AInd(K))))
                    K = K + 1
                END DO

                IF (K < High) CYCLE
            END IF

            ! Check special cases.
            IF (.NOT.ALLOCATED(Run)) THEN

                IF (K == High) THEN
                    ! The array is monotonous sequence,
                    ! and therefore already sorted.
                    Flag = TrueVal
                    RETURN
                END IF

                IF (K - Low < MIN_FIRST_RUN_SIZE) THEN
                    ! The first run is too small
                    ! to proceed with scanning.
                    Flag = FalseVal
                    RETURN
                END IF

                ! run = new int[((size >> 10) | 0x7F) & 0x3FF]
                ! RunSize = IAND(IOR(SHIFTA(Size, 10), INT(Z'0000007F')), INT(Z'000003FF'))
                RunSize = IAND(IOR(SHIFTA(Size, 10), 127), 1023)
                CALL MemAlloc(Run, RunSize, StartID=0_kIndex)
                Run(0) = Low

            ELSEIF (COMPARE_GLT(AVal(AInd(Last)), AVal(AInd(Last-1)))) THEN

                IF (Count > SHIFTA((K-Low), MIN_FIRST_RUNS_FACTOR)) THEN
                    ! The first runs are not long
                    ! enough to continue scanning.
                    Flag = FalseVal
                    RETURN
                END IF

                Count = Count + 1
                IF (Count == MAX_RUN_CAPACITY) THEN
                    ! Array is not highly structured.
                    Flag = FalseVal
                    RETURN
                END IF

                IF (Count == RunSize) THEN
                    ! Increase capacity of index array.
                    ! Run = Arrays.copyOf(Run, Count << 1);
                    NewRunSize = SHIFTL(Count, 1)
                    CALL MemResize(Run, NewRunSize)
                    RunSize = NewRunSize
                END IF
            END IF
            Last = K
            Run(Count) = Last
        END DO

        ! Merge runs of highly structured array.
        IF (Count > 1) THEN
            Offset = Low
            ALLOCATE(BInd(0:Size-1))
            Dummy = Java_Merge_Runs_Rank(AVal, AInd, BInd, Offset, 1, Run, 0, Count)
            DEALLOCATE(BInd)
        END IF

        Flag = TrueVal

        RETURN

    END FUNCTION TryMergeRuns

    !**********************************************************************

END SUBROUTINE Java_Rank

!**********************************************************************

MODULE SUBROUTINE PDQ_Rank(AVal, AInd)

!** PURPOSE OF THIS SUBROUTINE
    ! To determine indices such that they sort the specified array into the desired
    ! order using the PDQSort algorithm [1, 2].

!** REFEENCE
!   [1] <a href="https://arxiv.org/abs/2106.05123">Peters, O.R.L. 2021.
!       Pattern-defeating Quicksort. </a> <br>
!   [2] <a href="https://github.com/orlp/pdqsort">pdqsort: Pattern-defeating
!       quicksort. Reference implementation. </a> <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)          ! array values
    tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal)) ! array indices

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! cutoff to insertion sort
    tInteger, PARAMETER     :: Insertion_CutOff     = 50
    tInteger, PARAMETER     :: Insertion_Threshold  = 44
    tInteger, PARAMETER     :: kSmallSort_Threshold = 16
    ! Partitions above this size use Tukey's ninther to select the pivot
    tInteger, PARAMETER     :: Ninther_Threshold    = 128
    ! When we detect an already sorted partition, attempt an insertion sort that allows this
    ! amount of element moves before giving up.
    tInteger, PARAMETER     :: Partial_Insertion_Limit = 8

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: MaxDepth     ! maximum depth used to switch partitioning routine
    tIndex          :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(AVal, KIND=kIndex)

    ! initialize the indices
    CALL Index_Init(NA, AInd)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Insertion_CutOff) THEN
        CALL DualInsert_Guarded(AVal, AInd, 1, NA)
        RETURN
    END IF

    ! determine maximum depth based on the size of the input array
    MaxDepth = MaximumDepth(NA)

    ! perform quick sort
    CALL PDQ_Recur(AVal, AInd, 0, NA, MaxDepth, TrueVal)

    RETURN

    CONTAINS

    RECURSIVE SUBROUTINE PDQ_Recur(AVal,AInd,Left,Right,BadAllowed,LeftMost)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)  ! array values
        tIndex,      INTENT(INOUT)  :: AInd(0:) ! array indices
        tIndex,      INTENT(IN)     :: Left
        tIndex,      INTENT(IN)     :: Right
        tIndex,      INTENT(IN)     :: BadAllowed
        tLogical,    INTENT(IN)     :: LeftMost

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: Temp
        tIndex          :: PivotIndx
        tIndex          :: Size, HalfSize, LSize, RSize
        tIndex          :: HSM1, HSP1, LSOver4, RSOver4
        tIndex          :: iBegin, iEnd, MaxDepth
        tLogical        :: Already_Partitioned, Highly_Unbalanced
        tLogical        :: IsLeftMost

    !** FLOW:

        ! initialize
        iBegin = Left
        iEnd   = Right
        MaxDepth = BadAllowed
        IsLeftMost = LeftMost

        ! Use a do loop for tail recursion elimination.
        DO

            Size = iEnd - iBegin

            IF (Size < Insertion_CutOff) THEN
                ! check whether the specified range is the left most range of the given array
                IF (IsLeftMost) THEN
                    CALL DualInsert_Guarded(AVal, AInd, iBegin+1, iEnd)
                ELSE
                    CALL DualInsert_UnGuarded(AVal, AInd, iBegin+1, iEnd)
                END IF
                RETURN
            END IF

            ! Choose pivot as median of 3 or pseudomedian of 9.
            HalfSize = Size/2
            IF (Size > Ninther_Threshold) THEN
                HSM1 = HalfSize - 1
                HSP1 = HalfSize + 1
                CALL Rank_3_Items_Base0(AVal, AInd, iBegin, iBegin+HalfSize, iEnd-1)
                CALL Rank_3_Items_Base0(AVal, AInd, iBegin+1, iBegin+HSM1, iEnd-2)
                CALL Rank_3_Items_Base0(AVal, AInd, iBegin+2, iBegin+HSP1, iEnd-3)
                CALL Rank_3_Items_Base0(AVal, AInd, iBegin+HSM1, iBegin+HalfSize, iBegin+HSP1)
                EXCHANGE(AInd, iBegin, iBegin+HalfSize)
            ELSE
                CALL Rank_3_Items_Base0(AVal, AInd, iBegin+HalfSize, iBegin, iEnd-1)
            END IF

            ! If A(begin - 1) is the end of the right partition of a previous partition operation
            ! there is no element in [begin, end) that is smaller than A(begin - 1). Then if our
            ! pivot compares equal to A(begin - 1) we change strategy, putting equal elements in
            ! the left partition, greater elements in the right partition. We do not have to
            ! recurse on the left partition, since it's sorted (all equal).
            IF ((.NOT.IsLeftMost).AND.(COMPARE_GLE(AVal(AInd(iBegin)), AVal(AInd(iBegin-1))))) THEN
                iBegin = PartitionLeft(AVal, AInd, iBegin, iEnd) + 1
                CYCLE
            END IF

            ! Partition and get results
            Already_Partitioned = PartitionRight(AVal, AInd, iBegin, iEnd, PivotIndx)

            ! Check for a highly unbalanced partition.
            LSize = PivotIndx - iBegin
            RSize = iEnd - (PivotIndx + 1)
            Highly_Unbalanced = (LSize < Size/8).OR.(RSize < Size/8)

            ! If we got a highly unbalanced partition we shuffle elements to break many patterns.
            IF (Highly_Unbalanced) THEN

                ! If we had too many bad partitions, switch to heapsort to guarantee O(n log n).
                MaxDepth = MaxDepth - 1
                IF (MaxDepth == 0) THEN
                    CALL HeapIndex_Java(AVal, AInd, iBegin, iEnd)
                    RETURN
                END IF

                IF (LSize >= Insertion_Threshold) THEN
                    LSOver4 = LSize / 4
                    EXCHANGE(AInd, iBegin,      iBegin+LSOver4)
                    EXCHANGE(AInd, PivotIndx-1, PivotIndx-LSOver4)

                    IF (LSize > Ninther_Threshold) THEN
                        EXCHANGE(AInd, iBegin+1,    iBegin+(LSOver4+1))
                        EXCHANGE(AInd, iBegin+2,    iBegin+(LSOver4+2))
                        EXCHANGE(AInd, PivotIndx-2, PivotIndx-(LSOver4+1))
                        EXCHANGE(AInd, PivotIndx-3, PivotIndx-(LSOver4+2))
                    END IF
                END IF

                IF (RSize >= Insertion_Threshold) THEN
                    RSOver4 = RSize / 4
                    EXCHANGE(AInd, PivotIndx+1, PivotIndx+(1+RSOver4))
                    EXCHANGE(AInd, iEnd-1,      iEnd-RSOver4)

                    IF (RSize > Ninther_Threshold) THEN
                        EXCHANGE(AInd, PivotIndx+2, PivotIndx+(2+RSOver4))
                        EXCHANGE(AInd, PivotIndx+3, PivotIndx+(3+RSOver4))
                        EXCHANGE(AInd, iEnd-2,      iEnd-(1+RSOver4))
                        EXCHANGE(AInd, iEnd-3,      iEnd-(2+RSOver4))
                    END IF
                END IF
            ELSE
                ! If we were decently balanced and we tried to sort an already partitioned
                ! sequence try to use insertion sort.
                IF (Already_Partitioned .AND. Insertion_Partial(AVal, AInd, iBegin, PivotIndx) &
                                        .AND. Insertion_Partial(AVal, AInd, PivotIndx+1, iEnd)) THEN
                    RETURN
                END IF
            END IF

            ! Sort the left partition first using recursion and do tail recursion elimination for
            ! the right-hand partition.
            CALL PDQ_Recur(AVal, AInd, iBegin, PivotIndx, MaxDepth, IsLeftMost)
            iBegin = PivotIndx + 1
            IsLeftMost = FalseVal

        END DO

        RETURN

    END SUBROUTINE PDQ_Recur

    !*****************************************************************

    FUNCTION PartitionRight(AVal,AInd,Left,Right,PivotIndx) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE
        ! To partition the range [Left, Right] around a pivot A(Left).
        ! Elements equal to the pivot are put in the right-hand partition.
        ! Returns the position of the pivot after partitioning and whether
        ! the passed sequence already was correctly partitioned.  Assumes that
        ! the pivot is a median of at least 3 elements and that the range
        ! [Left, Right] is at least insertion_sort_threshold long.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)      ! array values
        tIndex,      INTENT(INOUT)  :: AInd(0:)     ! array indices
        tIndex,      INTENT(IN)     :: Left         ! the start of the range
        tIndex,      INTENT(IN)     :: Right        ! the end of the range
        tIndex,      INTENT(OUT)    :: PivotIndx    ! position of the pivot after partitioning
        tLogical                    :: Flag         ! true if the passed sequence already was correctly partitioned

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: PInd, Temp
        tIndex          :: First, Last, LastMinus1
        tLogical        :: Already_Partitioned

    !** FLOW:

        ! set pivot value
        PInd = AInd(Left)

        ! initialize
        First = Left
        Last  = Right

        ! Find the first element greater than or equal to the pivot
        ! (the median of 3 guarantees this exists).
        First = First + 1
        DO WHILE (COMPARE_GLT(AVal(AInd(First)), AVal(PInd)))
            First = First + 1
        END DO

        ! Find the first element strictly smaller than the pivot.
        ! We have to guard this search if there was no element before A(First).
        IF ((First-1) == Left) THEN
            LastMinus1 = Last - 1
            DO WHILE ((First < Last).AND.(COMPARE_GLE(AVal(PInd), AVal(AInd(LastMinus1)))))
                Last = LastMinus1
                LastMinus1 = Last - 1
            END DO
            Last = LastMinus1
        ELSE
            Last = Last - 1
            DO WHILE (COMPARE_GLE(AVal(PInd), AVal(AInd(Last))))
                Last = Last - 1
            END DO
        END IF

        ! If the first pair of elements that should be swapped to partition are the
        ! same element, the passed sequence already was correctly partitioned.
        Already_Partitioned = (First >= Last)

        ! Keep swapping pairs of elements that are on the wrong side of the pivot.
        ! Previously swapped pairs guard the searches, which is why the first
        ! iteration is special-cased above.
        DO WHILE (First < Last)
            EXCHANGE(AInd, First, Last)
            First = First + 1
            DO WHILE (COMPARE_GLT(AVal(AInd(First)), AVal(PInd)))
                First = First + 1
            END DO
            Last = Last - 1
            DO WHILE (COMPARE_GLE(AVal(PInd), AVal(AInd(Last))))
                Last = Last - 1
            END DO
        END DO

        ! Put the pivot in the right place.
        PivotIndx = First - 1
        AInd(Left) = AInd(PivotIndx)
        AInd(PivotIndx) = PInd

        Flag = Already_Partitioned

        RETURN

    END FUNCTION PartitionRight

    !*****************************************************************

    FUNCTION PartitionLeft(AVal,AInd,Left,Right) RESULT(PivotIndx)

    !** PURPOSE OF THIS SUBROUTINE
        ! Similar to routine 'PartitionRight', except elements equal to
        ! the pivot are put to the left of the pivot and it doesn't check
        ! or return if the passed sequence already was partitioned.
        ! Since this is rarely used (the many equal case), and in that case
        ! PDQSort already has O(n) performance, no block quicksort is applied
        ! here for simplicity.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)      ! array values
        tIndex,      INTENT(INOUT)  :: AInd(0:)     ! array indices
        tIndex,      INTENT(IN)     :: Left         ! the start of the range
        tIndex,      INTENT(IN)     :: Right        ! the end of the range
        tIndex                      :: PivotIndx    ! position of the pivot after partitioning

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: PInd, Temp
        tIndex          :: First, Last, FirstPlus1

    !** FLOW:

        ! set pivot value
        PInd = AInd(Left)

        ! initialize
        First = Left
        Last  = Right

        Last = Last - 1
        DO WHILE (COMPARE_GLT(AVal(PInd), AVal(AInd(Last))))
            Last = Last - 1
        END DO

        IF ((Last+1) == Right) THEN
            FirstPlus1 = First + 1
            DO WHILE ((First < Last).AND.(COMPARE_GLE(AVal(AInd(FirstPlus1)), AVal(PInd))))
                First = FirstPlus1
                FirstPlus1 = First + 1
            END DO
            First = FirstPlus1
        ELSE
            First = First + 1
            DO WHILE (COMPARE_GLE(AVal(AInd(First)), AVal(PInd)))
                First = First + 1
            END DO
        END IF

        DO WHILE (First < Last)
            EXCHANGE(AInd, First, Last)
            Last = Last - 1
            DO WHILE (COMPARE_GLT(AVal(PInd), AVal(AInd(Last))))
                Last = Last - 1
            END DO
            First = First + 1
            DO WHILE (COMPARE_GLE(AVal(AInd(First)), AVal(PInd)))
                First = First + 1
            END DO
        END DO

        ! Put the pivot in the right place.
        PivotIndx = Last
        AInd(Left) = AInd(PivotIndx)
        AInd(PivotIndx) = PInd

        RETURN

    END FUNCTION PartitionLeft

    !*****************************************************************

    FUNCTION Insertion_Partial(AVal,AInd,Low,High) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE
        ! To sort the specified range of the array using insertion sort.  Will
        ! return FalseVal if more than partial limit elements were moved and
        ! abort sorting.  Otherwise, it will successfully sort and return TrueVal

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)  ! array values
        tIndex,      INTENT(INOUT)  :: AInd(0:) ! array indices
        tIndex,      INTENT(IN)     :: Low      ! the index of the first element, inclusive, to be sorted
        tIndex,      INTENT(IN)     :: High     ! the index of the last element, exclusive, to be sorted
        tLogical                    :: Flag

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: I, K, Limit
        tIndex          :: AIndI

    !** FLOW:

        Flag = TrueVal

        Limit = 0
        K = Low + 1
        DO WHILE (K < High)
            I = K
            AIndI = AInd(I)
            IF (COMPARE_GLT(AVal(AIndI), AVal(AInd(I-1)))) THEN
                I = I - 1
                DO WHILE ((I >= Low).AND.(COMPARE_GLT(AVal(AIndI), AVal(AInd(I)))))
                    AInd(I+1) = AInd(I)
                    I = I - 1
                END DO
                AInd(I+1) = AIndI
                Limit = Limit + (K-I)
            END IF
            K = K + 1
            IF (Limit > Partial_Insertion_Limit) THEN
                Flag = FalseVal
                EXIT
            END IF
        END DO

        RETURN

    END FUNCTION Insertion_Partial

    !*****************************************************************

END SUBROUTINE PDQ_Rank

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                     STABLE DRIVER ROUTINES
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE SUBROUTINE Tim_Rank(AVal, AInd)

!** PURPOSE OF THIS SUBROUTINE
    ! To determine indices such that they sort the specified array into the desired
    ! order using the Timsort algorithm [1].

!** REFEENCE
!   [1] <a href="https://bugs.python.org/file4451/timsort.txt">Timsort by Tim Peters. </a> <br>
!   [2] <a href="https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/util/TimSort.java">
!       Java's Timsort class. </a> <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument,      INTENT(IN)      :: AVal(:)          ! array values
    tIndex, TARGET, INTENT(INOUT)   :: AInd(SIZE(AVal)) ! array indices

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER     :: Insertion_CutOff     = 32
    ! This is the minimum sized sequence that will be merged.  Shorter
    ! sequences will be lengthened by calling binarysort.  If the entire
    ! array is less than this length, no merges will be performed.
    tInteger, PARAMETER     :: Default_MinMerge     = 64
    ! When we get into galloping mode, we stay there until both runs win less
    ! often than MIN_GALLOP consecutive times.
    tInteger, PARAMETER     :: Default_MinGallop    = 7
    ! Maximum initial size of tmp array, which is used for merging.  The array
    ! can grow to accommodate demand.
    tInteger, PARAMETER     :: Default_TmpStorage   = 256

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! This controls when we get *into* galloping mode.  It is initialized
    ! to MIN_GALLOP.  The mergeLo and mergeHi methods nudge it higher for
    ! random data, and lower for highly structured data.
    tIndex                      :: Minimum_Gallop
    ! A stack of pending runs yet to be merged.  Run i starts at
    ! address base[i] and extends for len[i] elements.  It's always
    ! true (so long as the indices are in bounds) that:
    !
    !     runBase[i] + runLen[i] == runBase[i + 1]
    !
    ! so we could cut the storage for this, but it's a minor amount,
    ! and keeping all the info explicit simplifies the code.
    tIndex                      :: StackSize            ! Number of pending runs on stack
    tIndex,         ALLOCATABLE :: RunBase(:)
    tIndex,         ALLOCATABLE :: RunLen(:)
    tIndex, TARGET, ALLOCATABLE :: ATmp(:)              ! Temp storage for merges
    tIndex,         POINTER     :: APtr(:) => NULL()    ! pointer to the array to be sorted
    ! local variables used only in this main routine
    tIndex                      :: Low, High, nRemains
    tIndex                      :: nRun, MinRun, ForceRun
    tIndex                      :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(AVal, KIND=kIndex)

    ! initialize the indices
    CALL Index_Init(NA, AInd)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Insertion_CutOff) THEN
        CALL Insert_Guarded(AVal, AInd, 1, NA)
        RETURN
    END IF

    ! initialize
    Low = 0
    High = NA
    nRemains = High - Low
    nRun     = 0

    ! initialize working variables
    CALL InitWrkVar()

    ! March over the array once, left to right, finding natural runs,
    ! extending short natural runs to minRun elements, and merging runs
    ! to maintain stack invariant.
    MinRun = MinRunLength(nRemains)

    DO
        ! identify next run
        nRun = CountRunAndMakeAscending(AInd, Low, High)

        IF (nRun < MinRun) THEN
            ! If run is short, extend to min(MinRun, nRemains)
            ForceRun = nRemains
            IF (ForceRun > MinRun) ForceRun = MinRun
            CALL Insert_Guarded(AVal, AInd, Low+1, Low+ForceRun)
            nRun = ForceRun
        END IF

        ! Push new run into the pending stack and merge if necessary
        CALL PushRun(Low, nRun)
        CALL MergeCollapse()

        ! advance to next run
        Low = Low + nRun
        nRemains = nRemains - nRun
        IF (nRemains == 0) EXIT

    END DO

    ! Merge all remaining runs to complete the sorting task
    CALL MergeForceCollapse()

    ! free memory
    CALL FreeWrkVar()

    RETURN

CONTAINS

    SUBROUTINE InitWrkVar()

    !** PURPOSE OF THIS SUBROUTINE
	    ! To initialize working variables.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        ! na

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: Length
        tIndex          :: tLen
        tIndex          :: StackLen

    !** FLOW:

        ! set pointer to array to be sorted
        APtr(0:) => AInd

        ! set initial minimum gallop
        Minimum_Gallop = Default_MinGallop

        ! set initial values of main-procedure variables
        StackSize  = 0

        ! Allocate temp storage (which may be increased later if necessary)
        Length = SIZE(AInd)
        IF (Length < 2*Default_TmpStorage) THEN
            tLen = SHIFTR(Length,1)
        ELSE
            tLen = Default_TmpStorage
        END IF
        ALLOCATE(ATmp(0:tLen-1))
        ! Allocate runs-to-be-merged stack (which cannot be expanded).  The
        ! stack length requirements are described in listsort.txt.  The C
        ! version always uses the same stack length (85), but this was
        ! measured to be too expensive when sorting "mid-sized" arrays (e.g.,
        ! 100 elements) in Java.  Therefore, we use smaller (but sufficiently
        ! large) stack lengths for smaller arrays.  The "magic numbers" in the
        ! computation below must be changed if MIN_MERGE is decreased.  See
        ! the MIN_MERGE declaration above for more information.
        ! The maximum value of 49 allows for an array up to length
        ! Integer.MAX_VALUE-4, if array is filled by the worst case stack size
        ! increasing scenario. More explanations are given in section 4 of:
        ! http://envisage-project.eu/wp-content/uploads/2015/02/sorting.pdf
        IF (Length < 120) THEN
            StackLen = 5
        ELSEIF (Length < 1542) THEN
            StackLen = 10
        ELSEIF (Length < 119151) THEN
            StackLen = 24
        ELSE
            ! the following number is good for 2**31-5
            StackLen = 49
        END IF
        ALLOCATE(RunBase(0:StackLen-1))
        ALLOCATE(RunLen(0:StackLen-1))

        RETURN

    END SUBROUTINE InitWrkVar

    !******************************************************************

    SUBROUTINE FreeWrkVar()

    !** PURPOSE OF THIS SUBROUTINE
	    ! To free memory of working variables.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        ! na

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW:

        IF (ASSOCIATED(APtr))   NULLIFY(APtr)
        IF (ALLOCATED(ATmp))    DEALLOCATE(ATmp)
        IF (ALLOCATED(RunBase)) DEALLOCATE(RunBase)
        IF (ALLOCATED(RunLen))  DEALLOCATE(RunLen)

        RETURN

    END SUBROUTINE FreeWrkVar

    !******************************************************************

    FUNCTION MinRunLength(aLen) RESULT(MinRun)

    !** PURPOSE OF THIS SUBROUTINE
        ! To return the minimum acceptable run length for an array of the specified
        ! length. Natural runs shorter than this will be extended with binarySort.
        ! Roughly speaking, the computation is:
        !  If n < MIN_MERGE, return n (it's too small to bother with fancy stuff).
        !  Else if n is an exact power of 2, return MIN_MERGE/2.
        !  Else return an int k, MIN_MERGE/2 <= k <= MIN_MERGE, such that n/k
        !   is close to, but strictly less than, an exact power of 2.
        ! For the rationale, see listsort.txt.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex, INTENT(IN)  :: aLen     ! length of the array
        tIndex              :: MinRun   ! minimum acceptable run length

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: R, N

    !** FLOW:

        ! initialize
        N = aLen
        R = 0       ! Becomes 1 if any 1 bits are shifted off

        DO WHILE (N >= Default_MinMerge)
            R = IOR(R, IAND(N,1))
            N = SHIFTA(N,1)
        END DO
        MinRun = N + R

        RETURN

    END FUNCTION

    !******************************************************************

    FUNCTION CountRunAndMakeAscending(Indx, Low, Hi) RESULT(NRun)

    !** PURPOSE OF THIS SUBROUTINE
        ! To return the length of the run beginning at the specified position in
        ! the specified array and reverses the run if it is descending (ensuring
        ! that the run will always be ascending when the method returns).
        ! A run is the longest ascending sequence with:
        !    a[lo] <= a[lo + 1] <= a[lo + 2] <= ...
        ! or the longest descending sequence with:
        !    a[lo] >  a[lo + 1] >  a[lo + 2] >  ...
        ! For its intended use in a stable Mergesort, the strictness of the
        ! definition of "descending" is needed so that the call can safely
        ! reverse a descending sequence without violating stability.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex, INTENT(INOUT)   :: Indx(0:) ! array indices
        tIndex, INTENT(IN)      :: Low      ! first element in the range (inclusive)
        tIndex, INTENT(IN)      :: Hi       ! last element in the range
        tIndex                  :: NRun     ! length of the run

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: RunHi

    !** FLOW:

        ! initialize
        RunHi = Low + 1
        NRun  = 1

        IF (RunHi == Hi) RETURN

        ! Find end of run, and reverse range if descending
        IF (COMPARE_GLT(AVal(Indx(RunHi)), AVal(Indx(Low)))) THEN
            ! descending
            RunHi = RunHi + 1
            DO WHILE ((RunHi < Hi).AND.(COMPARE_GLT(AVal(Indx(RunHi)), AVal(Indx(RunHi-1)))))
                RunHi = RunHi + 1
            END DO
            CALL Reverse_Order_Base0(Indx, Low, RunHi-1)
        ELSE
            ! ascending
            RunHi = RunHi + 1
            DO WHILE ((RunHi < Hi).AND.(COMPARE_GLE(AVal(Indx(RunHi-1)), AVal(Indx(RunHi)))))
                RunHi = RunHi + 1
            END DO
        END IF

        NRun = RunHi - Low

        RETURN

    END FUNCTION CountRunAndMakeAscending

    !******************************************************************

    FUNCTION GallopLeft(Key, Indx, Base, Length, Hint) RESULT(Offset)

    !** PURPOSE OF THIS SUBROUTINE
        ! To locate the position at which to insert the specified key into the
        ! specified sorted range; if the range contains an element equal to key,
        ! returns the index of the leftmost equal element.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex, INTENT(IN)  :: Key      ! the key whose insertion point to be searched for
        tIndex, INTENT(IN)  :: Indx(0:) ! array indices
        tIndex, INTENT(IN)  :: Base     ! the index of the first element in the range
        tIndex, INTENT(IN)  :: Length   ! the length of the range(> 0)
        tIndex, INTENT(IN)  :: Hint     ! the index at which to begin the search, 0 <= hint < n.
        tIndex              :: Offset   ! the index where to insert the key value.

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: LastOffset, MaxOffset
        tIndex          :: Temp, M

    !** FLOW:

        ! initialize
        LastOffset = 0
        Offset     = 1

        IF (COMPARE_GLT(AVal(Indx(Base+Hint)), AVal(Key))) THEN
            ! gallop right until A(Base+Hint+LastOffset) < Key <= A(Base+Hint+Offset)
            MaxOffset = Length - Hint
            DO WHILE ((Offset < MaxOffset).AND.(COMPARE_GLT(AVal(Indx(Base+Hint+Offset)), AVal(Key))))
                LastOffset = Offset
                Offset = SHIFTL(Offset,1) + 1
                ! integer overflow
                IF (Offset <= 0) Offset = MaxOffset
            END DO
            IF (Offset > MaxOffset) Offset = MaxOffset
            ! Make offsets relative to base
            LastOffset = LastOffset + Hint
            Offset = Offset + Hint
        ELSE    ! Key <= A(Base+Hint)
            ! gallop left until A(Base+Hint-Offset) < Key <= A(Base+Hint-LastOffset)
            MaxOffset = Hint + 1
            DO WHILE ((Offset < MaxOffset).AND.(COMPARE_GLE(AVal(Key), AVal(Indx(Base+Hint-Offset)))))
                LastOffset = Offset
                Offset = SHIFTL(Offset,1) + 1
                ! integer overflow
                IF (Offset <= 0) Offset = MaxOffset
            END DO
            IF (Offset > MaxOffset) Offset = MaxOffset
            ! make offsets relative to base
            Temp = LastOffset
            LastOffset = Hint - Offset
            Offset = Hint - Temp
        END IF

        ! Now A(Base+LastOffset) < Key <= A(Base+Offset), so Key belongs somewhere
        ! in the range (Base + LastOffset, Base + Offset]. Do a binary search, with
        ! invariant A(Base+LastOffset-1] < Key <= A(Base+Offset).
        LastOffset = LastOffset + 1
        DO WHILE (LastOffset < Offset)
            M = LastOffset + SHIFTR((Offset-LastOffset),1)
            IF (COMPARE_GLT(AVal(Indx(Base+M)), AVal(Key))) THEN
                LastOffset = M + 1
            ELSE
                Offset = M
            END IF
        END DO

        RETURN

    END FUNCTION GallopLeft

    !******************************************************************

    FUNCTION GallopRight(Key, Indx, Base, Length, Hint) RESULT(Offset)

    !** PURPOSE OF THIS SUBROUTINE
        ! Like gallopLeft, except that if the range contains an element equal to
        ! key, gallopRight returns the index after the rightmost equal element.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex, INTENT(IN)  :: Key      ! the key whose insertion point to be searched for
        tIndex, INTENT(IN)  :: Indx(0:) ! array indices
        tIndex, INTENT(IN)  :: Base     ! the index of the first element in the range
        tIndex, INTENT(IN)  :: Length   ! the length of the range(> 0)
        tIndex, INTENT(IN)  :: Hint     ! the index at which to begin the search, 0 <= hint < n.
        tIndex              :: Offset   ! the index where to insert the key value.

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: LastOffset, MaxOffset
        tIndex          :: Temp, M

    !** FLOW:

        ! initialize
        LastOffset = 0
        Offset     = 1

        IF (COMPARE_GLT(AVal(Key), AVal(Indx(Base+Hint)))) THEN
            ! gallop left until A(Base+Hint-Offset) <= Key < A(Base+Hint-LastOffset)
            MaxOffset = Hint + 1
            DO WHILE ((Offset < MaxOffset).AND.(COMPARE_GLT(AVal(Key), AVal(Indx(Base+Hint-Offset)))))
                LastOffset = Offset
                Offset = SHIFTL(Offset,1) + 1
                ! integer overflow
                IF (Offset <= 0) Offset = MaxOffset
            END DO
            IF (Offset > MaxOffset) Offset = MaxOffset
            ! make offsets relative to base
            Temp = LastOffset
            LastOffset = Hint - Offset
            Offset = Hint - Temp
        ELSE    ! Key >= A(Base+Hint)
            ! gallop right until A(Base+Hint+LastOffset) <= Key < A(Base+Hint+Offset)
            MaxOffset = Length - Hint
            DO WHILE ((Offset < MaxOffset).AND.(COMPARE_GLE(AVal(Indx(Base+Hint+Offset)), AVal(Key))))
                LastOffset = Offset
                Offset = SHIFTL(Offset,1) + 1
                ! integer overflow
                IF (Offset <= 0) Offset = MaxOffset
            END DO
            IF (Offset > MaxOffset) Offset = MaxOffset
            ! Make offsets relative to base
            LastOffset = LastOffset + Hint
            Offset = Offset + Hint
        END IF

        ! Now A(Base+LastOffset) <= Key < A(Base+Offset), so Key belongs somewhere
        ! in the range (Base + LastOffset, Base + Offset]. Do a binary search, with
        ! invariant A(Base+LastOffset-1] <= Key < A(Base+Offset).
        LastOffset = LastOffset + 1
        DO WHILE (LastOffset < Offset)
            M = LastOffset + SHIFTR((Offset-LastOffset),1)
            IF (COMPARE_GLT(AVal(Key), AVal(Indx(Base+M)))) THEN
                Offset = M
            ELSE
                LastOffset = M + 1
            END IF
        END DO

        RETURN

    END FUNCTION GallopRight

    !******************************************************************

    SUBROUTINE PushRun(Base, Length)

    !** PURPOSE OF THIS SUBROUTINE
        ! To push the specified run onto the pending-run stack.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex, INTENT(IN)  :: Base     ! the index of the first element in the run
        tIndex, INTENT(IN)  :: Length   ! the number of elements in the run

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW:

        RunBase(Stacksize) = Base
        RunLen(Stacksize)  = Length
        Stacksize = Stacksize + 1

        RETURN

    END SUBROUTINE PushRun

    !******************************************************************

    SUBROUTINE MergeCollapse()

    !** PURPOSE OF THIS SUBROUTINE
        ! To examine the stack of runs waiting to be merged and merges adjacent runs
        ! until the stack invariants are reestablished:
        !     1. runLen[i - 3] > runLen[i - 2] + runLen[i - 1]
        !     2. runLen[i - 2] > runLen[i - 1]
        ! This method is called each time a new run is pushed onto the stack,
        ! so the invariants are guaranteed to hold for i < stackSize upon
        ! entry to the method.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        ! na

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: N

    !** FLOW:

        DO WHILE (StackSize > 1)
            N = StackSize - 2
            IF (((N > 0).AND.(RunLen(N-1) <= RunLen(N) + RunLen(N+1))).OR. &
                ((N > 1).AND.(RunLen(N-2) <= RunLen(N) + RunLen(N-1)))) THEN
                IF (RunLen(N-1) < RunLen(N+1)) N = N -1
            ELSEIF ((N < 0).OR.(RunLen(N) > RunLen(N+1))) THEN
                ! Invariant is established
                EXIT
            END IF
            CALL MergeAt(N)
        END DO

        RETURN

    END SUBROUTINE MergeCollapse

    !******************************************************************

    SUBROUTINE MergeForceCollapse()

    !** PURPOSE OF THIS SUBROUTINE
        ! To merge all runs on the stack until only one remains.  This method is
        ! called once, to complete the sort.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        ! na

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: N

    !** FLOW:

        DO WHILE (StackSize > 1)
            N = StackSize - 2
            IF ((N > 0).AND.(RunLen(N-1) < RunLen(N+1))) N = N -1
            CALL MergeAt(N)
        END DO

        RETURN

    END SUBROUTINE MergeForceCollapse

    !******************************************************************

    SUBROUTINE MergeAt(I)

    !** PURPOSE OF THIS SUBROUTINE
        ! To merge the two runs at stack indices i and i+1.  Run i must be
        ! the penultimate or ante-penultimate run on the stack.  In other words,
        ! I must be equal to stackSize-2 or stackSize-3.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex, INTENT(IN)  :: I

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: Base1, Len1
        tIndex          :: Base2, Len2
        tIndex          :: K

    !** FLOW:

        ! initialize
        Base1 = RunBase(I)
        Len1  = RunLen(I)
        Base2 = RunBase(I+1)
        Len2  = RunLen(I+1)

        ! Record the length of the combined runs; if i is the 3rd-last
        ! run now, also slide over the last run (which isn't involved
        ! in this merge).  The current run (i+1) goes away in any case.
        RunLen(I) = Len1 + Len2
        IF (I == StackSize-3) THEN
            RunBase(I+1) = RunBase(I+2)
            RunLen(I+1)  = RunLen(I+2)
        END IF
        StackSize = StackSize - 1

        ! Find where the first element of run2 goes in run1. Prior elements
        ! in run1 can be ignored (because they're already in place).
        K = GallopRight(APtr(Base2), APtr, Base1, Len1, 0)
        Base1 = Base1 + K
        Len1  = Len1 - K
        IF (Len1 == 0) RETURN

        ! Find where the last element of run1 goes in run2. Subsequent elements
        ! in run2 can be ignored (because they're already in place).
        Len2 = GallopLeft(APtr(Base1+Len1-1), APtr, Base2, Len2, Len2-1)
        IF (Len2 == 0) RETURN

        ! Merge remaining runs, using ATmp array with min(Len1, Len2) elements
        IF (Len1 <= Len2) THEN
            CALL EnsureCapacity(Len1)
            !CALL MergeLow(Base1, Len1, Base2, Len2, APtr, ATmp)
            CALL MergeLow_With_Gallop(Base1, Len1, Base2, Len2, APtr, ATmp)
        ELSE
            CALL EnsureCapacity(Len2)
            !CALL MergeHigh(Base1, Len1, Base2, Len2, APtr, ATmp)
            CALL MergeHigh_With_Gallop(Base1, Len1, Base2, Len2, APtr, ATmp)
        END IF

        RETURN

    END SUBROUTINE MergeAt

    !******************************************************************

    SUBROUTINE MergeLow(Base1, Length1, Base2, Length2, Indx, Tmp)

    !** PURPOSE OF THIS SUBROUTINE
        ! To merge two adjacent runs in place, in a stable fashion.  The first
        ! element of the first run must be greater than the first element of the
        ! second run (a[base1] > a[base2]), and the last element of the first run
        ! (a[base1 + len1-1]) must be greater than all elements of the second run.
        ! For performance, this method should be called only when len1 <= len2;
        ! its twin, mergeHi should be called if len1 >= len2.  (Either method
        ! may be called if len1 == len2.)
        ! ************************* IMPORTANT NOTE **************************
        !   The galloping mode is removed.
        ! *******************************************************************

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex, INTENT(IN)      :: Base1    ! index of first element in first run to be merged
        tIndex, INTENT(IN)      :: Length1  ! length of first run to be merged (must be > 0)
        tIndex, INTENT(IN)      :: Base2    ! index of first element in second run to be merged
                                            !   (must be aBase + aLen)
        tIndex, INTENT(IN)      :: Length2  ! length of second run to be merged (must be > 0)
        tIndex, INTENT(INOUT)   :: Indx(0:) ! array indices
        tIndex, INTENT(INOUT)   :: Tmp(0:)  ! work space

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: Len1, Len2
        tIndex          :: Index, Dest
        tIndex          :: Cursor1, Cursor2

    !** FLOW:

        ! initialize
        Len1 = Length1
        Len2 = Length2
        Cursor1 = 0         ! Index into ATmp array
        Cursor2 = Base2     ! Index into APtr array
        Dest    = Base1     ! Index into APtr array

        ! copy first run into temp array
        COPY_ARRAY_ZERO_BASED(Indx, Base1, Tmp, Cursor1, Len1)

        ! Move first element of second run and deal with degenerate cases
        Indx(Dest) = Indx(Cursor2)
        Dest    = Dest + 1
        Cursor2 = Cursor2 + 1

        Len2 = Len2 - 1
        IF ((Len2) == 0) THEN
            COPY_ARRAY_ZERO_BASED(Tmp, Cursor1, Indx, Dest, Len1)
            RETURN
        END IF

        IF (Len1 == 1) THEN
            COPY_ARRAY_ZERO_BASED(Indx, Cursor2, Indx, Dest, Len2)
            ! Last element of run 1 to end of merge
            Indx(Dest+Len2) = Tmp(Cursor1)
            RETURN
        END IF

        ! Do the straightforward thing
        DO
            IF (COMPARE_GLT(AVal(Indx(Cursor2)), AVal(Tmp(Cursor1)))) THEN
                Indx(Dest) = Indx(Cursor2)
                Dest    = Dest + 1
                Cursor2 = Cursor2 + 1
                Len2    = Len2 - 1
                IF (Len2 == 0) EXIT
            ELSE
                Indx(Dest) = Tmp(Cursor1)
                Dest    = Dest + 1
                Cursor1 = Cursor1 + 1
                Len1    = Len1 - 1
                IF (Len1 == 1) EXIT
            END IF
        END DO

        IF (Len1 == 1) THEN
            COPY_ARRAY_ZERO_BASED(Indx, Cursor2, Indx, Dest, Len2)
            ! Move last element of run 1 to end of merge
            Indx(Dest+Len2) = Tmp(Cursor1)
        ELSEIF (Len1 == 0) THEN
            CALL Handle_ErrLevel('MergeLow', ModName, ErrSevere, 'Preconditions were not met.')
        ELSE
            COPY_ARRAY_ZERO_BASED(Tmp, Cursor1, Indx, Dest, Len1)
        END IF

        RETURN

    END SUBROUTINE MergeLow

    !******************************************************************

    SUBROUTINE MergeLow_With_Gallop(Base1, Length1, Base2, Length2, Indx, Tmp)

    !** PURPOSE OF THIS SUBROUTINE
        ! To merge two adjacent runs in place, in a stable fashion.  The first
        ! element of the first run must be greater than the first element of the
        ! second run (a[base1] > a[base2]), and the last element of the first run
        ! (a[base1 + len1-1]) must be greater than all elements of the second run.
        ! For performance, this method should be called only when len1 <= len2;
        ! its twin, mergeHi should be called if len1 >= len2.  (Either method
        ! may be called if len1 == len2.)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex, INTENT(IN)      :: Base1    ! index of first element in first run to be merged
        tIndex, INTENT(IN)      :: Length1  ! length of first run to be merged (must be > 0)
        tIndex, INTENT(IN)      :: Base2    ! index of first element in second run to be merged
                                            !   (must be aBase + aLen)
        tIndex, INTENT(IN)      :: Length2  ! length of second run to be merged (must be > 0)
        tIndex, INTENT(INOUT)   :: Indx(0:) ! array indices
        tIndex, INTENT(INOUT)   :: Tmp(0:)  ! work space

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: MinGallop
        tIndex          :: Len1, Len2
        tIndex          :: Index, Dest
        tIndex          :: Cursor1, Cursor2
        tIndex          :: Count1, Count2
        tByte           :: IFlag1, IFlag2
        tLogical        :: LFlag

    !** FLOW:

        ! initialize
        Len1 = Length1
        Len2 = Length2
        Cursor1 = 0         ! Index into ATmp array
        Cursor2 = Base2     ! Index into APtr array
        Dest    = Base1     ! Index into APtr array

        ! copy first run into temp array
        COPY_ARRAY_ZERO_BASED(Indx, Base1, Tmp, Cursor1, Len1)

        ! Move first element of second run and deal with degenerate cases
        Indx(Dest) = Indx(Cursor2)
        Dest    = Dest + 1
        Cursor2 = Cursor2 + 1

        Len2 = Len2 - 1
        IF ((Len2) == 0) THEN
            COPY_ARRAY_ZERO_BASED(Tmp, Cursor1, Indx, Dest, Len1)
            RETURN
        END IF

        IF (Len1 == 1) THEN
            COPY_ARRAY_ZERO_BASED(Indx, Cursor2, Indx, Dest, Len2)
            ! Last element of run 1 to end of merge
            Indx(Dest+Len2) = Tmp(Cursor1)
            RETURN
        END IF

        ! use local variable
        MinGallop = Minimum_Gallop
        OUTER: DO
            ! initialize
            Count1 = 0      ! Number of times in a row that first run won
            Count2 = 0      ! Number of times in a row that second run won
            ! Do the straightforward thing until (if ever) one run starts
            ! winning consistently.
            DO
                IF (COMPARE_GLT(AVal(Indx(Cursor2)), AVal(Tmp(Cursor1)))) THEN
                    Indx(Dest) = Indx(Cursor2)
                    Dest    = Dest + 1
                    Cursor2 = Cursor2 + 1
                    Count2  = Count2 + 1
                    Count1  = 0
                    Len2    = Len2 - 1
                    IF (Len2 == 0) EXIT OUTER
                ELSE
                    Indx(Dest) = Tmp(Cursor1)
                    Dest    = Dest + 1
                    Cursor1 = Cursor1 + 1
                    Count1  = Count1 + 1
                    Count2  = 0
                    Len1    = Len1 - 1
                    IF (Len1 == 1) EXIT OUTER
                END IF
                IF (.NOT.(IOR(Count1,Count2) < MinGallop)) EXIT
            END DO

            ! One run is winning so consistently that galloping may be a
            ! huge win. So try that, and continue galloping until (if ever)
            ! neither run appears to be winning consistently anymore.
            DO
                Count1 = GallopRight(Indx(Cursor2), Tmp, Cursor1, Len1, 0)
                IF (Count1 /= 0) THEN
                    COPY_ARRAY_ZERO_BASED(Tmp, Cursor1, Indx, Dest, Count1)
                    Dest    = Dest + Count1
                    Cursor1 = Cursor1 + Count1
                    Len1    = Len1 - Count1
                    ! len1 == 1 || len1 == 0
                    IF (Len1 <= 1) EXIT OUTER
                END IF
                Indx(Dest) = Indx(Cursor2)
                Dest    = Dest + 1
                Cursor2 = Cursor2 + 1
                Len2    = Len2 - 1
                IF (Len2 == 0) EXIT OUTER

                Count2 = GallopLeft(Tmp(Cursor1), Indx, Cursor2, Len2, 0)
                IF (Count2 /= 0) THEN
                    COPY_ARRAY_ZERO_BASED(Indx, Cursor2, Indx, Dest, Count2)
                    Dest    = Dest + Count2
                    Cursor2 = Cursor2 + Count2
                    Len2    = Len2 - Count2
                    IF (Len2 == 0) EXIT OUTER
                END IF

                Indx(Dest) = Tmp(Cursor1)
                Dest    = Dest + 1
                Cursor1 = Cursor1 + 1
                Len1    = Len1 - 1
                IF (Len1 == 1) EXIT OUTER

                MinGallop = MinGallop - 1
                IF (Count1 >= Default_MinGallop) THEN
                    IFlag1 = 1_kByte
                ELSE
                    IFlag1 = 0_kByte
                END IF
                IF (Count2 >= Default_MinGallop) THEN
                    IFlag2 = 1_kByte
                ELSE
                    IFlag2 = 0_kByte
                END IF
                IF (IOR(IFlag1, IFlag2) /= 0_kByte) THEN
                    LFlag = .TRUE.
                ELSE
                    LFlag = .FALSE.
                END IF
                IF (.NOT.(LFlag)) EXIT
            END DO

            IF (MinGallop < 0) MinGallop = 0
            MinGallop = MinGallop + 2   ! Penalize for leaving gallop mode

        END DO OUTER    ! End of "outer" loop

        ! set main-routine variable
        IF (MinGallop < 1) THEN
            Minimum_Gallop = 1
        ELSE
            Minimum_Gallop = MinGallop
        END IF

        IF (Len1 == 1) THEN
            COPY_ARRAY_ZERO_BASED(Indx, Cursor2, Indx, Dest, Len2)
            ! Move last element of run 1 to end of merge
            Indx(Dest+Len2) = Tmp(Cursor1)
        ELSEIF (Len1 == 0) THEN
            CALL Handle_ErrLevel('MergeLow_With_Gallop', ModName, ErrSevere, 'Preconditions were not met.')
        ELSE
            COPY_ARRAY_ZERO_BASED(Tmp, Cursor1, Indx, Dest, Len1)
        END IF

        RETURN

    END SUBROUTINE MergeLow_With_Gallop

    !******************************************************************

    SUBROUTINE MergeHigh(Base1, Length1, Base2, Length2, Indx, Tmp)

    !** PURPOSE OF THIS SUBROUTINE
        ! Like mergeLo, except that this method should be called only if
        ! len1 >= len2; mergeLo should be called if len1 <= len2. (Either method
        ! may be called if len1 == len2.)
        ! ************************* IMPORTANT NOTE **************************
        !   The galloping mode is removed.
        ! *******************************************************************

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex, INTENT(IN)      :: Base1    ! index of first element in first run to be merged
        tIndex, INTENT(IN)      :: Length1  ! length of first run to be merged (must be > 0)
        tIndex, INTENT(IN)      :: Base2    ! index of first element in second run to be merged
                                            !   (must be aBase + aLen)
        tIndex, INTENT(IN)      :: Length2  ! length of second run to be merged (must be > 0)
        tIndex, INTENT(INOUT)   :: Indx(0:) ! array indices
        tIndex, INTENT(INOUT)   :: Tmp(0:)  ! work space

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: Len1, Len2
        tIndex          :: Index, Dest
        tIndex          :: Cursor1, Cursor2

    !** FLOW:

        ! initialize
        Len1 = Length1
        Len2 = Length2
        Cursor1 = Base1 + Len1 - 1  ! Index into ATmp array
        Cursor2 = Len2 - 1          ! Index into APtr array
        Dest    = Base2 + Len2 - 1  ! Index into APtr array

        ! copy second run into temp array
        COPY_ARRAY_ZERO_BASED(Indx, Base2, Tmp, 0, Len2)

        ! Move last element of first run and deal with degenerate cases
        Indx(Dest) = Indx(Cursor1)
        Dest    = Dest - 1
        Cursor1 = Cursor1 - 1

        Len1 = Len1 - 1
        IF (Len1 == 0) THEN
            COPY_ARRAY_ZERO_BASED(Tmp, 0, Indx, Dest-(Len2-1), Len2)
            RETURN
        END IF

        IF (Len2 == 1) THEN
            Dest    = Dest - Len1
            Cursor1 = Cursor1 - Len1
            COPY_ARRAY_ZERO_BASED(Indx, Cursor1+1, Indx, Dest+1, Len1)
            ! First element of run 2 to end of merge
            Indx(Dest) = Tmp(Cursor2)
            RETURN
        END IF

        ! Do the straightforward thing
        DO
            IF (COMPARE_GLT(AVal(Tmp(Cursor2)), AVal(Indx(Cursor1)))) THEN
                Indx(Dest) = Indx(Cursor1)
                Dest    = Dest - 1
                Cursor1 = Cursor1 - 1
                Len1 = Len1 - 1
                IF (Len1 == 0) EXIT
            ELSE
                Indx(Dest) = Tmp(Cursor2)
                Dest    = Dest - 1
                Cursor2 = Cursor2 - 1
                Len2    = Len2 - 1
                IF (Len2 == 1) EXIT
            END IF
        END DO

        IF (Len2 == 1) THEN
            Dest    = Dest - Len1
            Cursor1 = Cursor1 - Len1
            COPY_ARRAY_ZERO_BASED(Indx, Cursor1+1, Indx, Dest+1, Len1)
            ! Move first element of run2 to front of merge
            Indx(Dest) = Tmp(Cursor2)
        ELSEIF (Len2 == 0) THEN
            CALL Handle_ErrLevel('MergeHigh', ModName, ErrSevere, 'Preconditions were not met.')
        ELSE
            COPY_ARRAY_ZERO_BASED(Tmp, 0, Indx, Dest-(Len2-1), Len2)
        END IF

        RETURN

    END SUBROUTINE MergeHigh

    !******************************************************************

    SUBROUTINE MergeHigh_With_Gallop(Base1, Length1, Base2, Length2, Indx, Tmp)

    !** PURPOSE OF THIS SUBROUTINE
        ! Like mergeLo, except that this method should be called only if
        ! len1 >= len2; mergeLo should be called if len1 <= len2.  (Either
        ! method may be called if len1 == len2.)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex, INTENT(IN)      :: Base1    ! index of first element in first run to be merged
        tIndex, INTENT(IN)      :: Length1  ! length of first run to be merged (must be > 0)
        tIndex, INTENT(IN)      :: Base2    ! index of first element in second run to be merged
                                            !   (must be aBase + aLen)
        tIndex, INTENT(IN)      :: Length2  ! length of second run to be merged (must be > 0)
        tIndex, INTENT(INOUT)   :: Indx(0:) ! array indices
        tIndex, INTENT(INOUT)   :: Tmp(0:)  ! work space

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: MinGallop
        tIndex          :: Len1, Len2
        tIndex          :: Index, Dest
        tIndex          :: Cursor1, Cursor2
        tIndex          :: Count1, Count2
        tByte           :: IFlag1, IFlag2
        tLogical        :: LFlag

    !** FLOW:

        ! initialize
        Len1 = Length1
        Len2 = Length2
        Cursor1 = Base1 + Len1 - 1  ! Index into ATmp array
        Cursor2 = Len2 - 1          ! Index into APtr array
        Dest    = Base2 + Len2 - 1  ! Index into APtr array

        ! copy second run into temp array
        COPY_ARRAY_ZERO_BASED(Indx, Base2, Tmp, 0, Len2)

        ! Move last element of first run and deal with degenerate cases
        Indx(Dest) = Indx(Cursor1)
        Dest    = Dest - 1
        Cursor1 = Cursor1 - 1

        Len1 = Len1 - 1
        IF (Len1 == 0) THEN
            COPY_ARRAY_ZERO_BASED(Tmp, 0, Indx, Dest-(Len2-1), Len2)
            RETURN
        END IF

        IF (Len2 == 1) THEN
            Dest    = Dest - Len1
            Cursor1 = Cursor1 - Len1
            COPY_ARRAY_ZERO_BASED(Indx, Cursor1+1, Indx, Dest+1, Len1)
            ! First element of run 2 to end of merge
            Indx(Dest) = Tmp(Cursor2)
            RETURN
        END IF

        ! use local variable
        MinGallop = Minimum_Gallop
        OUTER: DO
            ! initialize
            Count1 = 0      ! Number of times in a row that first run won
            Count2 = 0      ! Number of times in a row that second run won
            ! Do the straightforward thing until (if ever) one run
            ! appears to win consistently.
            DO
                IF (COMPARE_GLT(AVal(Tmp(Cursor2)), AVal(Indx(Cursor1)))) THEN
                    Indx(Dest) = Indx(Cursor1)
                    Dest    = Dest - 1
                    Cursor1 = Cursor1 - 1
                    Count1  = Count1 + 1
                    Count2  = 0
                    Len1 = Len1 - 1
                    IF (Len1 == 0) EXIT OUTER
                ELSE
                    Indx(Dest) = Tmp(Cursor2)
                    Dest    = Dest - 1
                    Cursor2 = Cursor2 - 1
                    Count2  = Count2 + 1
                    Count1  = 0
                    Len2    = Len2 - 1
                    IF (Len2 == 1) EXIT OUTER
                END IF
                IF (.NOT.(IOR(Count1,Count2) < MinGallop)) EXIT
            END DO

            ! One run is winning so consistently that galloping may be a
            ! huge win. So try that, and continue galloping until (if ever)
            ! neither run appears to be winning consistently anymore.
            DO
                Count1 = Len1 - GallopRight(Tmp(Cursor2), Indx, Base1, Len1, Len1-1)
                IF (Count1 /= 0) THEN
                    Dest    = Dest - Count1
                    Cursor1 = Cursor1 - Count1
                    Len1    = Len1 - Count1
                    COPY_ARRAY_ZERO_BASED(Indx, Cursor1+1, Indx, Dest+1, Count1)
                    IF (Len1 == 0) EXIT OUTER
                END IF

                Indx(Dest) = Tmp(Cursor2)
                Dest    = Dest - 1
                Cursor2 = Cursor2 - 1
                Len2    = Len2 - 1
                IF (Len2 == 1) EXIT OUTER

                Count2 = Len2 - GallopLeft(Indx(Cursor1), Tmp, 0, Len2, Len2 - 1)
                IF (Count2 /= 0) THEN
                    Dest    = Dest - Count2
                    Cursor2 = Cursor2 - Count2
                    Len2    = Len2 - Count2
                    COPY_ARRAY_ZERO_BASED(Tmp, Cursor2+1, Indx, Dest+1, Count2)
                    ! len2 == 1 || len2 == 0
                    IF (Len2 <= 1) EXIT OUTER
                END IF

                Indx(Dest) = Indx(Cursor1)
                Dest    = Dest - 1
                Cursor1 = Cursor1 - 1
                Len1    = Len1 - 1
                IF (Len1 == 0) EXIT OUTER

                MinGallop = MinGallop - 1
                IF (Count1 >= Default_MinGallop) THEN
                    IFlag1 = 1_kByte
                ELSE
                    IFlag1 = 0_kByte
                END IF
                IF (Count2 >= Default_MinGallop) THEN
                    IFlag2 = 1_kByte
                ELSE
                    IFlag2 = 0_kByte
                END IF
                IF (IOR(IFlag1, IFlag2) /= 0_kByte) THEN
                    LFlag = .TRUE.
                ELSE
                    LFlag = .FALSE.
                END IF
                IF (.NOT.(LFlag)) EXIT
            END DO

            IF (MinGallop < 0) MinGallop = 0
            MinGallop = MinGallop + 2   ! Penalize for leaving gallop mode

        END DO OUTER    ! End of "outer" loop

        ! set main-routine variable
        IF (MinGallop < 1) THEN
            Minimum_Gallop = 1
        ELSE
            Minimum_Gallop = MinGallop
        END IF

        IF (Len2 == 1) THEN
            Dest    = Dest - Len1
            Cursor1 = Cursor1 - Len1
            COPY_ARRAY_ZERO_BASED(Indx, Cursor1+1, Indx, Dest+1, Len1)
            ! Move first element of run2 to front of merge
            Indx(Dest) = Tmp(Cursor2)
        ELSEIF (Len2 == 0) THEN
            CALL Handle_ErrLevel('MergeHigh_With_Gallop', ModName, ErrSevere, 'Preconditions were not met.')
        ELSE
            COPY_ARRAY_ZERO_BASED(Tmp, 0, Indx, Dest-(Len2-1), Len2)
        END IF

        RETURN

    END SUBROUTINE MergeHigh_With_Gallop

    !******************************************************************

    SUBROUTINE EnsureCapacity(MinCapacity)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To initialize working variables.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex, INTENT(IN)  :: MinCapacity

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: NewSize, NZero
        tIndex          :: StackLen

    !** FLOW:

        IF (SIZE(ATmp) < MinCapacity) THEN

            ! Compute smallest power of 2 > minCapacity
            NZero = 31 - FLOOR(LOG(REAL(MinCapacity,KIND=kFP))/LOG(Two))
            NewSize = SHIFTR(-1,NZero)
            NewSize = NewSize + 1

            ! set NewSize
            IF (NewSize < 0) THEN   ! Not bloody likely!
                NewSize = MinCapacity
            ELSE
                NewSize = MIN(NewSize, SHIFTR(NA,1))
            END IF

            ! allocate working variables
            DEALLOCATE(ATmp)
            ALLOCATE(ATmp(0:NewSize-1))

        END IF

        RETURN

    END SUBROUTINE EnsureCapacity

    !******************************************************************

END SUBROUTINE Tim_Rank

!**********************************************************************

MODULE SUBROUTINE Rust_Rank(AVal, AInd)

!** PURPOSE OF THIS SUBROUTINE
    ! To determine indices such that they sort the specified array into the desired
    ! order using the Rust's mergesort algorithm [1],  which is a simplified version
    ! of the Timsort algorithm.

!** REFEENCE
!   [1] <a href="https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159">
!       Rust's mergesort. </a> <br>
!   [2] <a href="https://github.com/fortran-lang/stdlib/blob/master/src/stdlib_sorting_ord_sort.fypp">
!       Fortran Standard Library's ordered sorting. </a> <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)          ! array values
    tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal)) ! array indices

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER     :: Insertion_CutOff = 32    ! initial cutoff to pair insertion sort
    ! The maximum number of entries in a run stack, good for an array of 2**64 elements.
    tInteger, PARAMETER     :: Max_Merge_Stack = INT(CEILING(LOG(2.0_kFP**64) / &
                                                 LOG(1.6180339887_kFP)))

!** DERIVED TYPE DEFINITIONS
    ! na

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(AVal, KIND=kIndex)

    ! initialize the indices
    CALL Index_Init(NA, AInd)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to use pair insertion instead
    IF (NA <= Insertion_CutOff) THEN
        ! Note: this is a zero-based routine.
        CALL Insert_Guarded(AVal, AInd, 1, NA)
        RETURN
    END IF

    BLOCK
        tIndex  :: BInd(NA/2)
        ! perform merge sort
        CALL Rust_MergeIndex(AVal, AInd, NA, BInd)
    END BLOCK

    RETURN

CONTAINS

    SUBROUTINE Rust_MergeIndex(AVal, AInd, ALen, BInd)

    !** PURPOSE OF THIS SUBROUTINE
        ! The Rust merge sort borrows some (but not all) of the ideas from TimSort,
        ! which is described in detail at
        ! (http://svn.python.org/projects/python/trunk/Objects/listsort.txt).
        ! The algorithm identifies strictly descending and non-descending
        ! subsequences, which are called natural runs.  Where these runs are less
        ! than a minimum run size they are padded by adding additional samples
        ! using a pair-insertion sort.  The merge process is driven by a stack of
        ! pending unmerged runs.  Each newly found run is pushed onto the stack,
        ! and then pairs of adjacent runs are merged until these two invariants
        ! are satisfied:
        ! 1. for every 'i' in '1..size(runs)-1': 'runlen(i - 1) > runlen(i)'
        ! 2. for every 'i' in '2..size(runs)-1': 'runlen(i - 2) > runlen(i - 1) + runlen(i)'
        ! The invariants ensure that the total running time is 'O(n log n)' worst-case.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(IN)     :: AVal(:)
        tIndex,    INTENT(INOUT)  :: AInd(0:)
        tIndex,    INTENT(IN)     :: ALen
        tIndex,    INTENT(INOUT)  :: BInd(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: Finish, Min_Run, nRun, R, RCount, Start
        tIndex          :: RunLen(0:Max_Merge_Stack-1)
        tIndex          :: RunBase(0:Max_Merge_Stack-1)
        tIndex          :: LeftLen, LeftBase
        tIndex          :: RightLen, RightBase

    !** FLOW:

        ! Very short runs are extended using insertion sort to span at least Min_Run elements.
        ! Slices of up to this length are sorted using pair-insertion sort.
        Min_Run = Calc_Min_Run(ALen)

        ! Following Rust sort, natural runs in 'A' are identified by traversing
        ! it backwards.  By traversing it backward, merges more often go in the
        ! opposite direction (forwards).  According to developers of Rust sort,
        ! merging forwards is slightly faster than merging backwards.  Therefore
        ! identifying runs by traversing backwards should improve performance.
        RCount = 0
        Finish = ALen - 1
        DO WHILE (Finish >= 0)

            ! Find the next natural run, and reverse it if it's strictly descending.
            Start = Finish
            IF (Start > 0) THEN
                Start = Start - 1
                IF (COMPARE_GLT(AVal(AInd(Start+1)), AVal(AInd(Start)))) THEN
                    Descending: DO WHILE (Start > 0)
                        IF (COMPARE_GLE(AVal(AInd(Start-1)), AVal(AInd(Start)))) EXIT Descending
                        Start = Start - 1
                    END DO Descending
                    CALL Reverse_Order_Base0(AInd, Start, Finish)
                ELSE
                    Ascending: DO WHILE(Start > 0)
                        IF (COMPARE_GLT(AVal(AInd(Start)), AVal(AInd(Start-1)))) EXIT Ascending
                        Start = Start - 1
                    END DO Ascending
                END IF
            END IF

            ! compute run length
            nRun = Finish - Start

            IF (nRun < Min_Run) THEN
                ! If run is short, extend to MinRun
                IF (nRun < Min_Run/8) THEN
                    ! If nRun is too short, use pair insertion sort
                    Start = Finish - Min_Run + 1
                    IF (Start < 0) Start = 0
                    CALL Insert_Guarded(AVal, AInd, Start+1, Finish+1)
                ELSE
                    ! nRun is long enough, use insertion sort
                    Insert_Loop: DO WHILE (Start > 0)
                        IF ((Finish-Start) >= (Min_Run-1)) EXIT Insert_Loop
                        Start = Start - 1
                        CALL Insert_Head(AVal, AInd(Start:Finish))
                    END DO Insert_Loop
                    IF ((Start == 0).AND.(Finish == ALen-1)) RETURN
                END IF
            END IF

            ! initialize the stack
            RunBase(RCount) = Start
            RunLen(RCount)  = Finish - Start + 1

            Finish = Start-1
            RCount = RCount + 1

            ! Determine whether pairs of adjacent runs need to be merged to satisfy
            ! the invariants, and, if so, merge them.
            Merge_Loop: DO

                ! examine the stack of runs waiting to be merged
                R = Collapse(RunBase(0:RCount-1), RunLen(0:RCount-1))

                IF ((R < 0).OR.(RCount <= 1)) EXIT Merge_Loop
                LeftLen   = RunLen(R+1)
                LeftBase  = RunBase(R+1)
                RightLen  = RunLen(R)
                RightBase = RunBase(R)

                ! merge adjacent runs
                CALL Merging(AVal, AInd(LeftBase:RightBase+RightLen-1), LeftLen, BInd)

                ! set the stack
                RunBase(R) = LeftBase
                RunLen(R)  = LeftLen + RightLen
                IF (R == RCount-3) THEN
                    RunBase(R+1) = RunBase(R+2)
                    RunLen(R+1)  = RunLen(R+2)
                END IF
                RCount = RCount - 1
            END DO Merge_Loop
        END DO

        RETURN

    END SUBROUTINE Rust_MergeIndex

    !******************************************************************

    FUNCTION Calc_Min_Run(N) RESULT(Min_Run)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To determine the minimum length of a run from 32-63 so that N/MIN_RUN is
        ! less than or equal to a power of two. See
        ! https://svn.python.org/projects/python/trunk/Objects/listsort.txt.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex, INTENT(IN)  :: N
        tIndex              :: Min_Run

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: Num
        tIndex          :: R

    !** FLOW:

        Num = N
        R = 0

        DO WHILE(Num >= 64)
            R = IOR(R, IAND(Num, 1))
            Num = SHIFTA(Num, 1)
        END DO
        Min_Run = Num + R

        RETURN

    END FUNCTION Calc_Min_Run

    !******************************************************************

    FUNCTION Collapse(Base, Length) RESULT (R)

    !** PURPOSE OF THIS SUBROUTINE
        ! To examine the stack of runs waiting to be merged, identifying adjacent runs
        ! to be merged until the stack invariants are re-established:
        ! 1. len(-3) > len(-2) + len(-1)
        ! 2. len(-2) > len(-1)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex, INTENT(IN)  :: Base(0:)
        tIndex, INTENT(IN)  :: Length(0:)
        tIndex              :: R

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: N
        tLogical        :: Test

    !** FLOW:

        N = MIN(SIZE(Base),SIZE(Length))
        Test = FalseVal
        IF (N >= 2) THEN
            IF ((Base(N-1) == 0).OR.(Length(N-2) <= Length(N-1))) THEN
                Test = TrueVal
            ELSEIF (N >= 3) THEN
                ! X exists
                IF (Length(N-3) <= (Length(N-2)+Length(N-1))) THEN
                    Test = TrueVal
                    ! |X| <= |Y| + |Z| => will need to merge due to rho1 or rho2
                ELSEIF(N >= 4) THEN
                    IF (Length(N-4) <= (Length(N-3)+Length(N-2))) THEN
                        Test = TrueVal
                        ! |W| <= |X| + |Y| => will need to merge due to rho1 or rho3
                    END IF
                END IF
            END IF
        END IF
        IF (Test) THEN
            ! By default merge Y & Z, rho2 or rho3
            IF (N >= 3) THEN
                IF (Length(N-3) < Length(N-1)) THEN
                    R = N - 3
                    ! |X| < |Z| => merge X & Y, rho1
                    RETURN
                END IF
            END IF
            R = N - 2
            ! |Y| <= |Z| => merge Y & Z, rho4
            RETURN
        ELSE
            R = -1
        END IF

        RETURN

    END FUNCTION Collapse

    !******************************************************************

    SUBROUTINE Insert_Head(AVal, AInd)

    !** PURPOSE OF THIS SUBROUTINE
        ! To inserts 'A(0)' into the pre-sorted sequence 'A(1:)' so that the
        ! whole 'A(0:)' becomes sorted, copying the first element into
        ! a temporary variable, iterating until the right place for it is found.
        ! copying every traversed element into the slot preceding it, and finally,
        ! copying data from the temporary variable into the resulting hole.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(IN)       :: AVal(:)
        tIndex,    INTENT(INOUT)    :: AInd(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: I
        tIndex          :: Temp

    !** FLOW:

        Temp = AInd(0)
        Find_Hole: DO I = 1, SIZE(AInd)-1
            IF (COMPARE_GLE(AVal(Temp), AVal(AInd(I)))) EXIT Find_Hole
            AInd(I-1) = AInd(I)
        END DO Find_Hole
        AInd(I-1) = Temp

        RETURN

    END SUBROUTINE Insert_Head

    !******************************************************************

    SUBROUTINE Merging(AVal, AInd, Mid, BInd)

    !** PURPOSE OF THIS SUBROUTINE
        ! To merge the two non-decreasing runs 'A(0:Mid-1)' and 'A(Mid:)'
        ! using 'B' as a temporary storage, and stores the merged runs into
        ! 'A(0:)'.  'Mid' must be > 0, and < 'SIZE(A)-1'. The buffer 'B'
        ! must be long enough to hold the shorter of the two runs.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(IN)     :: AVal(:)
        tIndex,    INTENT(INOUT)  :: AInd(0:)
        tIndex,    INTENT(IN)     :: Mid
        tIndex,    INTENT(INOUT)  :: BInd(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: ALen, I, J, K
        tIndex          :: Temp

    !** FLOW:

        ALen = SIZE(AInd)

        ! Merge first copies the shorter run into 'B'. Then, depending on which
        ! run was shorter, it traces the copied run and the longer run forwards
        ! (or backwards), comparing their next unprocessed elements and then
        ! copying the lesser (or greater) one into 'A'.

        IF (Mid <= (ALen - Mid)) THEN
            ! The left run is shorter.
            BInd(0:Mid-1) = AInd(0:Mid-1)
            I = 0
            J = Mid
            Merge_Lower: DO K = 0, ALen-1
                IF (COMPARE_GLE(AVal(BInd(I)), AVal(AInd(J)))) THEN
                    AInd(K) = BInd(I)
                    I = I + 1
                    IF (I >= Mid) EXIT Merge_Lower
                ELSE
                    AInd(K) = AInd(J)
                    J = J + 1
                    IF (J >= ALen) THEN
                        AInd(K+1:) = BInd(I:Mid-1)
                        EXIT Merge_Lower
                    END IF
                END IF
            END DO Merge_Lower
        ELSE
            ! The right run is shorter so check that it is stable
            BInd(0:ALen-Mid-1) = AInd(Mid:ALen-1)
            I = Mid - 1
            J = ALen - Mid -1
            Merge_Upper: DO K = ALen-1, 0, -1
                IF (COMPARE_GLE(AVal(AInd(I)), AVal(BInd(J)))) THEN
                    AInd(K) = BInd(J)
                    J = J - 1
                    IF (J < 0) EXIT Merge_Upper
                ELSE
                    AInd(K) = AInd(I)
                    I = I - 1
                    IF (I < 0) THEN
                        AInd(0:K-1) = BInd(0:J)
                        EXIT Merge_Upper
                    END IF
                END IF
            END DO Merge_Upper
        END IF

        RETURN

    END SUBROUTINE Merging

    !******************************************************************

END SUBROUTINE Rust_Rank

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           AUXILIARY ROUTINES
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE HeapIndex_Java(AVal, AInd, Left, Right)

!** PURPOSE OF THIS SUBROUTINE
    ! To determine indices such that they sort the specified array into the desired
    ! order using the heapsort algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)     :: AVal(:)  ! array values
    tIndex,    INTENT(INOUT)  :: AInd(0:) ! array indices
    tIndex,    INTENT(IN)     :: Left     ! the index of the first element, inclusive, to be sorted
    tIndex,    INTENT(IN)     :: Right    ! the index of the last element, exclusive, to be sorted

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: K
    tIndex          :: Low
    tIndex          :: High
    tIndex          :: Max

!** FLOW:

    ! initialize
    Low = Left
    High = Right

    ! perform heap sort
    K = SHIFTR(Low+High, 1)
    DO WHILE (K > Low)
        K = K - 1
        CALL PushDown(AVal, AInd, K, AInd(K), Low, High)
    END DO

    High = High - 1
    DO WHILE (High > Low)
        Max = AInd(Low)
        CALL PushDown(AVal, AInd, Low, AInd(High), Low, High)
        AInd(High) = Max
        High = High - 1
    END DO

    RETURN

    CONTAINS

    SUBROUTINE PushDown(AVal, AInd, Q, VInd, Low, High)

    !** PURPOSE OF THIS SUBROUTINE
        !*
        ! Pushes specified element down during heap sort.
        !
        ! @param a the given array
        ! @param p the start index
        ! @param value the given element
        ! @param low the index of the first element, inclusive, to be sorted
        ! @param high the index of the last element, exclusive, to be sorted
        !/

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(IN)     :: AVal(:)  ! array values
        tIndex,      INTENT(INOUT)  :: AInd(0:) ! array indices
        tIndex,      INTENT(IN)     :: Q        ! p
        tIndex,      INTENT(IN)     :: VInd     ! v index
        tIndex,      INTENT(IN)     :: Low      ! low
        tIndex,      INTENT(IN)     :: High     ! high

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: K, P

    !** FLOW:

        ! initialize
        P = Q

        DO
            ! Index of the right child
            K = SHIFTL(P, 1) - Low + 2

            IF (K > High) EXIT

            IF ((K == High).OR.(COMPARE_GLT(AVal(AInd(K)), AVal(AInd(K-1))))) K = K -1

            IF (COMPARE_GLE(AVal(AInd(K)), AVal(VInd))) EXIT

            ! update
            AInd(P) = AInd(K)
            P = K
        END DO

        AInd(P) = VInd

        RETURN

    END SUBROUTINE PushDown

    !******************************************************************

END SUBROUTINE HeapIndex_Java

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                       QUICK SORT ROUTINES
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   RECURSIVE QUICKSORT DRIVER ROUTINES
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE SUBROUTINE Quick_Rank_Hoare(AVal, AInd)

!** PURPOSE OF THIS SUBROUTINE
    ! To determine indices such that they sort the specified array into the desired
    ! order using the quicksort algorithm with Hoare's partitioning scheme.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)          ! array values
    tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal)) ! array indices

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: CutOff       ! cutoff to pair insertion sort
    tIndex          :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(AVal, KIND=kIndex)

    ! initialize the indices
    CALL Index_Init(NA, AInd)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Quick_Insert_CutOff_Low) THEN
        CALL DualInsert_Guarded(AVal, AInd, 1, NA)
        RETURN
    END IF

    ! set cutoff depending on the size of the input array
    IF (NA < 2000) THEN
        CutOff = Quick_Insert_CutOff_Low
    ELSE
        CutOff = Quick_Insert_CutOff_High
    END IF

    ! perform quick sort using tail recursion
    CALL QuickIndex_TailRecur(AVal, AInd, 1, NA, CutOff, TrueVal, Partition_Hoare)

    RETURN

    CONTAINS

    SUBROUTINE Partition_Hoare(AVal,AInd,LStart,REnd,LEnd,RStart)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To partition an array using Hoare's partitioning scheme.
        ! The given array in the range [LStart,REnd] is partitioned
        ! into two sub-arrays, Left and Right, such that all elements
        ! in the Left sub-array in the range [LStart,LEnd] are less
        ! than or equal to the selected pivot, and all elements in
        ! the Right sub-array in the range [RStart,REnd] are greater
        ! than or equal to the selected pivot.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)  ! array values
        tIndex,      INTENT(INOUT)  :: AInd(:)  ! array indices
        tIndex,      INTENT(IN)     :: LStart   ! on entry, the start of the given array
                                                ! on exit,  the start of the Left sub-array
        tIndex,      INTENT(IN)     :: REnd     ! on entry, the end of the given array
                                                ! on exit,  the end of the Right sub-array
        tIndex,      INTENT(OUT)    :: LEnd     ! the end of the Left sub-array
        tIndex,      INTENT(OUT)    :: RStart   ! the start of the Right sub-array

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: PInd, Temp
        tIndex          :: Mid, LPtr, RPtr

    !** FLOW:

        ! Select the pivot
        Mid = LStart + (REnd-LStart) / 2
        PInd = AInd(FLOOR(REAL(Mid,KIND=kFP)))

        ! Perform partitioning.
        LPtr = LStart
        RPtr = REnd
        DO WHILE (LPtr <= RPtr)

            ! Increment the left pointer until we find an element
            ! that is greater than or equal to the pivot.
            DO WHILE (COMPARE_GLT(AVal(AInd(LPtr)), AVal(PInd)))
                LPtr = LPtr + 1
            END DO

            ! Decrement the right pointer until we find an element
            ! that is less than or equal to the pivot.
            DO WHILE (COMPARE_GLT(AVal(PInd), AVal(AInd(RPtr))))
                RPtr = RPtr - 1
            END DO

            ! A pair of values have been found where A(LPtr) is greater than the pivot
            ! and A(RPtr) is less than the pivot; thus, while the left pointer is less
            ! than or equal to the right pointer, swap A(LPtr) with A(RPtr).
            IF (LPtr <= RPtr) THEN
                EXCHANGE(AInd, LPtr, RPtr)
                LPtr = LPtr + 1
                RPtr = RPtr - 1
            END IF

        END DO

        ! Set output indices
        LEnd   = LPtr-1
        RStart = LPtr

        RETURN

    END SUBROUTINE Partition_Hoare

    !******************************************************************

END SUBROUTINE Quick_Rank_Hoare

!**********************************************************************

MODULE SUBROUTINE Quick_Rank_Lomuto(AVal, AInd)

!** PURPOSE OF THIS SUBROUTINE
    ! To determine indices such that they sort the specified array into the desired
    ! order using the quicksort algorithm with Lomuto's partitioning scheme.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)          ! array values
    tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal)) ! array indices

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: CutOff       ! cutoff to pair insertion sort
    tIndex          :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(AVal, KIND=kIndex)

    ! initialize the indices
    CALL Index_Init(NA, AInd)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Quick_Insert_CutOff_Low) THEN
        CALL DualInsert_Guarded(AVal, AInd, 1, NA)
        RETURN
    END IF

    ! set cutoff depending on the size of the input array
    IF (NA < 2000) THEN
        CutOff = Quick_Insert_CutOff_Low
    ELSE
        CutOff = Quick_Insert_CutOff_High
    END IF

    ! perform quick sort using tail recursion
    CALL QuickIndex_TailRecur(AVal, AInd, 1, NA, CutOff, TrueVal, Partition_Lomuto)

    RETURN

    CONTAINS

    SUBROUTINE Partition_Lomuto(AVal,AInd,LStart,REnd,LEnd,RStart)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To partition an array using Lomuto's partitioning scheme.
        ! The given array in the range [LStart,REnd] is partitioned
        ! into two sub-arrays, Left and Right, such that all elements
        ! in the Left sub-array in the range [LStart,LEnd] are less
        ! than or equal to the selected pivot, and all elements in
        ! the Right sub-array in the range [RStart,REnd] are greater
        ! than or equal to the selected pivot.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)  ! array values
        tIndex,      INTENT(INOUT)  :: AInd(:)  ! array indices
        tIndex,      INTENT(IN)     :: LStart   ! on entry, the start of the given array
                                                ! on exit,  the start of the Left sub-array
        tIndex,      INTENT(IN)     :: REnd     ! on entry, the end of the given array
                                                ! on exit,  the end of the Right sub-array
        tIndex,      INTENT(OUT)    :: LEnd     ! the end of the Left sub-array
        tIndex,      INTENT(OUT)    :: RStart   ! the start of the Right sub-array

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: J, Mid, Indx
        tIndex          :: PInd, Temp

    !** FLOW:

        ! Select the pivot
        Mid = LStart + (REnd-LStart) / 2
        IF (COMPARE_GLT(AVal(AInd(Mid)), AVal(AInd(LStart)))) THEN
            EXCHANGE(AInd, Mid, LStart)
        END IF
        IF (COMPARE_GLT(AVal(AInd(REnd)), AVal(AInd(LStart)))) THEN
            EXCHANGE(AInd, REnd, LStart)
        END IF
        IF (COMPARE_GLT(AVal(AInd(Mid)), AVal(AInd(REnd)))) THEN
            EXCHANGE(AInd, Mid, REnd)
        END IF
        PInd = AInd(REnd)

        ! Perform partitioning
        Indx = LStart
        DO J = LStart, REnd
            IF (COMPARE_GLT(AVal(AInd(J)), AVal(PInd))) THEN
                EXCHANGE(AInd, Indx, J)
                Indx = Indx + 1
            END IF
        END DO
        EXCHANGE(AInd, Indx, REnd)

        ! Set output indices
        LEnd   = Indx - 1
        RStart = Indx + 1

        RETURN

    END SUBROUTINE Partition_Lomuto

    !******************************************************************

END SUBROUTINE Quick_Rank_Lomuto

!**********************************************************************

MODULE SUBROUTINE Quick_Rank_3Way(AVal, AInd)

!** PURPOSE OF THIS SUBROUTINE
    ! To determine indices such that they sort the specified array into the desired
    ! order using the quicksort algorithm with 3-way partitioning scheme.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)          ! array values
    tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal)) ! array indices

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: Quick_Median_of_3_CutOff = 150   ! cutoff to median-of-3 partitioning

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: CutOff       ! cutoff to pair insertion sort
    tIndex          :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(AVal, KIND=kIndex)

    ! initialize the indices
    CALL Index_Init(NA, AInd)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Quick_Insert_CutOff_Low) THEN
        CALL DualInsert_Guarded(AVal, AInd, 1, NA)
        RETURN
    END IF

    ! set cutoff depending on the size of the input array
    IF (NA < 2000) THEN
        CutOff = Quick_Insert_CutOff_Low
    ELSE
        CutOff = Quick_Insert_CutOff_High
    END IF

    ! perform quick sort using tail recursion
    CALL QuickIndex_TailRecur(AVal, AInd, 1, NA, CutOff, TrueVal, Partition_3Way)

    RETURN

    CONTAINS

    SUBROUTINE Partition_3Way(AVal,AInd,LStart,REnd,LEnd,RStart)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To partition an array using Bentley-McIlroy's 3-way partitioning scheme.
        ! The given array in the range [LStart,REnd] is partitioned into three
        ! sub-arrays, Left, Right and Middle, such that all elements in the Left
        ! sub-array in the range [LStart,LEnd] are less than the selected pivot,
        ! and all elements in the Right sub-array in the range [RStart,REnd] are
        ! greater than the selected pivot, and all elements in the Middle sub-array
        ! (if exists) in the range [LEnd+1,RStart-1] are equal to the selected pivot.

    !** REFERENCES
        ! "Quick.java" source code from "https://algs4.cs.princeton.edu/23quicksort"
        ! of "Algorithms, 4th Edition" by Robert Sedgewick and Kevin Wayne.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)  ! array values
        tIndex,      INTENT(INOUT)  :: AInd(:)  ! array indices
        tIndex,      INTENT(IN)     :: LStart   ! on entry, the start of the given array
                                                ! on exit,  the start of the Left sub-array
        tIndex,      INTENT(IN)     :: REnd     ! on entry, the end of the given array
                                                ! on exit,  the end of the Right sub-array
        tIndex,      INTENT(OUT)    :: LEnd     ! the end of the Left sub-array
        tIndex,      INTENT(OUT)    :: RStart   ! the start of the Right sub-array

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: PInd, Temp
        tIndex          :: I, J, K
        tIndex          :: P, Q, N
        tIndex          :: Eps, Mid
        tIndex          :: M1, M2, M3
        tIndex          :: Ninther

    !** FLOW:

        N = REnd - LStart + 1
        IF (N <= Quick_Median_of_3_CutOff) THEN
            ! use median-of-3 as partitioning element
            M1 = Median_Of_Three(AVal, AInd, LStart, LStart+N/2, REnd)
            IF (M1 /= LStart) THEN
                EXCHANGE(AInd, M1, LStart)
            END IF
        ELSE
            ! use Tukey's ninther as partitioning element
            Eps = N/8
            Mid = LStart + N/2
            M1 = Median_Of_Three(AVal, AInd, LStart, LStart + Eps, LStart + Eps + Eps)
            M2 = Median_Of_Three(AVal, AInd, Mid - Eps, Mid, Mid + Eps)
            M3 = Median_Of_Three(AVal, AInd, REnd - Eps - Eps, REnd - Eps, REnd)
            Ninther = Median_Of_Three(AVal, AInd, M1, M2, M3)
            IF (Ninther /= LStart) THEN
                EXCHANGE(AInd, Ninther, LStart)
            END IF
        END IF

        ! Bentley-McIlroy's 3-way partitioning
        PInd = AInd(LStart)
        RStart = LStart
        LEnd = REnd+1
        P = RStart
        Q = LEnd
        OUTLOOP: DO

            RStart = RStart + 1
            DO WHILE (COMPARE_GLT(AVal(AInd(RStart)), AVal(PInd)))
                IF (RStart == REnd) EXIT
                RStart = RStart + 1
            END DO

            LEnd = LEnd - 1
            DO WHILE (COMPARE_GLT(AVal(PInd), AVal(AInd(LEnd))))
                IF (LEnd == LStart) EXIT
                LEnd = LEnd - 1
            END DO

            ! pointers cross
            IF ((RStart == LEnd).AND.(AVal(AInd(RStart)) == AVal(PInd))) THEN
                P = P + 1
                EXCHANGE(AInd, P, RStart)
            END IF
            IF (RStart >= LEnd) EXIT OUTLOOP

            EXCHANGE(AInd, RStart, LEnd)
            IF (AVal(AInd(RStart)) == AVal(PInd)) THEN
                P = P + 1
                EXCHANGE(AInd, P, RStart)
            END IF
            IF (AVal(AInd(LEnd)) == AVal(PInd)) THEN
                Q = Q - 1
                EXCHANGE(AInd, Q, LEnd)
            END IF
        END DO OUTLOOP

        RStart = LEnd + 1
        DO K = LStart, P
            EXCHANGE(AInd, K, LEnd)
            LEnd = LEnd - 1
        END DO
        DO K = REnd, Q, -1
            EXCHANGE(AInd, K, RStart)
            RStart = RStart + 1
        END DO

        RETURN

    END SUBROUTINE Partition_3Way

    !******************************************************************

END SUBROUTINE Quick_Rank_3Way

!**********************************************************************

MODULE SUBROUTINE Quick_Rank_Mo3(AVal, AInd)

!** PURPOSE OF THIS SUBROUTINE
    ! To determine indices such that they sort the specified array into the desired
    ! order using the quicksort algorithm with median-of-3 partitioning scheme.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)          ! array values
    tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal)) ! array indices

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: CutOff       ! cutoff to pair insertion sort
    tIndex          :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(AVal, KIND=kIndex)

    ! initialize the indices
    CALL Index_Init(NA, AInd)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Quick_Insert_CutOff_Low) THEN
        CALL DualInsert_Guarded(AVal, AInd, 1, NA)
        RETURN
    END IF

    ! set cutoff depending on the size of the input array
    IF (NA < 2000) THEN
        CutOff = Quick_Insert_CutOff_Low
    ELSE
        CutOff = Quick_Insert_CutOff_High
    END IF

    ! perform quick sort using tail recursion
    CALL QuickIndex_Recursive(AVal, AInd, 1, NA, CutOff, TrueVal, Partition_Mo3)

    RETURN

    CONTAINS

    SUBROUTINE Partition_Mo3(AVal,AInd,LStart,REnd,LEnd,RStart)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To partition an array using median-of-3 partitioning scheme.
        ! The given array in the range [LStart,REnd] is partitioned
        ! into two sub-arrays, Left and Right, such that all elements
        ! in the Left sub-array in the range [LStart,LEnd] are less
        ! than or equal to the selected pivot, and all elements in
        ! the Right sub-array in the range [RStart,REnd] are greater
        ! than or equal to the selected pivot.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)  ! array values
        tIndex,      INTENT(INOUT)  :: AInd(:)  ! array indices
        tIndex,      INTENT(IN)     :: LStart   ! on entry, the start of the given array
                                                ! on exit,  the start of the Left sub-array
        tIndex,      INTENT(IN)     :: REnd     ! on entry, the end of the given array
                                                ! on exit,  the end of the Right sub-array
        tIndex,      INTENT(OUT)    :: LEnd     ! the end of the Left sub-array
        tIndex,      INTENT(OUT)    :: RStart   ! the start of the Right sub-array

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: PInd, Temp
        tIndex          :: Mid, LPtr, UPtr
        tIndex          :: I, J, K

    !** FLOW:

        ! choose median of left, center, and right elements as partitioning element (the pivot)
        ! also, rearrange so that A(LStart) < A(LStart+1) < A(REnd)
        Mid = LStart + (REnd-LStart) / 2
        CALL Rank_3_Items(AVal, AInd, LStart, Mid, REnd)
        EXCHANGE(AInd, Mid, LStart+1)

        ! set pivot value
        PInd = AInd(LStart+1)

        ! set working pointers
        LPtr = LStart + 1
        UPtr = REnd

        DO
            ! increment the left pointer until we find an element
            ! that is greater than the pivot
            DO
                LPtr = LPtr + 1
                IF (COMPARE_GLE(AVal(PInd), AVal(AInd(LPtr)))) EXIT
            END DO

            ! decrement the right pointer until we find an element
            ! that is less than the pivot
            DO
                UPtr = UPtr - 1
                IF (COMPARE_GLE(AVal(AInd(UPtr)), AVal(PInd))) EXIT
            END DO

            ! exit if the pointers cross
            IF (UPtr < LPtr) EXIT

            ! swap AInd(LPtr) and AInd(UPtr)
            EXCHANGE(AInd, LPtr, UPtr)
        END DO

        ! insert partitioning element
        AInd(LStart+1) = AInd(UPtr)
        AInd(UPtr)     = PInd

        ! set output indices
        LEnd   = UPtr - 1
        RStart = LPtr

        RETURN

    END SUBROUTINE Partition_Mo3

    !******************************************************************

END SUBROUTINE Quick_Rank_Mo3

!**********************************************************************

MODULE SUBROUTINE Quick_Rank_Vowels(AVal, AInd)

!** PURPOSE OF THIS SUBROUTINE
    ! To determine indices such that they sort the specified array into the desired
    ! order using the quicksort algorithm based on the *QuickSort Version 3* (the
    ! professional version) by R.A. Vowels [1].

!** REFERENCES
!   [1] <a href="http://pages.swcp.com/~walt/fortran_store/Html/Info/books/adsff.html">
!       Robin A. Vowels. 1998. Algorithms and Data Structures in F and Fortran,
!       Unicomp. </a> <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)          ! array values
    tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal)) ! array indices

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: CutOff       ! cutoff to pair insertion sort
    tIndex          :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(AVal, KIND=kIndex)

    ! initialize the indices
    CALL Index_Init(NA, AInd)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Quick_Insert_CutOff_Low) THEN
        CALL DualInsert_Guarded(AVal, AInd, 1, NA)
        RETURN
    END IF

    ! set cutoff depending on the size of the input array
    IF (NA < 2000) THEN
        CutOff = Quick_Insert_CutOff_Low
    ELSE
        CutOff = Quick_Insert_CutOff_High
    END IF

    ! perform quick sort
    CALL QuickIndex(AVal, AInd, 1, NA, CutOff, TrueVal)

    RETURN

CONTAINS

    RECURSIVE SUBROUTINE QuickIndex(AVal, AInd,Low,High,CutOff,LeftMost)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To sort array in the range [Low,High] using recursive quick sort algorithm.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)  ! array values
        tIndex,      INTENT(INOUT)  :: AInd(:)  ! array indices
        tIndex,      INTENT(IN)     :: Low      ! the start of the range
        tIndex,      INTENT(IN)     :: High     ! the end of the range
        tIndex,      INTENT(IN)     :: CutOff   ! cutoff to pair insertion sort
        tLogical,    INTENT(IN)     :: LeftMost ! flag indicating whether the specified range
                                                ! is the left most of the given array
                                                ! If false, an unguarded pair insertion sorting
                                                ! algorithm is used instead of the guarded one.

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: Mid
        tIndex          :: Left, Right
        tIndex          :: RefInd           ! To hold the Reference Element.
        tIndex          :: Temp             ! A temporary for swapping elements, same type as A.
        tIndex          :: L                ! L = a pointer used when searching from the left.
        tIndex          :: R                ! R = a pointer, used when searching from the right.
        tIndex          :: No_Swaps         ! To count the number of swaps.
        tIndex          :: La, J, Rml, NP

    ! FLOW

        Left = Low
        Right = High
        IF (Right <= Left) THEN             ! The partition is empty or contains one element.
            RETURN                          ! There's no work to do.
        END IF

        NP = Right - Left + 1
        ! check whether to perform insertion sort instead
        IF (NP < CutOff) THEN
            IF (LeftMost) THEN
                CALL DualInsert_Guarded(AVal, AInd, Left, Right)
            ELSE
                CALL DualInsert_UnGuarded(AVal, AInd, Left, Right)
            END IF
            RETURN
        END IF

        ! Section to select the median of three elements, and to move it to the left.
        Mid = (Left + Right)/2
        IF (COMPARE_GLT(AVal(AInd(Right)), AVal(AInd(Mid))))  THEN
            EXCHANGE(AInd, Mid, Right)
        END IF

        IF (Left+1 == Right) THEN           ! There are 2 elements in the partition,
            RETURN                          ! & they are now in sort.
        END IF
        IF (COMPARE_GLT(AVal(AInd(Mid)), AVal(AInd(Left))))  THEN
            EXCHANGE(AInd, Left, Mid)
        END IF
        IF (COMPARE_GLT(AVal(AInd(Right)), AVal(AInd(Mid))))  THEN
            EXCHANGE(AInd, Mid, Right)
        END IF
        IF (Left+ 2 == Right) THEN          ! There are 3 elements in the partition,
            RETURN                          ! & they are now in sort.
        END IF
        IF (AVal(AInd(Mid)) == AVal(AInd(Right))) THEN  ! Some elements are equal!
            RefInd = AInd(Left)                         ! Forces the left partition to omit equal elements.
        ELSE
            EXCHANGE(AInd, Left, Mid)
            RefInd = AInd(Left)                         ! Select the Reference Element.
        END IF

        L = Left
        R = Right + 1

        ! Partition the elements into three groups.
        No_Swaps = 0
        DO
            IF (L >= R) THEN
                EXIT
            END IF
            DO L = L + 1, R - 1                                 ! Scan from the left for an element
                IF (COMPARE_GLT(AVal(RefInd), AVal(AInd(L)))) THEN  ! larger than the Reference.
                    EXIT
                END IF
            END DO

            DO                                                  ! Scan from the right for an element
                R = R - 1
                IF (COMPARE_GLE(AVal(AInd(R)), AVal(RefInd))) THEN ! less than or equal to the Reference.
                    EXIT
                END IF
            END DO

            IF (L < R) THEN                 ! Swap two elements that are in the
                                            ! wrong partitions.
                EXCHANGE(AInd, R, L)
                No_Swaps = No_Swaps + 1     ! Count each swap as we go.
            END IF
        END DO
                                            ! partitioning is complete.
        IF (Left < R) THEN                  ! Swap the Reference Element into its
                                            ! final position R in the array.
            AInd(Left) = AInd(R)
            AInd(R)    = RefInd
        END IF
        ! At this point, A(R) is in its correct position in the list.  Elements A(Left) to A(R-1)
        ! are less than or equal to A(R), and elements A(R+1) to A(Right) are greater then A(R).

        ! Section to find out why no swaps were performed.
        IF (No_Swaps == 0) THEN             ! Something funny happened: not one
                                            ! element was moved. Investigate cause.
            INCREASING: DO
                ! Look for any pre-existing order.
                DO J = Left, Right-1
                    IF (COMPARE_GLT(AVal(AInd(J+1)), AVal(AInd(J))))  THEN
                        EXIT  INCREASING
                    END IF
                END DO
                RETURN                      ! The elements are already in order.
            END DO  INCREASING

        ! Section to take a strong hand when the maximum number of elements is swapped.
        ! It's possible that the elements were in decreasing order.  Check it.
        ELSE IF (No_Swaps+1 == (Right-Left)/2) THEN
            ! All possible pairs were swapped.
            ! Perhaps the elements were in reverse order?  Find out why.
            DECREASING: DO
                Rml = Right - Left
                IF (IAND(Rml, 1) /= 0) THEN           ! A partition containing an even number
                                                        ! of elements was disarranged during
                                                        ! partitioning.
                    IF (Left < R-1)   THEN
                        EXCHANGE(AInd, Left, R-1)       ! Restore order.
                    END IF
                END IF
                DO J = Left, Right-1                    ! Check that the elements are sorted.
                    IF (COMPARE_GLT(AVal(AInd(J+1)), AVal(AInd(J)))) THEN
                        EXIT DECREASING
                    END IF
                END DO
                RETURN                                  ! The entire sub-list is sorted.
            END DO  DECREASING
        END IF

        DO La = R-1, Left+1, -1             ! Pass over any elements that are equal to Ref.
            IF (AVal(AInd(La)) /= AVal(RefInd)) THEN
                EXIT
            END IF
        END DO

        ! At this point, elements A(La+1) through A(R) are equal.
        ! A(L) is in its correct position too, even if it is not equal to A(Mid)!
        ! But if Left=La already, the partition was just lop-sided.

        IF (Left < La) THEN
            CALL QuickIndex (AVal, AInd, Left, La, CutOff, LeftMost)    ! Partition the left segment.
        END IF
                                            ! The element at R is in its correct position.
        IF (R+1 < Right) THEN
            CALL QuickIndex (AVal, AInd, R+1, Right, CutOff, FalseVal)  ! Partition the right segment.
        END IF

        RETURN

    END SUBROUTINE QuickIndex

    !******************************************************************

END SUBROUTINE Quick_Rank_Vowels

!**********************************************************************

MODULE SUBROUTINE Quick_Rank_Stable(AVal, AInd)

!** PURPOSE OF THIS SUBROUTINE
    ! To determine indices such that they sort the specified array into the desired
    ! order using the quicksort algorithm with stable partitioning scheme.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)          ! array values
    tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal)) ! array indices

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: Insert_CutOff_Low  = 75    ! cutoff to pair insertion
    tInteger, PARAMETER :: Insert_CutOff_High = 95    ! cutoff to pair insertion

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: CutOff       ! cutoff to pair insertion sort
    tIndex          :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(AVal, KIND=kIndex)

    ! initialize the indices
    CALL Index_Init(NA, AInd)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Insert_CutOff_Low) THEN
        CALL Insert_Guarded(AVal, AInd, 1, NA)
        RETURN
    END IF

    ! set cutoff depending on the size of the input array
    IF (NA < 2000) THEN
        CutOff = Insert_CutOff_Low
    ELSE
        CutOff = Insert_CutOff_High
    END IF

    ! perform quick sort using tail recursion
    CALL Quick_Rank_Recur(AVal, AInd, 1, NA, CutOff, TrueVal)

    RETURN

    CONTAINS

    SUBROUTINE Partition_Stable(AVal, AInd, LStart, REnd, LEnd, RStart)

    !** PURPOSE OF THIS SUBROUTINE
        ! To partition an array using stable partitioning scheme.
        ! The given array in the range [LStart,REnd] is partitioned
        ! into two sub-arrays, Left and Right, such that all elements
        ! in the Left sub-array in the range [LStart,LEnd] are less
        ! than or equal to the selected pivot, and all elements in
        ! the Right sub-array in the range [RStart,REnd] are greater
        ! than or equal to the selected pivot.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)  ! values of array to be partitioned
        tIndex,      INTENT(INOUT)  :: AInd(:)  ! array indices
        tIndex,      INTENT(IN)     :: LStart   ! on entry, the start of the given array
                                                ! on exit,  the start of the Left sub-array
        tIndex,      INTENT(IN)     :: REnd     ! on entry, the end of the given array
                                                ! on exit,  the end of the Right sub-array
        tIndex,      INTENT(OUT)    :: LEnd     ! the end of the Left sub-array
        tIndex,      INTENT(OUT)    :: RStart   ! the start of the Right sub-array

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: BInd(SIZE(AInd)) ! auxiliary array indices
        tIndex      :: PInd             ! index of the pivot used to partition the array
        tIndex      :: Less             ! current pointer for value less than the pivot
        tIndex      :: Great            ! current pointer for value greater than the pivot
        tIndex      :: Equal            ! current pointer for value equal to the pivot
        tIndex      :: Indx             ! index
        tIndex      :: BSize            ! size of BInd array
        tIndex      :: N, M1, M2, M3
        tIndex      :: Eps, Ninther

    !** FLOW:

        N = REnd - LStart + 1

        ! use Tukey's ninther as partitioning element (the pivot)
        ! also, rearrange so that A(LStart) < A(LStart+1) < A(REnd)
        Eps = N/8
        M1 = LStart + Eps
        M2 = LStart + N/2
        M3 = REnd - Eps
        M1 = Median_Of_Three(AVal, AInd, LStart, M1, M1+Eps)
        M2 = Median_Of_Three(AVal, AInd, M2-Eps, M2, M2+Eps)
        M3 = Median_Of_Three(AVal, AInd, M3-Eps, M3, REnd)
        Ninther = Median_Of_Three(AVal, AInd, M1, M2, M3)
        PInd = AInd(Ninther)

        ! initialize greater index
        BSize = SIZE(BInd)
        Less  = LStart - 1
        Equal = BSize + 1
        Great = 0

        ! perform supposedly-stable partition of the array
        DO Indx = LStart, REnd
            ! check whether the current element is greater in value than our pivot value.
            ! If so, save it to the buffer.  Otherwise, move it up front
            IF (COMPARE_GLT(AVal(PInd), AVal(AInd(Indx)))) THEN
                ! save greater value to the front of the buffer
                Great = Great + 1
                BInd(Great) = AInd(Indx)
            ELSEIF (COMPARE_GLT(AVal(AInd(Indx)), AVal(PInd))) THEN
                ! move less value to the front of the array
                Less = Less + 1
                IF (Less < Indx) AInd(Less) = AInd(Indx)
            ELSE
                ! save equal value to the back of the buffer
                Equal = Equal - 1
                BInd(Equal) = AInd(Indx)
            END IF
        END DO

        ! set maximum index of sub-arrays with less values
        LEnd = Less

        ! transfer data back from the buffer
        IF (Equal <= BSize) THEN
            ! first, transfer data with equal values
            DO Indx = BSize, Equal, -1
                Less = Less + 1
                AInd(Less) = BInd(Indx)
            END DO
        END IF
        Less = Less + 1
        IF (Great >= 1) THEN
            ! next, transfer data with greater values
            AInd(Less:REnd) = BInd(1:Great)
        END IF
        ! set mimimum index of sub-arrays with greater values
        RStart = Less

        RETURN

    END SUBROUTINE Partition_Stable

    !**********************************************************************

    RECURSIVE SUBROUTINE Quick_Rank_Recur(AVal,AInd,LStart,REnd,CutOff,LeftMost)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To sort the array in the range [LStart,REnd] into an ascending order
        ! using recursive quicksort algorithm.  If the number of elements in
        ! the range below the cutoff, then a pair-insertion sorting routine
        ! is used instead.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)          ! array values
        tIndex,      INTENT(INOUT)  :: AInd(:)          ! array indices
        tIndex,      INTENT(IN)     :: LStart           ! the start of the range
        tIndex,      INTENT(IN)     :: REnd             ! the end of the range
        tIndex,      INTENT(IN)     :: CutOff           ! cutoff to pair insertion sort
        tLogical,    INTENT(IN)     :: LeftMost         ! flag indicating whether the specified range
                                                        ! is the left most of the given array
                                                        ! If false, an unguarded pair insertion sorting
                                                        ! algorithm is used instead of the guarded one.

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: LEnd     ! the end of the left sub-array
        tIndex          :: RStart   ! the start of the right sub-array
        tIndex          :: NE       ! number of elements in the range

    !** FLOW:

        ! determine the number of elements in the specified range
        NE = REnd - LStart + 1

        ! check whether to perform pair insertion sort
        IF (NE <= CutOff) THEN
            ! check whether the specified range is the left most range of the given array
            IF (LeftMost) THEN
                CALL Insert_Guarded(AVal, AInd, LStart, REnd)
            ELSE
                CALL Insert_UnGuarded(AVal, AInd, LStart, REnd)
            END IF
            RETURN
        END IF

        ! perform partitioning
        CALL Partition_Stable(AVal, AInd, LStart, REnd, LEnd, RStart)

        ! perform quick sort on the two sub-arrays
        CALL Quick_Rank_Recur(AVal, AInd, LStart, LEnd, CutOff, LeftMost)
        CALL Quick_Rank_Recur(AVal, AInd, RStart, REnd, CutOff, FalseVal)

        RETURN

    END SUBROUTINE Quick_Rank_Recur

    !**********************************************************************

END SUBROUTINE Quick_Rank_Stable

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   ITERATIVE QUICKSORT DRIVER ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE SUBROUTINE Quick_Rank_Iterative(AVal, AInd)

!** PURPOSE OF THIS SUBROUTINE
    ! To determine indices such that they sort the specified array into the desired
    ! order using the iterative quicksort algorithm with median-of-3 partitioning scheme [1].

!** REFERENCES
!   [1] <a href="http://numerical.recipes/oldverswitcher.html">Numerical
!       Recipes Books Online. </a> <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)          ! array values
    tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal)) ! array indices

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: iStart, iEnd
    tIndex          :: CutOff
    tIndex          :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(AVal, KIND=kIndex)

    ! initialize the indices
    CALL Index_Init(NA, AInd)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! initialize
    iStart = 1
    iEnd = NA
    IF (NA < 2000) THEN
        CutOff = Quick_Insert_CutOff_Low
    ELSE
        CutOff = Quick_Insert_CutOff_High
    END IF

    ! perform quick sort
    CALL QuickIndex(AVal, AInd, iStart, iEnd, CutOff)

    RETURN

CONTAINS

    SUBROUTINE QuickIndex(AVal,AInd,Left,Right,CutOff)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)  ! array values
        tIndex,      INTENT(INOUT)  :: AInd(:)  ! array indices
        tIndex,      INTENT(IN)     :: Left
        tIndex,      INTENT(IN)     :: Right
        tIndex,      INTENT(IN)     :: CutOff

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: PInd, Temp
        tIndex          :: I, J, K, L, R
        tIndex          :: Ptr
        tIndex          :: Stack(2,STORAGE_SIZE(NA))

    !** FLOW:

        ! initialize
        Ptr = 1
        L = Left
        R = Right

        ! perform quick sort
        DO
            IF (R-L < CutOff) THEN
                ! perform insertion sort when sub-array small enough
                IF (L == Left) THEN
                    ! left-most sub-array
                    CALL DualInsert_Guarded(AVal, AInd, L, R)
                ELSE
                    ! not left-most sub-array
                    CALL DualInsert_UnGuarded(AVal, AInd, L, R)
                END IF
                IF (Ptr == 1) RETURN
                ! pop stack and begin a new round of partitioning
                Ptr = Ptr - 1
                L = Stack(1,Ptr)
                R = Stack(2,Ptr)
                CYCLE
            END IF
            ! choose median of left, center, and right elements as partitioning element A
            ! also, rearrange so that A(L) < A(L+1) < A(R)
            K = L + (R-L) / 2
            EXCHANGE(AInd, K, L+1)
            IF (COMPARE_GLT(AVal(AInd(R)), AVal(AInd(L)))) THEN
                EXCHANGE(AInd, L, R)
            END IF
            IF (COMPARE_GLT(AVal(AInd(R)), AVal(AInd(L+1)))) THEN
                EXCHANGE(AInd, L+1, R)
            END IF
            IF (COMPARE_GLT(AVal(AInd(L+1)), AVal(AInd(L)))) THEN
                EXCHANGE(AInd, L, L+1)
            END IF
            ! initialize pointers for partitioning
            I = L+1
            J = R
            ! partitioning element
            PInd = AInd(L+1)
            DO
                ! here is the meat
                DO
                    ! scan up to find element >= Pivot
                    I = I+1
                    IF (COMPARE_GLE(AVal(PInd), AVal(AInd(I)))) EXIT
                END DO
                DO
                    ! scan down to find element <= Pivot
                    J = J-1
                    IF (COMPARE_GLE(AVal(AInd(J)), AVal(PInd))) EXIT
                END DO
                ! pointers crossed. exit with partitioning complete.
                IF (J < I) EXIT
                EXCHANGE(AInd, I, J)
            END DO
            ! insert partitioning element
            AInd(L+1) = AInd(J)
            AInd(J)   = PInd
            ! push pointers to larger sub-array on stack and process smaller sub-array immediately.
            IF (R-I+1 >= J-L) THEN
                Stack(1,Ptr) = I
                Stack(2,Ptr) = R
                R = J-1
            ELSE
                Stack(1,Ptr) = L
                Stack(2,Ptr) = J-1
                L = I
            END IF
            Ptr = Ptr + 1
        END DO

        RETURN

    END SUBROUTINE QuickIndex

    !******************************************************************

END SUBROUTINE Quick_Rank_Iterative

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   DUAL-PIVOT QUICKSORT DRIVER ROUTINES
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE SUBROUTINE Quick_Rank_Java(AVal, AInd)

!** PURPOSE OF THIS SUBROUTINE
    ! To determine indices such that they sort the specified array into the desired
    ! order using the dual-pivot quicksort algorithm [1].

!** REFERENCES
!   [1] <a href="https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/util/DualPivotQuicksort.java">
!       Java's DualPivotQuicksort class. </a> <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)          ! array values
    tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal)) ! array indices

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: CutOff       ! cutoff to pair insertion sort
    tIndex          :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(AVal, KIND=kIndex)

    ! initialize the indices
    CALL Index_Init(NA, AInd)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Quick_Insert_CutOff_Low) THEN
        CALL DualInsert_Guarded(AVal, AInd, 1, NA)
        RETURN
    END IF

    ! set cutoff depending on the size of the input array
    IF (NA < 2000) THEN
        CutOff = Quick_Insert_CutOff_Low
    ELSE
        CutOff = Quick_Insert_CutOff_High
    END IF

    ! perform double-pivot quick sort
    CALL QuickIndexDP(AVal, AInd, 0, NA, CutOff)

    RETURN

CONTAINS

    RECURSIVE SUBROUTINE QuickIndexDP(AVal,AInd,Lo,Hi,CutOff)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To sort the array in the range [Lo,Hi-1] into an ascending order
        ! using double-pivot quicksort algorithm.  If the number of elements in
        ! the range below the cutoff, then a pair-insertion sorting routine
        ! is used instead.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)  ! array values
        tIndex,      INTENT(INOUT)  :: AInd(0:) ! array indices
        tIndex,      INTENT(IN)     :: Lo       ! the start of the range (incluisve)
        tIndex,      INTENT(IN)     :: Hi       ! the end of the range (exclusive)
        tIndex,      INTENT(IN)     :: CutOff   ! cutoff to pair insertion sort

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: Low, High, iEnd, Size
        tIndex          :: AInd3, AIndK, Temp
        tIndex          :: PInd, PInd1, PInd2
        tIndex          :: Step
        tIndex          :: K
        tIndex          :: E1, E2, E3, E4, E5
        tIndex          :: Lower, Upper

    !** FLOW:

        ! initialize
        Low  = Lo
        High = Hi

        DO WHILE (TrueVal)

            ! initialize
            iEnd = High - 1
            Size = High - Low

            ! check whether to perform insertion sort instead
            IF (Size < CutOff) THEN
                ! check whether the specified range is the left most range of the given array
                IF (Low == 0) THEN
                    CALL DualInsert_Guarded(AVal, AInd, 1, High)
                ELSE
                    CALL DualInsert_UnGuarded(AVal, AInd, Low+1, High)
                END IF
                RETURN
            END IF

            ! Use an inexpensive approximation of the golden ratio
            ! to select five sample elements and determine pivots.
            Step = SHIFTA(Size, 3) * 3 + 3

            ! Five elements around (and including) the central element
            ! will be used for pivot selection as described below. The
            ! unequal choice of spacing these elements was empirically
            ! determined to work well on a wide variety of inputs.
            E1 = Low + Step
            E5 = iEnd - Step
            E3 = SHIFTR((E1 + E5), 1)
            E2 = SHIFTR((E1 + E3), 1)
            E4 = SHIFTR((E3 + E5), 1)
            AInd3 = AInd(E3)

            ! Sort these elements in place by the combination
            ! of 4-element sorting network and insertion sort.
            !
            !    5 ------o-----------o------------
            !            |           |
            !    4 ------|-----o-----o-----o------
            !            |     |           |
            !    2 ------o-----|-----o-----o------
            !                  |     |
            !    1 ------------o-----o------------
            IF (COMPARE_GLT(AVal(AInd(E5)), AVal(AInd(E2)))) THEN
                ! t = AInd(E5); AInd(E5) = AInd(E2); AInd(E2) = t
                EXCHANGE(AInd, E5, E2)
            END IF
            IF (COMPARE_GLT(AVal(AInd(E4)), AVal(AInd(E1)))) THEN
                ! t = AInd(E4); AInd(E4) = AInd(E1); AInd(E1) = t
                EXCHANGE(AInd, E4, E1)
            END IF
            IF (COMPARE_GLT(AVal(AInd(E5)), AVal(AInd(E4)))) THEN
                ! t = AInd(E5); AInd(E5) = AInd(E4); AInd(E4) = t
                EXCHANGE(AInd, E5, E4)
            END IF
            IF (COMPARE_GLT(AVal(AInd(E2)), AVal(AInd(E1)))) THEN
                ! t = AInd(E2); AInd(E2) = AInd(E1); AInd(E1) = t
                EXCHANGE(AInd, E2, E1)
            END IF
            IF (COMPARE_GLT(AVal(AInd(E4)), AVal(AInd(E2)))) THEN
                ! t = AInd(E4); AInd(E4) = AInd(E2); AInd(E2) = t
                EXCHANGE(AInd, E4, E2)
            END IF

            IF (COMPARE_GLT(AVal(AInd3), AVal(AInd(E2)))) THEN
                IF (COMPARE_GLT(AVal(AInd3), AVal(AInd(E1)))) THEN
                    AInd(E3) = AInd(E2)
                    AInd(E2) = AInd(E1)
                    AInd(E1) = AInd3
                ELSE
                    AInd(E3) = AInd(E2)
                    AInd(E2) = AInd3
                END IF
            ELSEIF (COMPARE_GLT(AVal(AInd(E4)), AVal(AInd3))) THEN
                IF (COMPARE_GLT(AVal(AInd(E5)), AVal(AInd3))) THEN
                    AInd(E3) = AInd(E4)
                    AInd(E4) = AInd(E5)
                    AInd(E5) = AInd3
                ELSE
                    AInd(E3) = AInd(E4)
                    AInd(E4) = AInd3
                END IF
            END IF

            ! Pointers
            Lower = Low     ! The index of the last element of the left part
            Upper = iEnd    ! The index of the first element of the right part

            ! Partitioning with 2 pivots in case of different elements.
            IF (COMPARE_GLT(AVal(AInd(E1)), AVal(AInd(E2))) .AND. &
                COMPARE_GLT(AVal(AInd(E2)), AVal(AInd(E3))) .AND. &
                COMPARE_GLT(AVal(AInd(E3)), AVal(AInd(E4))) .AND. &
                COMPARE_GLT(AVal(AInd(E4)), AVal(AInd(E5)))) THEN

                ! Use the first and fifth of the five sorted elements as
                ! the pivots. These values are inexpensive approximation
                ! of tertiles. Note, that Pivot1 < Pivot2.
                PInd1 = AInd(E1)
                PInd2 = AInd(E5)

                ! The first and the last elements to be sorted are moved
                ! to the locations formerly occupied by the pivots. When
                ! partitioning is completed, the pivots are swapped back
                ! into their final positions, and excluded from the next
                ! subsequent sorting.
                AInd(E1) = AInd(Lower)
                AInd(E5) = AInd(Upper)

                ! Skip elements, which are less or greater than the pivots.
                Lower = Lower + 1
                DO WHILE (COMPARE_GLT(AVal(AInd(Lower)), AVal(PInd1)))
                    Lower = Lower + 1
                END DO
                Upper = Upper - 1
                DO WHILE (COMPARE_GLT(AVal(PInd2), AVal(AInd(Upper))))
                    Upper = Upper - 1
                END DO

                ! Backward 3-interval partitioning
                !
                !   left part                 central part          right part
                ! +------------------------------------------------------------+
                ! |  < Pivot1  |   ?   |  Pivot1 <= && <= Pivot2  |  > Pivot2  |
                ! +------------------------------------------------------------+
                !             ^       ^                            ^
                !             |       |                            |
                !           Lower     K                          Upper
                !
                ! Invariants:
                !
                !              all in (Low, Lower) < Pivot1
                !    Pivot1 <= all in (K, Upper)  <= Pivot2
                !              all in [Upper, iEnd) > Pivot2
                !
                ! Pointer K is the last index of ?-part
                Lower = Lower - 1
                Upper = Upper + 1
                K = Upper - 1
                DO WHILE (K > Lower)
                    AIndK = AInd(K)
                    IF (COMPARE_GLT(AVal(AIndK), AVal(PInd1))) THEN
                        ! Move AInd(K) to the left side
                        DO WHILE (Lower < K)
                            Lower = Lower + 1
                            IF (COMPARE_GLE(AVal(PInd1), AVal(AInd(Lower)))) THEN
                                IF (COMPARE_GLT(AVal(PInd2), AVal(AInd(Lower)))) THEN
                                    Upper = Upper - 1
                                    AInd(K) = AInd(Upper)
                                    AInd(Upper) = AInd(Lower)
                                ELSE
                                    AInd(K) = AInd(Lower)
                                END IF
                                AInd(Lower) = AIndK
                                EXIT
                            END IF
                        END DO
                    ELSEIF (COMPARE_GLT(AVal(PInd2), AVal(AIndK))) THEN
                        ! Move AInd(K) to the right side
                        Upper = Upper - 1
                        AInd(K) = AInd(Upper)
                        AInd(Upper) = AIndK
                    END IF
                    K = K - 1
                END DO

                ! Swap the pivots into their final positions.
                AInd(Low) = AInd(Lower)
                AInd(Lower) = PInd1
                AInd(iEnd) = AInd(Upper)
                AInd(Upper) = PInd2

                ! Sort non-left parts recursively ,
                ! excluding known pivots.
                CALL QuickIndexDP(AVal, AInd, Lower + 1, Upper, CutOff)
                CALL QuickIndexDP(AVal, AInd, Upper + 1, High, CutOff)

            ELSE    ! Use single pivot in case of many equal elements

                ! Use the third of the five sorted elements as the pivot.
                ! This value is inexpensive approximation of the median.
                PInd = AInd(E3)

                ! The first element to be sorted is moved to the
                ! location formerly occupied by the pivot. After
                ! completion of partitioning the pivot is swapped
                ! back into its final position, and excluded from
                ! the next subsequent sorting.
                AInd(E3) = AInd(Lower)

                ! Traditional 3-way (Dutch National Flag) partitioning
                !
                !   left part                 central part    right part
                ! +------------------------------------------------------+
                ! |   < Pivot   |     ?     |   == Pivot   |   > Pivot   |
                ! +------------------------------------------------------+
                !              ^           ^                ^
                !              |           |                |
                !            Lower         K              Upper
                !
                ! Invariants:
                !
                !   all in (Low, Lower) < Pivot
                !   all in (K, Upper)  == Pivot
                !   all in [Upper, iEnd) > Pivot
                !
                ! Pointer K is the last index of ?-part
                Upper = Upper + 1
                K = Upper - 1
                DO WHILE (K > Lower)
                    AIndK = AInd(K)

                    IF (AVal(AIndK) /= AVal(PInd)) THEN
                        AInd(K) = PInd

                        IF (COMPARE_GLT(AVal(AIndK), AVal(PInd))) THEN
                            ! Move AInd(K) to the left side
                            Lower = Lower + 1
                            DO WHILE (COMPARE_GLT(AVal(AInd(Lower)), AVal(PInd)))
                                Lower = Lower + 1
                            END DO

                            IF (COMPARE_GLT(AVal(PInd), AVal(AInd(Lower)))) THEN
                                Upper = Upper - 1
                                AInd(Upper) = AInd(Lower)
                            END IF
                            AInd(Lower) = AIndK
                        ELSE
                            ! AK > Pivot - Move AInd(K) to the right side
                            Upper = Upper - 1
                            AInd(Upper) = AIndK
                        END IF
                    END IF
                    K = K - 1
                END DO

                ! Swap the pivot into its final position.
                AInd(Low) = AInd(Lower)
                AInd(Lower) = PInd

                ! Sort the right part , excluding known pivot.
                ! All elements from the central part are equal
                ! and therefore already sorted.
                CALL QuickIndexDP(AVal, AInd, Upper, High, CutOff)
            END IF
            High = Lower    ! Iterate along the left part

        END DO

        RETURN

    END SUBROUTINE QuickIndexDP

    !******************************************************************

END SUBROUTINE Quick_Rank_Java

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   RECURSIVE QUICKSORT WORKING ROUTINES
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

RECURSIVE SUBROUTINE QuickIndex_Recursive(AVal,AInd,LStart,REnd,CutOff,LeftMost,Partition_Array)

!** PURPOSE OF THIS SUBROUTINE
	! To sort the array in the range [LStart,REnd] into an ascending order
    ! using recursive quicksort algorithm.  If the number of elements in
    ! the range below the cutoff, then a pair-insertion sorting routine
    ! is used instead.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)          ! array values
    tIndex,    INTENT(INOUT)    :: AInd(:)          ! array indices
    tIndex,    INTENT(IN)       :: LStart           ! the start of the range
    tIndex,    INTENT(IN)       :: REnd             ! the end of the range
    tIndex,    INTENT(IN)       :: CutOff           ! cutoff to pair insertion sort
    tLogical,  INTENT(IN)       :: LeftMost         ! flag indicating whether the specified range
                                                    ! is the left most of the given array
                                                    ! If false, an unguarded pair insertion sorting
                                                    ! algorithm is used instead of the guarded one.
    PROCEDURE(Partition)        :: Partition_Array  ! partitioning routine

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: LEnd     ! the end of the left sub-array
    tIndex          :: RStart   ! the start of the right sub-array
    tIndex          :: NE       ! number of elements in the range

!** FLOW:

    ! determine the number of elements in the specified range
    NE = REnd - LStart + 1

    ! check whether to perform pair insertion sort
    IF (NE <= CutOff) THEN
        ! check whether the specified range is the left most range of the given array
        IF (LeftMost) THEN
            CALL DualInsert_Guarded(AVal, AInd, LStart, REnd)
        ELSE
            CALL DualInsert_UnGuarded(AVal, AInd, LStart, REnd)
        END IF
        RETURN
    END IF

    ! perform partitioning
    CALL Partition_Array(AVal, AInd, LStart, REnd, LEnd, RStart)

    ! perform quick sort on the two sub-arrays
    CALL QuickIndex_Recursive(AVal, AInd, LStart, LEnd, CutOff, LeftMost, Partition_Array)
    CALL QuickIndex_Recursive(AVal, AInd, RStart, REnd, CutOff, FalseVal, Partition_Array)

    RETURN

END SUBROUTINE QuickIndex_Recursive

!**********************************************************************

RECURSIVE SUBROUTINE QuickIndex_TailRecur(AVal,AInd,Left,Right,CutOff,LeftMost,Partition_Array)

!** PURPOSE OF THIS SUBROUTINE
	! To sort the array in the range [Left,Right] into an ascending order
    ! using recursive quicksort algorithm with a tail recursion.  If the
    ! number of elements in the range below the cutoff, then a pair-insertion
    ! sorting routine is used instead.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument,   INTENT(IN)     :: AVal(:)          ! array values
    tIndex,      INTENT(INOUT)  :: AInd(:)          ! array indices
    tIndex,      INTENT(IN)     :: Left             ! the start of the range
    tIndex,      INTENT(IN)     :: Right            ! the end of the range
    tIndex,      INTENT(IN)     :: CutOff           ! cutoff to pair insertion sort
    tLogical,    INTENT(IN)     :: LeftMost         ! flag indicating whether the specified range
                                                    ! is the left most of the given array
                                                    ! If false, an unguarded pair insertion sorting
                                                    ! algorithm is used instead of the guarded one.
    PROCEDURE(Partition)        :: Partition_Array  ! partitioning routine

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: LStart       ! the start of the left sub-array
    tIndex          :: LEnd         ! the end of the left sub-array
    tIndex          :: RStart       ! the start of the right sub-array
    tIndex          :: REnd         ! the end of the right sub-array
    tIndex          :: NE           ! number of elements in the range
    tLogical        :: IsLeftMost   ! local working variable for input flag

!** FLOW:

    ! initialize
    LStart = Left
    REnd   = Right
    IsLeftMost = LeftMost

    DO WHILE (LStart < REnd)

        ! determine the number of elements in the current range
        NE = REnd - LStart + 1

        ! check whether to perform pair insertion sort
        IF (NE <= CutOff) THEN
            ! check whether the specified range is the left most range of the given array
            IF (IsLeftMost) THEN
                CALL DualInsert_Guarded(AVal, AInd, LStart, REnd)
            ELSE
                CALL DualInsert_UnGuarded(AVal, AInd, LStart, REnd)
            END IF
            EXIT
        END IF

        ! perform partitioning
        CALL Partition_Array(AVal, AInd, LStart, REnd, LEnd, RStart)

        ! recur on the smaller sub-array
        IF ((LEnd-LStart) < (REnd-RStart)) THEN
            CALL QuickIndex_TailRecur(AVal, AInd, LStart, LEnd, CutOff, IsLeftMost, Partition_Array)
            LStart = RStart
            IsLeftMost = FalseVal
        ELSE
            CALL QuickIndex_TailRecur(AVal, AInd, RStart, REnd, CutOff, FalseVal, Partition_Array)
            REnd = LEnd
            IsLeftMost = LeftMost
        END IF

    END DO

    RETURN

END SUBROUTINE QuickIndex_TailRecur

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                       MERGE SORT ROUTINES
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   MERGESORT ROUTINES
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE SUBROUTINE Merge_Rank_TopDown(AVal, AInd)

!** PURPOSE OF THIS SUBROUTINE
    ! To determine indices such that they sort the specified array into the desired
    ! order using the top-down merge sort algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)          ! array values
    tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal)) ! array indices

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: Insertion_CutOff_Low  = 35   ! cutoff to pair insertion sort
    tInteger, PARAMETER :: Insertion_CutOff_High = 40   ! cutoff to pair insertion sort

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: CutOff       ! cutoff to pair insertion sort
    tIndex      :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(AVal, KIND=kIndex)

    ! initialize the indices
    CALL Index_Init(NA, AInd)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Insertion_CutOff_Low) THEN
        CALL Insert_Guarded(AVal, AInd, 1_kIndex, NA)
        RETURN
    END IF

    ! set cutoff depending on the size of the input array
    IF (NA < 2000_kIndex) THEN
        CutOff = Insertion_CutOff_Low
    ELSE
        CutOff = Insertion_CutOff_High
    END IF

    BLOCK
        tIndex  :: BInd(NA)
        ! copy data from AInd
        BInd = AInd
        ! perform merge sort
        CALL TopDown_Split_Merge(AVal, BInd, 0_kIndex, NA, AInd, CutOff)
    END BLOCK

    RETURN

CONTAINS

    RECURSIVE SUBROUTINE TopDown_Split_Merge(AVal, BInd, Left, Right, AInd, CutOff)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To split A into 2 sub-arrays and sort both sub-arrays into B, and
        ! then merge both sub-arrays from B to A.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)  ! array values
        tIndex,      INTENT(INOUT)  :: BInd(0:) ! array indices (source)
        tIndex,      INTENT(IN)     :: Left     ! the start of the range
        tIndex,      INTENT(IN)     :: Right    ! the end of the range
        tIndex,      INTENT(INOUT)  :: AInd(0:) ! array indices (destination)
        tIndex,      INTENT(IN)     :: CutOff   ! cutoff to pair insertion sort

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: NE, Mid

    !** FLOW:

        NE = Right - Left
        IF (NE > CutOff) THEN

            ! split the array longer than 1 item into halves.
            Mid = Left + (Right-Left)/2

            ! recursively sort both sub-arrays from array A into array B
            CALL TopDown_Split_Merge(AVal, AInd, Left, Mid,   BInd, CutOff)
            CALL TopDown_Split_Merge(AVal, AInd, Mid,  Right, BInd, CutOff)

            IF (COMPARE_GLE(AVal(BInd(Mid-1)), AVal(BInd(Mid)))) THEN
                ! array B has already been sorted so just copy it to array A
                AInd(Left:Right-1) = BInd(Left:Right-1)
            ELSE
                ! merge the resulting sub-arrays from array B into array A
                CALL Merging_Wiki(AVal, BInd, Left, Mid, Right, AInd)
            END IF

        ELSE
            CALL Insert_Guarded(AVal, AInd, Left+1, Right)
        END IF

        RETURN

    END SUBROUTINE TopDown_Split_Merge

    !******************************************************************

END SUBROUTINE Merge_Rank_TopDown

!**********************************************************************

MODULE SUBROUTINE Merge_Rank_BottomUp(AVal, AInd)

!** PURPOSE OF THIS SUBROUTINE
    ! To determine indices such that they sort the specified array into the desired
    ! order using the bottom-up merge sort algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)          ! array values
    tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal)) ! array indices

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: Insertion_CutOff_Low  = 20   ! cutoff to pair insertion sort
    tInteger, PARAMETER :: Insertion_CutOff_High = 50   ! cutoff to pair insertion sort

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: CutOff       ! cutoff to pair insertion sort
    tIndex      :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(AVal, KIND=kIndex)

    ! initialize the indices
    CALL Index_Init(NA, AInd)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Insertion_CutOff_Low) THEN
        CALL Insert_Guarded(AVal, AInd, 1_kIndex, NA)
        RETURN
    END IF

    ! set cutoff depending on the size of the input array
    IF (NA < 2000_kIndex) THEN
        CutOff = Insertion_CutOff_Low
    ELSE
        CutOff = Insertion_CutOff_High
    END IF

    BLOCK
        tIndex  :: BInd(NA)
        ! perform merge sort
        CALL BottomUp_Merge(AVal, AInd, BInd, NA, CutOff)
    END BLOCK

    RETURN

CONTAINS

    SUBROUTINE BottomUp_Merge(AVal, AInd, BInd, NE, Runs)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To split A into 2 sub-arrays and sort both sub-arrays into B, and
        ! then merge both sub-arrays from B to A.

    !** REFERENCES
        ! Pseudocode from Wikipedia

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)  ! array values
        tIndex,      INTENT(INOUT)  :: AInd(0:) ! array indices
        tIndex,      INTENT(INOUT)  :: BInd(0:) ! array indices (auxiliary)
        tIndex,      INTENT(IN)     :: NE       ! size of the array
        tIndex,      INTENT(IN)     :: Runs     ! number of elements of sub-arrays to be sorted
                                                ! by pair insertion sort

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: Left, Mid, Right
        tIndex          :: Width
        tLogical        :: Switch

    !** FLOW:

        ! sort individual sub-arrays of size Runs
        Left  = 1
        Right = MIN(Runs, NE)
        DO
            CALL Insert_Guarded(AVal, AInd, Left, Right)
            IF (Right == NE) EXIT
            Left = Left + Runs
            Right = MIN(Right+Runs, NE)
        END DO

        ! set flag
        Switch = FalseVal

        ! Each sub-array with Run elements in A is already "sorted".
        ! Make successively longer sorted sub-arrays of length 2*Runs, 4*Runs, 8*Runs, 16*Runs...
        ! until the whole array is sorted.
        Width = Runs
        DO WHILE (Width < NE)

            ! Ptr1 array is full of sub-arrays of length Width
            Left = 0
            DO WHILE (Left < NE)

                Mid = MIN(Left+Width, NE)
                Right = MIN(Mid+Width, NE)
                IF (Switch) THEN
                    IF (COMPARE_GLE(AVal(BInd(Mid-1)), AVal(BInd(Mid)))) THEN
                        ! Merely copy the two sub-arrays: B(Left:Mid-1) and B(Mid:Right-1) into A()
                        AInd(Left:Right-1) = BInd(Left:Right-1)
                    ELSE
                        ! Merge two sub-arrays: B(Left:Mid-1) and B(Mid:Right-1) into A()
                        CALL Merging_Wiki(AVal, BInd, Left, Mid, Right, AInd)
                    END IF
                ELSE
                    IF (COMPARE_GLE(AVal(AInd(Mid-1)), AVal(AInd(Mid)))) THEN
                        ! Merely copy the two sub-arrays: A(Left:Mid-1) and A(Mid:Right-1) into B()
                        BInd(Left:Right-1) = AInd(Left:Right-1)
                    ELSE
                        ! Merge two sub-arrays: A(Left:Mid-1) and A(Mid:Right-1) into B()
                        CALL Merging_Wiki(AVal, AInd, Left, Mid, Right, BInd)
                    END IF
                END IF
                Left = Right
            END DO

            ! set flag
            Switch = .NOT.Switch

            ! Now array A is full of sub-arrays of length 2*Width.
            Width = 2*Width
        END DO

        ! if the Switch flag is true, copy final result from B to A
        IF (Switch) AInd = BInd

        RETURN

    END SUBROUTINE BottomUp_Merge

    !******************************************************************

END SUBROUTINE Merge_Rank_BottomUp

!**********************************************************************

MODULE SUBROUTINE Merge_Rank_QuadSplit(AVal, AInd)

!** PURPOSE OF THIS SUBROUTINE
    ! To determine indices such that they sort the specified array into the desired
    ! order using the top-down merge sort algorithm where the array is split into
    ! four sub-arrays instead of two sub-arrays.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)          ! array values
    tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal)) ! array indices

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: Insertion_CutOff_Low  = 35   ! cutoff to pair insertion sort
    tInteger, PARAMETER :: Insertion_CutOff_High = 40   ! cutoff to pair insertion sort

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: CutOff       ! cutoff to pair insertion sort
    tIndex      :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(AVal, KIND=kIndex)

    ! initialize the indices
    CALL Index_Init(NA, AInd)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Insertion_CutOff_Low) THEN
        CALL Insert_Guarded(AVal, AInd, 1_kIndex, NA)
        RETURN
    END IF

    ! set cutoff depending on the size of the input array
    IF (NA < 2000_kIndex) THEN
        CutOff = Insertion_CutOff_Low
    ELSE
        CutOff = Insertion_CutOff_High
    END IF

    BLOCK
        tIndex  :: BInd(NA)
        ! perform merge sort
        CALL Quad_Split_Merge(AVal, AInd, 0_kIndex, NA, BInd, CutOff)
    END BLOCK

    RETURN

CONTAINS

    RECURSIVE SUBROUTINE Quad_Split_Merge(AVal, AInd, Left, Right, BInd, CutOff)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To split A into 4 sub-arrays and sort them into B, and
        ! then merge the four sub-arrays from B to A.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(IN)     :: AVal(:)  ! array values
        tIndex,      INTENT(INOUT)  :: AInd(0:) ! array indices
        tIndex,      INTENT(IN)     :: Left     ! the start of the range
        tIndex,      INTENT(IN)     :: Right    ! the end of the range
        tIndex,      INTENT(INOUT)  :: BInd(0:) ! array indices (auxialiary array)
        tIndex,      INTENT(IN)     :: CutOff   ! cutoff to pair insertion sort

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: NE, NOver4, Mid, LeftMid, RightMid

    !** FLOW:

        NE = Right - Left
        IF (NE > CutOff) THEN

            ! split the array into four parts.
            NOver4   = NE / 4
            LeftMid  = Left + NOver4
            Mid      = LeftMid + NOver4
            RightMid = Mid + NOver4

            ! recursively split into four sub-arrays
            CALL Quad_Split_Merge(AVal, AInd, Left,     LeftMid,  BInd, CutOff)
            CALL Quad_Split_Merge(AVal, AInd, LeftMid,  Mid,      BInd, CutOff)
            CALL Quad_Split_Merge(AVal, AInd, Mid,      RightMid, BInd, CutOff)
            CALL Quad_Split_Merge(AVal, AInd, RightMid, Right,    BInd, CutOff)

            IF (COMPARE_GLE(AVal(AInd(LeftMid-1)), AVal(AInd(LeftMid)))) THEN
                ! merely copy the resulting sub-arrays into array B
                BInd(Left:Mid-1) = AInd(Left:Mid-1)
            ELSE
                ! merge the resulting sub-arrays into array B
                CALL Merging_Wiki(AVal, AInd, Left,  LeftMid,  Mid,   BInd)
            END IF
            IF (COMPARE_GLE(AVal(AInd(RightMid-1)), AVal(AInd(RightMid)))) THEN
                ! merely copy the resulting sub-arrays into array B
                BInd(Mid:Right-1) = AInd(Mid:Right-1)
            ELSE
                ! merge the resulting sub-arrays into array B
                CALL Merging_Wiki(AVal, AInd, Mid, RightMid, Right, BInd)
            END IF

            IF (COMPARE_GLE(AVal(BInd(Mid-1)), AVal(BInd(Mid)))) THEN
                ! array B has already been sorted so just copy it to array A
                AInd(Left:Right-1) = BInd(Left:Right-1)
            ELSE
                ! merge the sub-arrays of array B into array A
                CALL Merging_Wiki(AVal, BInd, Left, Mid, Right, AInd)
            END IF
        ELSE
            CALL Insert_Guarded(AVal, AInd, Left+1, Right)
        END IF

        RETURN

    END SUBROUTINE Quad_Split_Merge

    !******************************************************************

END SUBROUTINE Merge_Rank_QuadSplit

!**********************************************************************

MODULE SUBROUTINE Merge_Rank_OrderPack(AVal, AInd)

!** PURPOSE OF THIS SUBROUTINE
    ! To determine indices such that they sort the specified array into the desired
    ! order using a merge sort algorithm based on 'MrgRnk' routine in the *OrderPack*
    ! 2.0 library by Olagnon [1].

!** REFERENCE:
!   [1] <a href="http://www.fortran-2000.com/rank/">ORDERPACK 2.0: Unconditional,
!       Unique, and Partial Ranking, Sorting, and Permutation Downloadable Fortran90
!       source code. </a> <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)          ! array values
    tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal)) ! array indices

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: Insertion_CutOff_Init = 30   ! cutoff to pair insertion sort

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(AVal, KIND=kIndex)

    ! initialize the indices
    CALL Index_Init(NA, AInd)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Insertion_CutOff_Init) THEN
        CALL Insert_Guarded(AVal, AInd, 1_kIndex, NA)
        RETURN
    END IF

    ! perform merge sort
    CALL Merge_Rank(NA, AVal, AInd)

    RETURN

    CONTAINS

    SUBROUTINE Merge_Rank(NVal, AVal, AInd)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To determine indices such that they sort the array
        ! in ascending order using merge sort

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex,    INTENT(IN)     :: NVal     ! size of the arrays
        tArgument, INTENT(IN)     :: AVal(:)  ! array values
        tIndex,    INTENT(INOUT)  :: AInd(:)  ! array indices

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tVarScalar      :: AVal1, AVal2
        tIndex          :: IndWrk(NVal)
        tIndex          :: LmtNA, LmtNC, IRng1, IRng2
        tIndex          :: IndI, IWrkD, IWrk
        tIndex          :: IndJA, IndIA, IndIB, IWrkF

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(AVal1, MOLD=AVal(1))
        ALLOCATE(AVal2, MOLD=AVal(1))
#endif

        !  FILL-IN THE INDEX ARRAY, CREATING ORDERED COUPLES
        DO IndI = 2, NVal, 2
            IF (COMPARE_GLE(AVal(IndI-1), AVal(IndI))) THEN
                AInd(IndI-1) = IndI - 1
                AInd(IndI)   = IndI
            ELSE
                AInd(IndI-1) = IndI
                AInd(IndI)   = IndI - 1
            END IF
        END DO
        IF (MODULO(NVal, 2) /= 0) THEN
            AInd(NVal) = NVal
        END IF
        !  WE WILL NOW HAVE ORDERED SUBSETS A - B - A - B - ...
        !  AND MERGE A AND B COUPLES INTO     C   -   C   - ...
        LmtNA = 2
        LmtNC = 4
        !  FIRST ITERATION. THE LENGTH OF THE ORDERED SUBSETS GOES FROM 2 TO 4
        DO
            IF (NVal <= 2) EXIT
            !   LOOP ON MERGES OF A AND B INTO C
            DO IWrkD = 0, NVal - 1, 4
                IF ((IWrkD+4) > NVal) THEN
                    IF ((IWrkD+2) >= NVal) EXIT
                    !   1 2 3
                    IF (COMPARE_GLE(AVal(AInd(IWrkD+2)), AVal(AInd(IWrkD+3)))) EXIT
                    !   1 3 2
                    IF (COMPARE_GLE(AVal(AInd(IWrkD+1)), AVal(AInd(IWrkD+3)))) THEN
                        IRng2 = AInd (IWrkD+2)
                        AInd(IWrkD+2) = AInd(IWrkD+3)
                        AInd(IWrkD+3) = IRng2
                    !   3 1 2
                    ELSE
                        IRng1 = AInd(IWrkD+1)
                        AInd(IWrkD+1) = AInd(IWrkD+3)
                        AInd(IWrkD+3) = AInd(IWrkD+2)
                        AInd(IWrkD+2) = IRng1
                    END IF
                    EXIT
                END IF
                !   1 2 3 4
                IF (COMPARE_GLE(AVal(AInd(IWrkD+2)), AVal(AInd(IWrkD+3)))) CYCLE
                !   1 3 X X
                IF (COMPARE_GLE(AVal(AInd(IWrkD+1)), AVal(AInd(IWrkD+3)))) THEN
                    IRng2 = AInd (IWrkD+2)
                    AInd(IWrkD+2) = AInd(IWrkD+3)
                    IF (COMPARE_GLE(AVal(IRng2), AVal(AInd(IWrkD+4)))) THEN
                        !   1 3 2 4
                        AInd(IWrkD+3) = IRng2
                    ELSE
                        !   1 3 4 2
                        AInd(IWrkD+3) = AInd(IWrkD+4)
                        AInd(IWrkD+4) = IRng2
                    END IF
                !   3 X X X
                ELSE
                    IRng1 = AInd(IWrkD+1)
                    IRng2 = AInd(IWrkD+2)
                    AInd(IWrkD+1) = AInd(IWrkD+3)
                    IF (COMPARE_GLE(AVal(IRng1), AVal(AInd(IWrkD+4)))) THEN
                        AInd(IWrkD+2) = IRng1
                        IF (COMPARE_GLE(AVal(IRng2), AVal(AInd(IWrkD+4)))) THEN
                            !   3 1 2 4
                            AInd(IWrkD+3) = IRng2
                        ELSE
                            !   3 1 4 2
                            AInd(IWrkD+3) = AInd(IWrkD+4)
                            AInd(IWrkD+4) = IRng2
                        END IF
                    ELSE
                        !   3 4 1 2
                        AInd(IWrkD+2) = AInd(IWrkD+4)
                        AInd(IWrkD+3) = IRng1
                        AInd(IWrkD+4) = IRng2
                    END IF
                END IF
            END DO
            !  THE CS BECOME AS AND BS
            LmtNA = 4
            EXIT
        END DO
        !  ITERATION LOOP. EACH TIME, THE LENGTH OF THE ORDERED SUBSETS
        !  IS DOUBLED.
        DO
            IF (LmtNA >= NVal) EXIT
            IWrkF = 0
            LmtNC = 2 * LmtNC
            !   LOOP ON MERGES OF A AND B INTO C
            DO
                IWrk = IWrkF
                IWrkD = IWrkF + 1
                IndJA = IWrkF + LmtNA
                IWrkF = IWrkF + LmtNC
                IF (IWrkF >= NVal) THEN
                    IF (IndJA >= NVal) EXIT
                    IWrkF = NVal
                END IF
                IndIA = 1
                IndIB = IndJA + 1
                !   SHORTCUT FOR THE CASE WHEN THE MAX OF A IS SMALLER
                !   THAN THE MIN OF B. THIS LINE MAY BE ACTIVATED WHEN THE
                !   INITIAL SET IS ALREADY CLOSE TO SORTED.
                !
                !          IF (AVal(AInd(IndJA)) <= AVal(AInd(IndIB))) CYCLE
                !
                !  ONE STEPS IN THE C SUBSET, THAT WE BUILD IN THE FINAL RANK ARRAY
                !
                !  MAKE A COPY OF THE RANK ARRAY FOR THE MERGE ITERATION
                IndWrk(1:LmtNA) = AInd(IWrkD:IndJA)
                AVal1 = AVal(IndWrk(IndIA))
                AVal2 = AVal(AInd(IndIB))
                !
                DO
                    IWrk = IWrk + 1
                    !  WE STILL HAVE UNPROCESSED VALUES IN BOTH A AND B
                    IF (COMPARE_GLT(AVal2, AVal1)) THEN
                        AInd(IWrk) = AInd(IndIB)
                        IndIB = IndIB + 1
                        IF (IndIB > IWrkF) THEN
                            !  ONLY A STILL WITH UNPROCESSED VALUES
                            AInd(IWrk+1:IWrkF) = IndWrk(IndIA:LmtNA)
                            EXIT
                        END IF
                        AVal2 = AVal(AInd(IndIB))
                    ELSE
                        AInd(IWrk) = IndWrk(IndIA)
                        IndIA = IndIA + 1
                        IF (IndIA > LmtNA) EXIT ! ONLY B STILL WITH UNPROCESSED VALUES
                        AVal1 = AVal(IndWrk(IndIA))
                    END IF
                END DO
            END DO
            !  THE CS BECOME AS AND BS
            LmtNA = 2 * LmtNA
        END DO

        RETURN

    END SUBROUTINE Merge_Rank

    !******************************************************************

END SUBROUTINE Merge_Rank_OrderPack

!**********************************************************************

MODULE SUBROUTINE Merge_Rank_HalfCopy(AVal, AInd)

!** PURPOSE OF THIS SUBROUTINE
    ! To determine indices such that they sort the specified array into the desired
    ! order using the half-copy merge sort algorithm [].

!** REFERENCES
!   [1] <a href="http://kicia.ift.uni.wroc.pl/algorytmy/mergesortpaper.pdf">
!       Juszczak, C. 2007.  Fast mergesort implementation based on half-copying
!       merge algorithm. </a> <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: AVal(:)          ! array values
    tIndex,    INTENT(INOUT)    :: AInd(SIZE(AVal)) ! array indices

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: Insertion_CutOff_Low  = 25   ! cutoff to pair insertion sort
    tInteger, PARAMETER :: Insertion_CutOff_High = 55   ! cutoff to pair insertion sort

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: N2           ! size of the buffer array
    tIndex      :: CutOff       ! cutoff to pair insertion sort
    tIndex      :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(AVal, KIND=kIndex)

    ! initialize the indices
    CALL Index_Init(NA, AInd)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Insertion_CutOff_Low) THEN
        CALL Insert_Guarded(AVal, AInd, 1_kIndex, NA)
        RETURN
    END IF

    ! set cutoff depending on the size of the input array
    IF (NA < 2000_kIndex) THEN
        CutOff = Insertion_CutOff_Low
    ELSE
        CutOff = Insertion_CutOff_High
    END IF

    ! allocate buffer
    N2 = NA - NA/2_kIndex
    BLOCK
        tIndex  :: BInd(N2)
        ! perform merge sort
        CALL Main_Split_Merging(AVal, AInd, 0_kIndex, NA, BInd, CutOff)
    END BLOCK

    RETURN

CONTAINS

    SUBROUTINE Main_Split_Merging(AVal, AInd, P, K, BInd, CutOff)

    !** PURPOSE OF THIS SUBROUTINE
        ! For small arrays, the pair-insertion sort is used as it is the
        ! fastest sorting algorithm for small N. This also ensures that
        ! the merge is never called with empty regions [P1,K1) or [P2,K2).
        ! The routine calls the 'HalfCopy_Split_Merge' routine. It sorts the
        ! input range [P,K) 'in place' but requires a buffer to temporarily
        ! store N/2 array elements.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)  ! array values
        tIndex,      INTENT(INOUT)  :: AInd(0:) ! array indices
        tIndex,      INTENT(IN)     :: P        ! the start (inclusive) of the range of array to be sorted
        tIndex,      INTENT(IN)     :: K        ! the end (exclusive) of the range of array to be sorted
        tIndex,      INTENT(INOUT)  :: BInd(0:) ! array indices
        tIndex,      INTENT(IN)     :: CutOff   ! cutoff to pair insertion sort

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: NE, S, T, KMS

    !** FLOW:

        NE = K - P
        IF (NE > CutOff) THEN
            ! split the array into halves.
            S = P + NE/2
            T = K-(S-P)
            ! CopyMerge(A2, B)
            CALL HalfCopy_Split_Merge(AVal, AInd, S, K, BInd, 0, CutOff)
            ! CopyMerge(A1, A2)
            CALL HalfCopy_Split_Merge(AVal, AInd, P, S, AInd, T, CutOff)

            KMS = K - S
            IF (COMPARE_GLE(AVal(BInd(KMS-1)), AVal(AInd(T)))) THEN
                ! just copy array B to the first half of array A
                AInd(T-KMS:T-1) = BInd(0:KMS-1)
            ELSE
                ! merge the resulting sub-arrays
                ! MergeR(B, A2, A)
                CALL MergingR_HalfCopy(AVal, BInd, 0, KMS, AInd, T, K)
            END IF
        ELSE
            CALL Insert_Guarded(AVal, AInd, P+1, K)
        END IF

        RETURN

    END SUBROUTINE Main_Split_Merging

    !******************************************************************

    RECURSIVE SUBROUTINE HalfCopy_Split_Merge(AVal, AInd, P, K, BInd, T, CutOff)

    !** PURPOSE OF THIS SUBROUTINE
        ! Based on the half-copying algorithm, a very efficient copying version
        ! of mergesort may be written sorting the elements form the range [P,K)
        ! and placing the result in the range [T,T+(K-P)). The right half of the
        ! input is sorted (by using recursively the same algorithm) into the right
        ! half of the output range. Then, the left half of the input is range
        ! sorted (also recursively) into the right half of the input range. Then,
        ! it is merged with our half copying merge with the numbers already present
        ! in the output.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,   INTENT(IN)     :: AVal(:)  ! array values
        tIndex,      INTENT(INOUT)  :: AInd(0:) ! array indices
        tIndex,      INTENT(IN)     :: P        ! the start (inclusive) of the range of array to be sorted
        tIndex,      INTENT(IN)     :: K        ! the end (exclusive) of the range of array to be sorted
        tIndex,      INTENT(INOUT)  :: BInd(0:) ! array indices
        tIndex,      INTENT(IN)     :: T        ! the start (inclusive) of the range of array to be sorted
        tIndex,      INTENT(IN)     :: CutOff   ! cutoff to pair insertion sort

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: NE, S, S1, T1

    !** FLOW:

        NE = K - P
        IF (NE > CutOff) THEN
            ! split the array into halves.
            S = P + (K-P)/2
            ! CopyMerge(A2, B2)
            CALL HalfCopy_Split_Merge(AVal, AInd, S, K, BInd, T+(S-P), CutOff)
            ! CopyMerge(A1, B2)
            CALL HalfCopy_Split_Merge(AVal, AInd, P, S, AInd, S,       CutOff)

            S1 = S + (S-P)
            T1 = T+(S-P)
            IF (COMPARE_GLE(AVal(AInd(S1-1)), AVal(BInd(T1)))) THEN
                ! just copy array A to the first half of array B
                BInd(T1-(S1-S):T1-1) = AInd(S:S1-1)
            ELSE
                ! merge the resulting sub-arrays
                ! Merge(A2, B2, B)
                CALL Merging_HalfCopy(AVal, AInd, S, S1, BInd, T1, T+(K-P))
            END IF
        ELSE
            ! copy data from A to B
            BInd(T:T+NE-1) = AInd(P:K-1)

            ! perform pair-insertion sort on B
            CALL Insert_Guarded(AVal, BInd, T+1, T+NE)
        END IF

        RETURN

    END SUBROUTINE HalfCopy_Split_Merge

    !******************************************************************

END SUBROUTINE Merge_Rank_HalfCopy

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   MERGE ROUTINES
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE Merging_HalfCopy(AVal, AInd, S1, K1, BInd, S2, K2)

!** PURPOSE OF THIS SUBROUTINE
    ! To merge two sub-arrays in the [P1,K1) and [P2,K2) sorted ranges.
    ! The pointer K2 coincides with the end of the target range [P,K)
    ! meaning that no element in [P2,K2) needs to be moved if all
    ! elements from [S1,K1) are smaller than B(P2). It is assumed that
    ! both ranges are non empty. When the range [P1,K1) becomes exhausted
    ! the merging is done because all remaining elements from [P2,K2) are
    ! already in place.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument,   INTENT(IN)     :: AVal(:)  ! array values
    tIndex,      INTENT(INOUT)  :: AInd(0:) ! array indices
    tIndex,      INTENT(IN)     :: S1       ! the start of the first sub-array (inclusive)
    tIndex,      INTENT(IN)     :: K1       ! the end of the first sub-array (exclusive)
    tIndex,      INTENT(INOUT)  :: BInd(0:) ! array indices
    tIndex,      INTENT(IN)     :: S2       ! the start of the second sub-array (inclusive)
    tIndex,      INTENT(IN)     :: K2       ! the end of the second sub-array (exclusive)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: P1, P2, P

!** FLOW:

    ! initialize
    P1 = S1
    P2 = S2

    ! calculate the beginning of the output
    P = P2 - (K1-P1)

    DO
        IF (COMPARE_GLE(AVal(AInd(P1)), AVal(BInd(P2)))) THEN
            BInd(P) = AInd(P1)
            P = P + 1
            P1 = P1 + 1
            IF (P1 == K1) RETURN
        ELSE
            BInd(P) = BInd(P2)
            P = P + 1
            P2 = P2 + 1
            IF (P2 == K2) EXIT
        END IF
    END DO
    DO
        BInd(P) = AInd(P1)
        P = P + 1
        P1 = P1 + 1
        IF (P1 == K1) EXIT
    END DO

    RETURN

END SUBROUTINE Merging_HalfCopy

!**********************************************************************

SUBROUTINE MergingR_HalfCopy(AVal, BInd, S1, K1, AInd, S2, K2)

!** PURPOSE OF THIS SUBROUTINE
    ! See purpose of the routine in 'Merging_HalfCopy' subroutine.
    ! To preserve the stability of MergeSort algorithm, this
    ! routine is a slightly changed version of Merging_HalfCopy routine
    ! It assumes that the range [P2,K2) was to the left of [K1,P1)
    ! in the input and takes this into account when the compared
    ! elements turn out to be equal (those from [P2,K2) are first
    ! copied to the output).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument,   INTENT(IN)     :: AVal(:)  ! array values
    tIndex,      INTENT(INOUT)  :: BInd(0:) ! array indices
    tIndex,      INTENT(IN)     :: S1       ! the start of the first sub-array (inclusive)
    tIndex,      INTENT(IN)     :: K1       ! the end of the first sub-array (exclusive)
    tIndex,      INTENT(INOUT)  :: AInd(0:) ! array indices
    tIndex,      INTENT(IN)     :: S2       ! the start of the second sub-array (inclusive)
    tIndex,      INTENT(IN)     :: K2       ! the end of the second sub-array (exclusive)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: P1, P2, P

!** FLOW:

    ! initialize
    P1 = S1
    P2 = S2

    ! calculate the beginning of the output
    P = P2 - (K1-P1)
    DO
        IF (COMPARE_GLT(AVal(BInd(P1)), AVal(AInd(P2)))) THEN
            AInd(P) = BInd(P1)
            P = P + 1
            P1 = P1 + 1
            IF (P1 == K1) RETURN
        ELSE
            AInd(P) = AInd(P2)
            P = P + 1
            P2 = P2 + 1
            IF (P2 == K2) EXIT
        END IF
    END DO
    DO
        AInd(P) = BInd(P1)
        P = P + 1
        P1 = P1 + 1
        IF (P1 == K1) EXIT
    END DO

    RETURN

END SUBROUTINE MergingR_HalfCopy

!**********************************************************************

SUBROUTINE Merging_Wiki(AVal, AInd, Left, Mid, Right, BInd)

!** PURPOSE OF THIS SUBROUTINE
    ! To merge array where
    !   left source half is  A(Left:Mid-1),
    !   right source half is A(Mid+1:Right-1), and
    !   result is            B(Left:Right-1).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument,   INTENT(IN)     :: AVal(:)  ! array values
    tIndex,      INTENT(IN)     :: AInd(0:) ! source array indices
    tIndex,      INTENT(IN)     :: Left     ! the start of the left half
    tIndex,      INTENT(IN)     :: Mid      ! the end of the left half
    tIndex,      INTENT(IN)     :: Right    ! the end of the right half
    tIndex,      INTENT(INOUT)  :: BInd(0:) ! destination array indices

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: I, J, K

!** FLOW:

    ! initialize
    I = Left
    J = Mid

    ! While there are elements in the left or right runs
    K = Left
    DO WHILE (K < Right)
        ! If left run head exists and is <= existing right run head.
        IF ((I < Mid).AND.((J >= Right).OR.(COMPARE_GLE(AVal(AInd(I)), AVal(AInd(J)))))) THEN
            BInd(K) = AInd(I)
            I = I + 1
        ELSE
            BInd(K) = AInd(J)
            J = J + 1
        END IF
        K = K + 1
    END DO

    RETURN

END SUBROUTINE Merging_Wiki

!**********************************************************************

#undef MaximumDepth
#undef tArgument
#undef tVarScalar
#undef COMPARE_GLT
#undef COMPARE_GLE
#undef Is_Array_Ranked
#undef Wise_Rank_Unstable
#undef Wise_Rank_Stable
#undef Intro_Rank
#undef Java_Rank
#undef PDQ_Rank
#undef Tim_Rank
#undef Rust_Rank
#undef Quick_Rank_Hoare
#undef Quick_Rank_Lomuto
#undef Quick_Rank_Mo3
#undef Quick_Rank_3Way
#undef Quick_Rank_Vowels
#undef Quick_Rank_Stable
#undef Quick_Rank_Iterative
#undef Quick_Rank_Java
#undef Merge_Rank_TopDown
#undef Merge_Rank_BottomUp
#undef Merge_Rank_QuadSplit
#undef Merge_Rank_HalfCopy
#undef Merge_Rank_OrderPack

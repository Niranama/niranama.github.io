
!** SUBMODULE PARAMETERS:
    ! general parameters for quick sort
    tInteger, PARAMETER :: Quick_Insert_CutOff_Low  = 40    ! cutoff to pair insertion
    tInteger, PARAMETER :: Quick_Insert_CutOff_High = 70    ! cutoff to pair insertion

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !> *Partition* is an interface for a partitioning procedure
        !  used by a quick-sort routine.
        SUBROUTINE Partition(A,LStart,REnd,LEnd,RStart)
            IMPORT
            tArgument, INTENT(INOUT)    :: A(:)     !! an array to be partitioned
            tIndex,    INTENT(IN)       :: LStart   !! starting position of the left pointer
            tIndex,    INTENT(IN)       :: REnd     !! ending position of the right pointer
            tIndex,    INTENT(OUT)      :: LEnd     !! ending position of the left pointer
            tIndex,    INTENT(OUT)      :: RStart   !! starting position of the right pointer
        END SUBROUTINE Partition
    END INTERFACE

!** SUBMODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** SUBMODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!------------------------------------------------------------------------------
!                       COMMON AUXILIARY ROUTINES
!------------------------------------------------------------------------------

MODULE FUNCTION Is_Array_Sorted(A) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE
    !^ To check whether the given array is sorted in the desired order.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)   :: A(:)     !! array values to be checked
    tLogical                :: Flag     !! true if the array is sorted

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, NA

!** FLOW:

    ! get array size
    NA = SIZE(A, KIND=kIndex)

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
        IF (COMPARE_GLT(A(I+1), A(I))) RETURN
    END DO

    ! the array elements are in order
    Flag = TrueVal

    RETURN

END FUNCTION Is_Array_Sorted

!******************************************************************************

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

SUBROUTINE Sort_3_Items(A, I, J, K)

!** PURPOSE OF THIS SUBROUTINE
    !^ To sort three elements A(I), A(J), A(K) where I, J, K are
    !   one-based indices so that <br>
    !   1. for a desired order,  A(I) <= A(J) <= A(K) <br>
    !   2. for an opposite order, A(I) >= A(J) >= A(K) <br>
    ! Note: comments in the routine are for a desired order.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(:)
    tIndex,    INTENT(IN)       :: I, J, K

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tVarScalar  :: Temp

!** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
    ALLOCATE(Temp, MOLD=A(I))
#endif

    IF (COMPARE_GLT(A(K), A(I))) THEN
        ! element K is smaller than element I
        IF (COMPARE_GLT(A(K), A(J))) THEN
            ! element K is the smallest one
            IF (COMPARE_GLT(A(J), A(I))) THEN
                ! element I is the largest one and element J is the middle one
                EXCHANGE(A,I,K)
            ELSE
                ! element J is the largest one and element I is the middle one
                Temp = A(J)
                A(J) = A(I)
                A(I) = A(K)
                A(K) = Temp
            END IF
        ELSE
            ! element K is the middle one (and element J is the smallest one
            !   and element I is the largest one)
            ! no need for further check
            Temp = A(I)
            A(I) = A(J)
            A(J) = A(K)
            A(K) = Temp
        END IF
    ELSE
        ! element K is greater than element I
        IF (COMPARE_GLT(A(K), A(J))) THEN
            ! element K is the middle one (and element J is the largest one
            ! and element I is the smallest one)
            ! no need for further check
            EXCHANGE(A,J,K)
        ELSE
            ! element K is the largest one
            IF (COMPARE_GLT(A(J), A(I))) THEN
                ! element J is the smallest one and element I is the middle one
                EXCHANGE(A,I,J)
            END IF
            ! Note: for A(J) > A(I), the given array is already in order
            ! so there is no need to do anything.
        END IF
    END IF

    RETURN

END SUBROUTINE Sort_3_Items

!******************************************************************************

SUBROUTINE Sort_3_Items_Base0(A, I, J, K)

!** PURPOSE OF THIS SUBROUTINE
    !^ To sort three elements A(I), A(J), A(K) where I, J, K are
    !   zero-based indices so that <br>
    !   1. for a desired order,  A(I) <= A(J) <= A(K) <br>
    !   2. for an opposite order, A(I) >= A(J) >= A(K) <br>
    ! Note: comments in the routine are for a desired order.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(0:)
    tIndex,    INTENT(IN)       :: I, J, K

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tVarScalar  :: Temp

!** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
    ALLOCATE(Temp, MOLD=A(I))
#endif

    IF (COMPARE_GLT(A(K), A(I))) THEN
        ! element K is smaller than element I
        IF (COMPARE_GLT(A(K), A(J))) THEN
            ! element K is the smallest one
            IF (COMPARE_GLT(A(J), A(I))) THEN
                ! element I is the largest one and element J is the middle one
                EXCHANGE(A,I,K)
            ELSE
                ! element J is the largest one and element I is the middle one
                Temp = A(J)
                A(J) = A(I)
                A(I) = A(K)
                A(K) = Temp
            END IF
        ELSE
            ! element K is the middle one (and element J is the smallest one
            !   and element I is the largest one)
            ! no need for further check
            Temp = A(I)
            A(I) = A(J)
            A(J) = A(K)
            A(K) = Temp
        END IF
    ELSE
        ! element K is greater than element I
        IF (COMPARE_GLT(A(K), A(J))) THEN
            ! element K is the middle one (and element J is the largest one
            ! and element I is the smallest one)
            ! no need for further check
            EXCHANGE(A,J,K)
        ELSE
            ! element K is the largest one
            IF (COMPARE_GLT(A(J), A(I))) THEN
                ! element J is the smallest one and element I is the middle one
                EXCHANGE(A,I,J)
            END IF
            ! Note: for A(J) > A(I), the given array is already in order
            ! so there is no need to do anything.
        END IF
    END IF

    RETURN

END SUBROUTINE Sort_3_Items_Base0

!******************************************************************************

FUNCTION Median_Of_Three(A, I, J, K) RESULT(M)

!** PURPOSE OF THIS SUBROUTINE
    !^ To return the (one-based) index of the median element among A(I), A(J), and A(K).

!** REFERENCES
    ! "Quick.java" source code from "https://algs4.cs.princeton.edu/23Quicksort"
    ! of "Algorithms, 4th Edition" by Robert Sedgewick and Kevin Wayne.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)   :: A(:)
    tIndex,    INTENT(IN)   :: I, J, K
    tIndex                  :: M

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    IF (COMPARE_GLT(A(I), A(J))) THEN
        IF (COMPARE_GLT(A(J), A(K))) THEN
            M = J
        ELSE
            IF (COMPARE_GLT(A(I), A(K))) THEN
                M = K
            ELSE
                M = I
            END IF
        END IF
    ELSE
        IF (COMPARE_GLT(A(K), A(J))) THEN
            M = J
        ELSE
            IF (COMPARE_GLT(A(K), A(I))) THEN
                M = K
            ELSE
                M = I
            END IF
        END IF
    END IF

    RETURN

END FUNCTION Median_Of_Three

!******************************************************************************

SUBROUTINE DualInsert_Guarded(A, Lo, Hi)

!** PURPOSE OF THIS SUBROUTINE
	!^ To sort array using the *pair-insertion* sort algorithm.

!** REFERENCES
    ! Adapted from a piece of code from Java's JDK7.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(:)     !! array to be sorted
    tIndex,    INTENT(IN)       :: Lo       !! starting (one-based) index (inclusive)
    tIndex,    INTENT(IN)       :: Hi       !! ending (one-based) index (inclusive)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tVarScalar  :: Pivot1, Pivot2, Last
    tIndex      :: LPtr, RPtr, Indx

!** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
    ALLOCATE(Pivot1, MOLD=A(Lo))
    ALLOCATE(Pivot2, MOLD=A(Lo))
    ALLOCATE(Last, MOLD=A(Lo))
#endif

    ! initialize
    LPtr = Lo
    RPtr = Hi

    ! Skip the longest (ascending/descending) sequence
    DO
        IF (LPtr >= RPtr) RETURN
        LPtr = LPtr + 1_kIndex
        IF (COMPARE_GLT(A(LPtr), A(LPtr-1))) EXIT
    END DO

    ! perform optimized dual-insertion sort
    Indx = LPtr
    LPtr = LPtr + 1_kIndex
    DO WHILE (LPtr <= RPtr)

        ! set pivots used for insertions where Pivot1 > Pivot2
        IF (COMPARE_GLT(A(Indx), A(LPtr))) THEN
            Pivot1 = A(LPtr)
            Pivot2 = A(Indx)
        ELSE
            Pivot1 = A(Indx)
            Pivot2 = A(LPtr)
        END IF

        ! K is the index used to find an insertion point
        Indx = Indx - 1_kIndex

        ! find an insertion point for Pivot1 and shift existing
        ! content by 2
        DO WHILE ((Indx >= Lo).AND.(COMPARE_GLT(Pivot1, A(Indx))))
            A(Indx+2) = A(Indx)
            Indx = Indx - 1_kIndex
        END DO

        ! store Pivot1 at its insertion place
        A(Indx+2) = Pivot1

        ! A(K+1) is an available space now so  find an insertion point
        ! for Pivot2 and shift existing content by 1
        DO WHILE ((Indx >= Lo).AND.(COMPARE_GLT(Pivot2, A(Indx))))
            A(Indx+1) = A(Indx)
            Indx = Indx - 1_kIndex
        END DO

        ! store Pivot2 at its insertion place
        A(Indx+1) = Pivot2

        ! update indices
        Indx = LPtr + 1_kIndex
        LPtr = Indx + 1_kIndex
    END DO

    ! perform single insertion for the last element
    Last = A(RPtr)
    RPtr = RPtr - 1_kIndex
    DO WHILE ((RPtr >= Lo).AND.(COMPARE_GLT(Last, A(RPtr))))
        A(RPtr+1) = A(RPtr)
        RPtr = RPtr - 1_kIndex
    END DO
    A(RPtr+1) = Last

    RETURN

END SUBROUTINE DualInsert_Guarded

!******************************************************************************

SUBROUTINE DualInsert_UnGuarded(A, Lo, Hi)

!** PURPOSE OF THIS SUBROUTINE
	!^ To sort array using *pair-insertion* sort algorithm without checking
    !  whether the array index is out of bound or not.

!** REFERENCES
    ! Adapted from a piece of code from Java's JDK7.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(:)     !! array to be sorted
    tIndex,    INTENT(IN)       :: Lo       !! starting (one-based) index (inclusive)
    tIndex,    INTENT(IN)       :: Hi       !! ending (one-based) index (inclusive)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tVarScalar  :: Pivot1, Pivot2, Last
    tIndex      :: LPtr, RPtr, Indx

!** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
    ALLOCATE(Pivot1, MOLD=A(Lo))
    ALLOCATE(Pivot2, MOLD=A(Lo))
    ALLOCATE(Last, MOLD=A(Lo))
#endif

    ! initialize
    LPtr = Lo
    RPtr = Hi

    ! Skip the longest (ascending/descending) sequence
    DO
        IF (LPtr >= RPtr) RETURN
        LPtr = LPtr + 1_kIndex
        IF (COMPARE_GLT(A(LPtr), A(LPtr-1))) EXIT
    END DO

    ! perform optimized dual-insertion sort
    Indx = LPtr
    LPtr = LPtr + 1_kIndex
    DO WHILE (LPtr <= RPtr)

        ! set pivots used for insertions where Pivot1 > Pivot2
        IF (COMPARE_GLT(A(Indx), A(LPtr))) THEN
            Pivot1 = A(LPtr)
            Pivot2 = A(Indx)
        ELSE
            Pivot1 = A(Indx)
            Pivot2 = A(LPtr)
        END IF

        ! K is the index used to find an insertion point
        Indx = Indx - 1_kIndex

        ! find an insertion point for Pivot1 and shift existing
        ! content by 2
        DO WHILE (COMPARE_GLT(Pivot1, A(Indx)))
            A(Indx+2) = A(Indx)
            Indx = Indx - 1_kIndex
        END DO

        ! store Pivot1 at its insertion place
        A(Indx+2) = Pivot1

        ! A(K+1) is an available space now so  find an insertion point
        ! for Pivot2 and shift existing content by 1
        DO WHILE (COMPARE_GLT(Pivot2, A(Indx)))
            A(Indx+1) = A(Indx)
            Indx = Indx - 1_kIndex
        END DO

        ! store Pivot2 at its insertion place
        A(Indx+1) = Pivot2

        ! update indices
        Indx = LPtr + 1_kIndex
        LPtr = Indx + 1_kIndex
    END DO

    ! perform single insertion for the last element
    Last = A(RPtr)
    RPtr = RPtr - 1_kIndex
    DO WHILE (COMPARE_GLT(Last, A(RPtr)))
        A(RPtr+1) = A(RPtr)
        RPtr = RPtr - 1_kIndex
    END DO
    A(RPtr+1) = Last

    RETURN

END SUBROUTINE DualInsert_UnGuarded

!******************************************************************************

SUBROUTINE Reverse_Order_Base0(A, IStart, IEnd)

!** PURPOSE OF THIS SUBROUTINE
    !^ To reverse order of a segment of an array in place.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(0:)    !! array to be reverse-ordered
    tIndex,    INTENT(IN)       :: IStart   !! starting (zero-based) index (inclusive)
    tIndex,    INTENT(IN)       :: IEnd     !! ending (zero-based) index (inclusive)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tVarScalar  :: Temp
    tIndex      :: Lo, Hi

!** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
    ALLOCATE(Temp, MOLD=A(IStart))
#endif

    Lo = IStart
    Hi = IEnd
    DO WHILE (Lo < Hi)
        EXCHANGE(A, Lo, Hi)
        Lo = Lo + 1_kIndex
        Hi = Hi - 1_kIndex
    END DO

    RETURN

END SUBROUTINE Reverse_Order_Base0

!******************************************************************************

SUBROUTINE Insert_Guarded(A, Lo, Hi)

!** PURPOSE OF THIS SUBROUTINE
	!^ To sort array using *insertion* sort algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(:) !! array to be sorted
    tIndex,    INTENT(IN)       :: Lo   !! starting (one-based) index (inclusive)
    tIndex,    INTENT(IN)       :: Hi   !! ending (one-based) index (inclusive)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tVarScalar  :: Temp
    tIndex      :: I, J, Nxch

!** FLOW:

    ! +++ Sedgewick's Optimized Version +++

#ifdef TYPE_IS_IN_Comparable_CLASS
    ALLOCATE(Temp, MOLD=A(Lo))
#endif

    ! initialize
    Nxch = 0_kIndex

    ! put smallest element in position to serve as sentinel
    DO I = Hi, Lo+1_kIndex, -1_kIndex
        J = I - 1_kIndex
        IF (COMPARE_GLT(A(I), A(J))) THEN
            EXCHANGE(A, I, J)
            Nxch = Nxch + 1_kIndex
        END IF
    END DO
    IF (Nxch == 0_kIndex) RETURN

    ! insertion sort with half exchanges
    DO I = Lo+2_kIndex, Hi
        Temp = A(I)
        J = I - 1_kIndex
        DO WHILE (COMPARE_GLT(Temp, A(J)))
            A(J+1)  = A(J)
            J = J - 1_kIndex
        END DO
        A(J+1) = Temp
    END DO

    RETURN

END SUBROUTINE Insert_Guarded

!******************************************************************************

SUBROUTINE Insert_UnGuarded(A, Lo, Hi)

!** PURPOSE OF THIS SUBROUTINE
	!^ To sort array using *insertion* sort algorithm without checking
    !  whether the array index is out of bound or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(:) !! array to be sorted
    tIndex,    INTENT(IN)       :: Lo   !! starting (one-based) index (inclusive)
    tIndex,    INTENT(IN)       :: Hi   !! ending (one-based) index (inclusive)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tVarScalar  :: Temp
    tIndex      :: I, J

!** FLOW:

    ! +++ Rosetta Version +++

#ifdef TYPE_IS_IN_Comparable_CLASS
    ALLOCATE(Temp, MOLD=A(Lo))
#endif

    DO I = Lo+1_kIndex, Hi
        J = I - 1_kIndex
        Temp = A(I)
        DO WHILE (COMPARE_GLT(Temp, A(J)))
            A(J+1)  = A(J)
            J = J - 1_kIndex
        END DO
        A(J+1) = Temp
    END DO

    RETURN

END SUBROUTINE Insert_UnGuarded

!------------------------------------------------------------------------------
!                       WISE SORT ROUTINES
!------------------------------------------------------------------------------

MODULE SUBROUTINE Wise_Sort_Unstable(A)

!^ **PURPOSE OF THIS SUBROUTINE**: <br>
!   To sort an array in a desired order using the *WiseSort* algorithm, which
!   is a hybrid algorithm that employs various sorting algorithms including
!   the *pair-insertion sort*, *mergesort* and *quicksort* algorithms. <br>
!    <br>
!  **TECHNICAL INFORMATION**: <br>
!   The *WiseSort* algorithm is a hybrid algorithm originated in this library
!   (*XpfLib*).  The algorithm sorts the given array using a variety of sorting
!   algorithms based on the pattern of the data.  First, it carefully and wisely
!   inspects the specified array by checking if there is any pattern in the given
!   array.  Next, it decides which sorting algorithm is most suitable for the array.
!   It then sorts the array using the chosen algorithm. <br>
!   The selection of a sorting algorithm is based on the following facts. <br>
!   - An *Insertion sort* algorithm usually performs well for an array with
!     small size. <br>
!   - If the data is already sorted, no sorting algorithm is needed. <br>
!   - If the data is already sorted but in an order opposite to the desired one,
!     a reversion of the array is the most optimal sorting algorithm. <br>
!   - A quicksort algorithm typically performs well for a totally randomized array. <br>
!   - A mergesort algorithm generally performs well for a mostly ordered array. <br>
!   Based on the aforementioned facts, the *WiseSort* algorithm utilizes various
!   sorting algorithms including: <br>
!   1. *Pair-insertion-sort-based* algorithms consisting of <br>
!    - guarded version for a left-most sub-array or the whole array with small size, <br>
!    - unguarded version for a non-left-most sub-array with small size. <br>
!   2. *Merge-sort-based* algorithms consisting of <br>
!    - Java's merging runs for a highly structured array (an array with a certain
!      recognized pattern, e.g. saw- or wave-like pattern), <br>
!    - Rust's mergesort (or the so-called simplified TimSort) for a mostly ordered
!      array (i.e. an array having a very long ordered run, either in ascending or
!      descending order). <br>
!      For this particular implementation, it is found that Rust's mergesort performs
!      very well for an array when more than 50% of the first or last elements of the
!      array are already sorted. <br>
!   3. *Quick-sort-based* algorithms where various partitioning schemes are employed
!      including: <br>
!    - Hoare's partitioning scheme for a NOT highly structured array, i.e. a
!      partly-ordered and partly-randomized array (an array having not very long
!      and not very short ordered runs), <br>
!    - Median-of-three partitioning scheme for a totally-randomized array
!      (an array having VERY SHORT ordered runs, e.g. an ordered run with less than
!      4 elements), <br>
!    - Ninther (median of medians or pseudo-median of nine) partitioning scheme for
!      worst-case situations (when the quicksort algorithm converges too slowly, it
!      switches from Hoare's or median-of-three partitioning scheme to the ninther
!      partitioning scheme), <br>
!    - Three-way (or Dutch's national flag) partitioning scheme for an array with
!      many equal elements (when the quick sort algorithm detects that many elements
!      are equal, it switches from Hoare's or median-of-three partitioning scheme to
!      the three-way partitioning scheme). <br>
!   As previously mentioned, the selection of an appropriate algorithm is based on
!   a careful and wise inspection of the given array.  The routine responsible for
!   this task will try to predict a suitable algorithm for any given array according
!   to its pattern found.  The following list provides an overview of the routine. <br>
!   - The routine will mostly try to scan just a very few runs and quit very quickly.
!     Only, the highly structured array (the one sorted by Java's merge runs) is
!     entirely scanned since all the runs are needed by the selected algorithm. <br>
!   - If the routine detects that the current run is in an order opposite to the
!     desired one, it will only reverse the run if necessary (i.e. the chosen
!     algorithm will benefit from the reversion of the run). <br>
!   - The prediction of a proper algorithm is quite accurate for most known cases.
!     However, there are certain known cases that the routine may not choose
!     the *BEST* algorithm.  For example, if highly-order parts of the given array
!     are in the middle while randomized parts are in the beginning and in the end,
!     the routine will typically select a quick sort algorithm.  However, if the
!     highly-ordered parts are long enough (e.g. more than 50% of the array size),
!     Rust/TimSort algorithms might be better algorithms.  For these certain cases,
!     nonetheless, it is not worth scanning the whole array just so we can choose
!     the *BEST* algorithm because the overhead will surely be very expensive. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(:) !! array to be sorted

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! parameters for Algo_QuickSort partitioning scheme
    tInteger, PARAMETER :: Mo3Scheme   = 1   ! use median-of-three partitioning scheme
    tInteger, PARAMETER :: HoareScheme = 2   ! use Hoare's partitioning scheme
    ! parameters for sorting algorithm
    tInteger, PARAMETER :: Algo_None      = 0    ! no algorithm needed (data already sorted)
    tInteger, PARAMETER :: Algo_MergeRun  = 1    ! Java's merging run (for highly structure data, i.e. Saw)
    tInteger, PARAMETER :: Algo_RustSort  = 2    ! Tim sort (for long first run - more than half)
    tInteger, PARAMETER :: Algo_QuickSort = 3    ! Quick sort (for hardly structure data)
    ! parameters for cutoffs
    tIndex,   PARAMETER :: Insertion_CutOff_Low  = 40_kIndex   ! cutoff to pair insertion sort
    tIndex,   PARAMETER :: Insertion_CutOff_Mid  = 50_kIndex   ! cutoff to pair insertion sort
    tIndex,   PARAMETER :: Insertion_CutOff_High = 70_kIndex   ! cutoff to pair insertion sort

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex, ALLOCATABLE :: Run(:)       ! info for Java's merge runs
    tIndex              :: RunCount     ! info for Java's merge runs
    tIndex              :: Dummy        ! dummy variable
    tIndex              :: QPScheme     ! quick-sort partitioning scheme
    tIndex              :: LongRunSize  ! long run size used by TimSort
    tLogical            :: FirstRun     ! long run size used by TimSort
    tIndex              :: CutOff       ! cutoff to pair insertion sort
    tIndex              :: MaxDepth     ! maximum depth used to switch partitioning routine
    tIndex              :: AlgoFlag     ! flag for sorting algorithm
    tIndex              :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(A, KIND=kIndex)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether the array is small
    IF (NA <= Insertion_CutOff_Low) THEN
        ! for small array, use pair insertion sort algorithm
        CALL DualInsert_Guarded(A, 1, NA)
        RETURN
    END IF

    ! inspect runs to determine sorting algorithm to be used
    CALL Expert_Inspection(A, AlgoFlag, QPScheme, LongRunSize, FirstRun, Run, RunCount)

    ! perform sorting
    SELECT CASE (AlgoFlag)
    CASE (Algo_None)
        ! do nothing since the array is already sorted
    CASE (Algo_MergeRun)
        BLOCK
#ifdef TYPE_IS_IN_Comparable_CLASS
            tVarAlloc   :: Buffer(:)
#else
            tVarLocal   :: Buffer(NA)
#endif
#ifdef TYPE_IS_IN_Comparable_CLASS
            ! allocate a buffer to use as scratch memory.
            ALLOCATE(Buffer(NA), MOLD=A)
#endif
            ! perform Java's merge runs
            Dummy = Java_Merge_Runs(A, Buffer, 0_kIndex, 1_kIndex, Run, 0_kIndex, RunCount)
        END BLOCK
    CASE (Algo_RustSort)
        BLOCK
#ifdef TYPE_IS_IN_Comparable_CLASS
            tVarAlloc   :: Buffer(:)
#else
            tVarLocal   :: Buffer(NA/2)
#endif
#ifdef TYPE_IS_IN_Comparable_CLASS
            ! allocate a buffer to use as scratch memory.
            ALLOCATE(Buffer(NA/2), MOLD=A)
#endif
            ! perform Rust's merge sort algorithm
            CALL Rust_Merge_Sort(A, NA, LongRunSize, FirstRun, Buffer)
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
        CASE (Mo3Scheme)
            CALL Quick_Mo3(A, NA, 1_kIndex, NA, MaxDepth)
        CASE (HoareScheme)
            CALL Quick_Hoare(A, NA, 1_kIndex, NA, MaxDepth)
        END SELECT
    END SELECT

    RETURN

CONTAINS

    SUBROUTINE Expert_Inspection(A, AlgoFlag, QPScheme, LongRunSize, FirstRun, Run, Count)

    !** PURPOSE OF THIS SUBROUTINE
        ! To carefully inspect whether there is any pattern in the given array
        ! and determine wisely an appropriate sorting algorithm for the array.

    !** TECHNICAL INFORMATION
        ! - The routine will try to predict a suitable algorithm for any given array according
        !   to the facts given in the technical information section in the main routine.
        ! - The routine will mostly try to scan just a very few runs and quit very quickly.
        !   Only, the highly structured array (the one sorted by Java's merge runs)
        !   is entirely scanned since all the runs are needed by the selected algorithm.
        ! - If the routine detects that the current run is in an order opposite to the
        !   desired one, it will only reverse the run if necessary (i.e. the chosen
        !   algorithm will benefit from the reversion of the run).
        ! - The prediction of a proper algorithm is quite accurate for most known cases.
        !   However, there are certain known cases that the routine may not choose
        !   the *BEST* algorithm.  For example, if highly-order parts of the given array
        !   are in the middle while randomized parts are in the beginning and in the end,
        !   the routine will typically select a quick sort algorithm.  However, if the
        !   highly-ordered parts are long enough (e.g. more than 50% of the array size),
        !   Rust/TimSort algorithms might be better algorithms.  For these certain cases,
        !   nonetheless, it is not worth scanning the whole array just so we can choose
        !   the *BEST* algorithm because the overhead will surely be very expensive.

    !** REFERENCE
        ! The routine is inspired and adapted from the "TryMergeRuns" function,
        ! which is a part of the "Java_Sort" subroutine where the "Java_Sort" routine
        ! is a translation of Java's sorting algorithm.
        ! OpenJDK's Java codes for Java's sorting algorithm can be obtained from
        ! https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/util/DualPivotQuicksort.java.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,           INTENT(INOUT)  :: A(0:)        ! array to be sorted
        tIndex,              INTENT(OUT)    :: AlgoFlag     ! flag indicating algorithm to be used
        tIndex,              INTENT(OUT)    :: QPScheme     ! flag indicating partitioning scheme
                                                            ! for quick sort algorithm
        tIndex,              INTENT(OUT)    :: LongRunSize  ! needed by Rust's TimSort algorithm
        tLogical,            INTENT(OUT)    :: FirstRun     ! needed by Rust's TimSort algorithm
        tIndex, ALLOCATABLE, INTENT(OUT)    :: Run(:)       ! needed by Java's merge-run algorithm
        tIndex,              INTENT(OUT)    :: Count        ! needed by Java's merge-run algorithm

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tInteger, PARAMETER :: MIN_FIRST_RUN_SIZE    = 16   ! Min size of the first run to continue with scanning
        tInteger, PARAMETER :: MIN_FIRST_RUNS_FACTOR = 7    ! Min factor for the first runs to continue scanning
        tInteger, PARAMETER :: MAX_RUN_CAPACITY      = 128  ! Max capacity of the index array for tracking runs
        tInteger, PARAMETER :: QUICK_SHORT_CUTOFF    = 256  ! Cutoff to quick sort algorithm
        tInteger, PARAMETER :: QUICK_SMALL_CUTOFF    = 1024 ! Cutoff to quick sort algorithm

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tVarScalar  :: AK, Temp
        tIndex      :: Low, ASize
        tIndex      :: RunSize, NewRunSize
        tIndex      :: I, J, K, II, JJ
        tIndex      :: High, Last
        tIndex      :: Start, Finish
        tIndex      :: MinFirstRun, LongRunLimit, LongRunLimit2
        tIndex      :: FirstRunSize, LastRunSize, CurrRunSize
        tLogical    :: CurrDescend, LastDescend

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(AK, MOLD=A(0))
        ALLOCATE(Temp, MOLD=A(0))
#endif

        ! Initialize working variables.
        Low = 0_kIndex
        ASize = SIZE(A)
        High = Low + ASize
        Count = 0_kIndex
        Last = Low
        LongRunSize = 0_kIndex
        FirstRun = TrueVal
        CurrDescend = FalseVal
        LastDescend = FalseVal
        QPScheme = Mo3Scheme
        ! these two expressions are determined experimentally.
        LongRunLimit = INT(0.50_kDouble*REAL(ASize, KIND=kDouble), KIND=kIndex)
        LongRunLimit2 = INT(0.55_kDouble*REAL(ASize, KIND=kDouble), KIND=kIndex)

        ! Determine minimum size of the first run to continue with scanning.
        MinFirstRun = MAX(ASize/MAX_RUN_CAPACITY, MIN_FIRST_RUN_SIZE)

        ! Identify all possible runs.
        K = Low + 1
        DO WHILE (K < High)

            ! Find the end index of the current run.
            IF (COMPARE_GLT(A(K-1), A(K))) THEN

                ! Identify ascending sequence.
                K = K + 1
                DO WHILE ((K < High).AND.(COMPARE_GLE(A(K-1), A(K))))
                    K = K + 1
                END DO
                CurrDescend = FalseVal

            ELSEIF (COMPARE_GLT(A(K), A(K-1))) THEN

                ! Identify descending sequence.
                K = K + 1
                DO WHILE ((K < High).AND.(COMPARE_GLE(A(K), A(K-1))))
                    K = K + 1
                END DO
                CurrDescend = TrueVal

            ELSE

                ! Identify constant sequence.
                AK = A(K)
                K = K + 1
                DO WHILE ((K < High).AND.(AK == A(K)))
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
                        ! Reverse into a desired order.
                        I = Last
                        J = K - 1
                        CALL Reverse_Order_Base0(A, I, J)
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
                        ! use quick sort algorithm with median-of-three scheme.
                        AlgoFlag = Algo_QuickSort
                        QPScheme = Mo3Scheme
                    ELSEIF (ASize <= QUICK_SHORT_CUTOFF) THEN
                        ! For a small array with the first run size more than 50% of ASize,
                        ! use quick sort algorithm with Hoare's scheme.
                        AlgoFlag = Algo_QuickSort
                        QPScheme = HoareScheme
                    ELSE
                        ! For a large (not so small) array with long first run,
                        ! use Rust/Tim sort.
                        AlgoFlag = Algo_RustSort
                        LongRunSize = FirstRunSize
                        FirstRun = TrueVal
                        IF (CurrDescend) THEN
                            ! Reverse into a desired order.
                            I = Last
                            J = K - 1
                            CALL Reverse_Order_Base0(A, I, J)
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
                    QPScheme = Mo3Scheme
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
                            IF (COMPARE_GLT(A(Start), A(Start+1))) THEN
                                ! Identify ascending sequence.
                                DO WHILE (Start > 0)
                                    IF (COMPARE_GLT(A(Start), A(Start-1))) EXIT
                                    Start = Start - 1
                                END DO
                                LastDescend = FalseVal
                            ELSEIF (COMPARE_GLT(A(Start+1), A(Start))) THEN
                                ! Identify descending sequence.
                                DO WHILE (Start > 0)
                                    IF (COMPARE_GLT(A(Start-1), A(Start))) EXIT
                                    Start = Start - 1
                                END DO
                                LastDescend = TrueVal
                            ELSE
                                ! Identify constant sequence.
                                AK = A(Start)
                                Start = Start - 1
                                DO WHILE ((Start > 0).AND.(AK == A(Start)))
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
                                ! use quick sort algorithm with median-of-three scheme.
                                AlgoFlag = Algo_QuickSort
                                QPScheme = Mo3Scheme
                            ELSEIF (ASize <= QUICK_SHORT_CUTOFF) THEN
                                ! For a small array with the last run size more than 50% of ASize,
                                ! use quick sort algorithm with Hoare's scheme.
                                AlgoFlag = Algo_QuickSort
                                QPScheme = HoareScheme
                            ELSE
                                ! For a large (not so small) array with long last run,
                                ! use Rust/Tim sort.
                                AlgoFlag = Algo_RustSort
                                LongRunSize = LastRunSize
                                FirstRun = FalseVal
                                IF (CurrDescend) THEN
                                    ! Reverse into a desired order.
                                    I = Last
                                    J = K - 1
                                    CALL Reverse_Order_Base0(A, I, J)
                                END IF
                                IF (LastDescend) THEN
                                    ! Reverse into a desired order.
                                    II = Start + 1
                                    JJ = Finish
                                    CALL Reverse_Order_Base0(A, II, JJ)
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
                                QPScheme = Mo3Scheme
                            ELSE
                                ! For a large (not so small) array with long last run,
                                ! use Rust/Tim sort.
                                AlgoFlag = Algo_RustSort
                                LongRunSize = LastRunSize
                                FirstRun = FalseVal
                                IF (CurrDescend) THEN
                                    ! Reverse into a desired order.
                                    I = Last
                                    J = K - 1
                                    CALL Reverse_Order_Base0(A, I, J)
                                END IF
                                IF (LastDescend) THEN
                                    ! Reverse into a desired order.
                                    II = Start + 1
                                    JJ = Finish
                                    CALL Reverse_Order_Base0(A, II, JJ)
                                END IF
                            END IF
                        ! Check whether the first and last runs are very short.
                        ELSEIF ((FirstRunSize <= 4).AND.(LastRunSize <= 4)) THEN
                            ! ------------------------------------------
                            ! Special Cases:  Totally Randomized Array
                            ! ------------------------------------------
                            ! Both first run and last run are too short
                            ! so use quick sort with median-of-three scheme.
                            AlgoFlag = Algo_QuickSort
                            QPScheme = Mo3Scheme
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
                            QPScheme = Mo3Scheme
                        ELSE
                            ! For a large array (more than 1024 elements), use Rust/Tim sort.
                            AlgoFlag = Algo_RustSort
                            LongRunSize = FirstRunSize
                            FirstRun = TrueVal
                            IF (CurrDescend) THEN
                                ! Reverse into a desired order
                                I = Last
                                J = K - 1
                                CALL Reverse_Order_Base0(A, I, J)
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
                            ! use quick sort algorithm with median-of-three scheme.
                            AlgoFlag = Algo_QuickSort
                            QPScheme = Mo3Scheme
                        ELSEIF (ASize <= QUICK_SHORT_CUTOFF) THEN
                            ! For a small array with the long run size more than 50% of ASize,
                            ! use quick sort algorithm with Hoare's scheme.
                            AlgoFlag = Algo_QuickSort
                            QPScheme = HoareScheme
                        ELSE
                            ! For a large (not so small) array with long run in the middle,
                            ! use Rust/Tim sort.
                            AlgoFlag = Algo_RustSort
                            LongRunSize = FirstRunSize
                            FirstRun = TrueVal
                            ! Reverse into a desired order if necessary.
                            IF (CurrDescend) THEN
                                ! Reverse into a desired order
                                I = Last
                                J = K - 1
                                CALL Reverse_Order_Base0(A, I, J)
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
                ! Reverse into a desired order.
                I = Last
                J = K - 1
                CALL Reverse_Order_Base0(A, I, J)
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

    !**************************************************************************

    RECURSIVE SUBROUTINE Quick_Mo3(A, NE, LStart, REnd, MaxDepth)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To sort the array in the range [LStart,REnd] in a desired order
        ! using recursive Quicksort algorithm.  If the number of elements in
        ! the range below the cutoff, then a pair-insertion sorting routine
        ! is used instead.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(:)     ! the array to be sorted
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
                CALL DualInsert_Guarded(A, LStart, REnd)
            ELSE
                CALL DualInsert_UnGuarded(A, LStart, REnd)
            END IF
            RETURN
        END IF

        ! compute center index
        Mid = LStart + (REnd-LStart) / 2

        ! check which partitioning scheme to be used
        IF ((A(LStart) /= A(Mid)).AND.(A(Mid) /= A(REnd)).AND.(A(LStart) /= A(REnd))) THEN

            IF (MaxDepth > 0) THEN
                ! partition with median-of-three partitioning scheme
                CALL Partition_Mo3(A, LStart, REnd, Mid, LEnd, RStart)
            ELSE
                ! partition with median of medians (ninther) partitioning scheme
                CALL Partition_Ninther(A, NE, LStart, REnd, Mid, LEnd, RStart)
            END IF

        ELSE

            ! partition with Dutch's national flag algorithm
            CALL Partition_3_Ways(A, LStart, REnd, LEnd, RStart)

        END IF

        ! compute NL, NR and NLimit
        NL = LEnd - LStart + 1
        NR = REnd - RStart + 1

        ! perform quick sort algorithm on the two sub-arrays
        CALL Quick_Mo3(A, NL, LStart, LEnd, MaxDepth-1)
        CALL Quick_Mo3(A, NR, RStart, REnd, MaxDepth-1)

        RETURN

    END SUBROUTINE Quick_Mo3

    !**************************************************************************

    RECURSIVE SUBROUTINE Quick_Hoare(A, NE, LStart, REnd, MaxDepth)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To sort the array in the range [LStart,REnd] in a desired order
        ! using recursive Quicksort algorithm.  If the number of elements in
        ! the range below the cutoff, then a pair-insertion sorting routine
        ! is used instead.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(:)     ! the array to be sorted
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
                CALL DualInsert_Guarded(A, LStart, REnd)
            ELSE
                CALL DualInsert_UnGuarded(A, LStart, REnd)
            END IF
            RETURN
        END IF

        ! compute center index
        Mid = LStart + (REnd-LStart) / 2

        ! check which partitioning scheme to be used
        IF ((A(LStart) /= A(Mid)).AND.(A(Mid) /= A(REnd)).AND.(A(LStart) /= A(REnd))) THEN

            IF (MaxDepth > 0) THEN
                ! partition with Hoare's partitioning scheme
                CALL Partition_Hoare(A, LStart, REnd, Mid, LEnd, RStart)
            ELSE
                ! partition with median of medians (ninther) partitioning scheme
                CALL Partition_Ninther(A, NE, LStart, REnd, Mid, LEnd, RStart)
            END IF

        ELSE

            ! partition with Dutch's national flag algorithm
            CALL Partition_3_Ways(A, LStart, REnd, LEnd, RStart)

        END IF

        ! compute NL, NR and NLimit
        NL = LEnd - LStart + 1
        NR = REnd - RStart + 1

        ! perform quick sort algorithm on the two sub-arrays
        CALL Quick_Hoare(A, NL, LStart, LEnd, MaxDepth-1)
        CALL Quick_Hoare(A, NR, RStart, REnd, MaxDepth-1)

        RETURN

    END SUBROUTINE Quick_Hoare

    !**************************************************************************

    SUBROUTINE Partition_Mo3(A, LStart, REnd, Mid, LEnd, RStart)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To partition an array using median-of-three partitioning scheme.
        ! The given array in the range [LStart,REnd] is partitioned
        ! into two sub-arrays, Left and Right, such that all elements
        ! in the Left sub-array in the range [LStart,LEnd] are less
        ! than or equal to the selected pivot, and all elements in
        ! the Right sub-array in the range [RStart,REnd] are greater
        ! than or equal to the selected pivot.

    !** REFERENCES
        ! Adapted from subroutine 'Sort' of Numerical Recipe F90

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(:)     ! the array to be sorted
        tIndex,    INTENT(IN)       :: LStart   ! on entry, the start of the given array
                                                ! on exit, the start of the Left sub-array
        tIndex,    INTENT(IN)       :: REnd     ! on entry, the end of the given array
                                                ! on exit, the end of the Right sub-array
        tIndex,    INTENT(IN)       :: Mid      ! the middle between LStart and REnd
        tIndex,    INTENT(OUT)      :: LEnd     ! the end of the Left sub-array
        tIndex,    INTENT(OUT)      :: RStart   ! the start of the Right sub-array

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tVarScalar  :: Pivot, Temp
        tIndex      :: LPtr, UPtr

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Pivot, MOLD=A(LStart))
        ALLOCATE(Temp, MOLD=A(LStart))
#endif

        ! choose median of left, center, and right elements as partitioning element (the pivot)
        ! also, rearrange so that A(LStart) < A(LStart+1) < A(REnd)
        CALL Sort_3_Items(A, LStart, Mid, REnd)
        EXCHANGE(A, Mid, LStart+1)

        ! set pivot value
        Pivot = A(LStart+1)

        ! set working pointers
        LPtr = LStart + 1
        UPtr = REnd

        DO
            ! increment the left pointer until we find an element
            ! that is greater than the pivot
            DO
                LPtr = LPtr + 1
                IF (COMPARE_GLE(Pivot, A(LPtr))) EXIT
            END DO

            ! decrement the right pointer until we find an element
            ! that is less than the pivot
            DO
                UPtr = UPtr - 1
                IF (COMPARE_GLE(A(UPtr), Pivot)) EXIT
            END DO

            ! exit if the pointers cross
            IF (UPtr < LPtr) EXIT

            ! swap A(LPtr) and A(UPtr)
            EXCHANGE(A, LPtr, UPtr)
        END DO

        ! insert partitioning element
        A(LStart+1) = A(UPtr)
        A(UPtr)     = Pivot

        ! set output indices
        LEnd   = UPtr - 1
        RStart = LPtr

        RETURN

    END SUBROUTINE Partition_Mo3

    !**************************************************************************

    SUBROUTINE Partition_Hoare(A, LStart, REnd, Mid, LEnd, RStart)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To partition an array using Hoare's partitioning scheme.
        ! The given array in the range [LStart,REnd] is partitioned
        ! into two sub-arrays, Left and Right, such that all elements
        ! in the Left sub-array in the range [LStart,LEnd] are less
        ! than or equal to the selected pivot, and all elements in
        ! the Right sub-array in the range [RStart,REnd] are greater
        ! than or equal to the selected pivot.

    !** REFERENCES
        ! Pseudocode from Wikipedia (https://en.wikipedia.org/wiki/Quicksort)
        ! Source code from "https://github.com/jyahn/Quicksort-visualization"

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(:)     ! the array to be sorted
        tIndex,    INTENT(IN)       :: LStart   ! on entry, the start of the given array
                                                ! on exit, the start of the Left sub-array
        tIndex,    INTENT(IN)       :: REnd     ! on entry, the end of the given array
                                                ! on exit, the end of the Right sub-array
        tIndex,    INTENT(IN)       :: Mid      ! the middle between LStart and REnd
        tIndex,    INTENT(OUT)      :: LEnd     ! the end of the Left sub-array
        tIndex,    INTENT(OUT)      :: RStart   ! the start of the Right sub-array

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tVarScalar  :: Pivot, Temp
        tIndex      :: LPtr, RPtr

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Pivot, MOLD=A(LStart))
        ALLOCATE(Temp, MOLD=A(LStart))
#endif

        ! Select the pivot
        Pivot = A(FLOOR(REAL(Mid,KIND=kFP)))

        ! Perform partitioning.
        LPtr = LStart
        RPtr = REnd
        DO WHILE (LPtr <= RPtr)

            ! Increment the left pointer until we find an element
            ! that is greater than or equal to the pivot.
            DO WHILE (COMPARE_GLT(A(LPtr), Pivot))
                LPtr = LPtr + 1
            END DO

            ! Decrement the right pointer until we find an element
            ! that is less than or equal to the pivot.
            DO WHILE (COMPARE_GLT(Pivot, A(RPtr)))
                RPtr = RPtr - 1
            END DO

            ! A pair of values have been found where A(LPtr) is greater than the pivot
            ! and A(RPtr) is less than the pivot; thus, while the left pointer is less
            ! than or equal to the right pointer, swap A(LPtr) with A(RPtr).
            IF (LPtr <= RPtr) THEN
                EXCHANGE(A, LPtr, RPtr)
                LPtr = LPtr + 1
                RPtr = RPtr - 1
            END IF

        END DO

        ! Set output indices
        LEnd   = LPtr-1
        RStart = LPtr

        RETURN

    END SUBROUTINE Partition_Hoare

    !**************************************************************************

    SUBROUTINE Partition_Ninther(A, NE, LStart, REnd, Mid, LEnd, RStart)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To partition an array using median-of-three partitioning scheme.
        ! The given array in the range [LStart,REnd] is partitioned
        ! into two sub-arrays, Left and Right, such that all elements
        ! in the Left sub-array in the range [LStart,LEnd] are less
        ! than or equal to the selected pivot, and all elements in
        ! the Right sub-array in the range [RStart,REnd] are greater
        ! than or equal to the selected pivot.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(:)     ! the array to be sorted
        tIndex,    INTENT(IN)       :: NE       ! number of array elements
        tIndex,    INTENT(IN)       :: LStart   ! on entry, the start of the given array
                                                ! on exit, the start of the Left sub-array
        tIndex,    INTENT(IN)       :: REnd     ! on entry, the end of the given array
                                                ! on exit, the end of the Right sub-array
        tIndex,    INTENT(IN)       :: Mid      ! the middle between LStart and REnd
        tIndex,    INTENT(OUT)      :: LEnd     ! the end of the Left sub-array
        tIndex,    INTENT(OUT)      :: RStart   ! the start of the Right sub-array

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tVarScalar  :: Pivot, Temp
        tIndex      :: LPtr, UPtr
        tIndex      :: M1, M2, M3
        tIndex      :: Eps, Ninther

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Pivot, MOLD=A(LStart))
        ALLOCATE(Temp, MOLD=A(LStart))
#endif

        ! use Tukey's ninther as partitioning element (the pivot)
        ! also, rearrange so that A(LStart) < A(LStart+1) < A(REnd)
        Eps = NE/8
        M1 = LStart + Eps
        M2 = Mid
        M3 = REnd - Eps
        M1 = Median_Of_Three(A, LStart, M1, M1+Eps)
        M2 = Median_Of_Three(A, M2-Eps, M2, M2+Eps)
        M3 = Median_Of_Three(A, M3-Eps, M3, REnd)
        Ninther = Median_Of_Three(A, M1, M2, M3)
        IF ((Ninther == LStart).OR.(Ninther == REnd)) THEN
            CALL Sort_3_Items(A, LStart, Mid, REnd)
            EXCHANGE(A, Mid, LStart+1)
        ELSE
            CALL Sort_3_Items(A, LStart, Ninther, REnd)
            EXCHANGE(A, Ninther, LStart+1)
        END IF

        ! set pivot value
        Pivot = A(LStart+1)

        ! set working pointers
        LPtr = LStart + 1
        UPtr = REnd

        DO
            ! increment the left pointer until we find an element
            ! that is greater than the pivot
            DO
                LPtr = LPtr + 1
                IF (COMPARE_GLE(Pivot, A(LPtr))) EXIT
            END DO

            ! decrement the right pointer until we find an element
            ! that is less than the pivot
            DO
                UPtr = UPtr - 1
                IF (COMPARE_GLE(A(UPtr), Pivot)) EXIT
            END DO

            ! exit if the pointers cross
            IF (UPtr < LPtr) EXIT

            ! swap A(LPtr) and A(UPtr)
            EXCHANGE(A, LPtr, UPtr)
        END DO

        ! insert partitioning element
        A(LStart+1) = A(UPtr)
        A(UPtr)     = Pivot

        ! set output indices
        LEnd   = UPtr - 1
        RStart = LPtr

        RETURN

    END SUBROUTINE Partition_Ninther

    !**************************************************************************

    SUBROUTINE Partition_3_Ways(A, LStart, REnd, LEnd, RStart)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To partition an array using 3-way partitioning scheme.

    !** REFERENCE
        ! The routine is based on a part of the "Java_Sort" subroutine where the
        ! "Java_Sort" routine is a translation of Java's sorting algorithm.
        ! OpenJDK's Java codes for Java's sorting algorithm can be obtained from
        ! https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/util/DualPivotQuicksort.java.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(0:)    ! the array to be sorted
        tIndex,    INTENT(IN)       :: LStart   ! on entry, the start of the given array
                                                ! on exit, the start of the Left sub-array
        tIndex,    INTENT(IN)       :: REnd     ! on entry, the end of the given array
                                                ! on exit, the end of the Right sub-array
        tIndex,    INTENT(OUT)      :: LEnd     ! the end of the Left sub-array
        tIndex,    INTENT(OUT)      :: RStart   ! the start of the Right sub-array

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tVarScalar  :: A3, AK, Temp
        tVarScalar  :: Pivot, Pivot1, Pivot2
        tIndex      :: Low, High, iEnd, Size, Last
        tIndex      :: Step
        tIndex      :: K, NL, NM, NR
        tIndex      :: E1, E2, E3, E4, E5
        tIndex      :: Lower, Upper

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Pivot, MOLD=A(LStart))
        ALLOCATE(Pivot1, MOLD=A(LStart))
        ALLOCATE(Pivot2, MOLD=A(LStart))
        ALLOCATE(A3, MOLD=A(LStart))
        ALLOCATE(AK, MOLD=A(LStart))
        ALLOCATE(Temp, MOLD=A(LStart))
#endif

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
        A3 = A(E3)

        ! Sort these elements in place by the combination
        ! of 4-element sorting network and insertion sort.
        !    5 ------o-----------o------------
        !            |           |
        !    4 ------|-----o-----o-----o------
        !            |     |           |
        !    2 ------o-----|-----o-----o------
        !                  |     |
        !    1 ------------o-----o------------
        IF (COMPARE_GLT(A(E5), A(E2))) THEN
            ! t = A(E5); A(E5) = A(E2); A(E2) = t
            EXCHANGE(A, E5, E2)
        END IF
        IF (COMPARE_GLT(A(E4), A(E1))) THEN
            ! t = A(E4); A(E4) = A(E1); A(E1) = t
            EXCHANGE(A, E4, E1)
        END IF
        IF (COMPARE_GLT(A(E5), A(E4))) THEN
            ! t = A(E5); A(E5) = A(E4); A(E4) = t
            EXCHANGE(A, E5, E4)
        END IF
        IF (COMPARE_GLT(A(E2), A(E1))) THEN
            ! t = A(E2); A(E2) = A(E1); A(E1) = t
            EXCHANGE(A, E2, E1)
        END IF
        IF (COMPARE_GLT(A(E4), A(E2))) THEN
            ! t = A(E4); A(E4) = A(E2); A(E2) = t
            EXCHANGE(A, E4, E2)
        END IF

        IF (COMPARE_GLT(A3, A(E2))) THEN
            IF (COMPARE_GLT(A3, A(E1))) THEN
                A(E3) = A(E2)
                A(E2) = A(E1)
                A(E1) = A3
            ELSE
                A(E3) = A(E2)
                A(E2) = A3
            END IF
        ELSEIF (COMPARE_GLT(A(E4), A3)) THEN
            IF (COMPARE_GLT(A(E5), A3)) THEN
                A(E3) = A(E4)
                A(E4) = A(E5)
                A(E5) = A3
            ELSE
                A(E3) = A(E4)
                A(E4) = A3
            END IF
        END IF

        ! Pointers
        Lower = Low     ! The index of the last element of the left part
        Upper = iEnd    ! The index of the first element of the right part

        ! *** Use single pivot in case of many equal elements ***

        ! Use the third of the five sorted elements as the pivot.
        ! This value is inexpensive approximation of the median.
        Pivot = A(E3)

        ! The first element to be sorted is moved to the
        ! location formerly occupied by the pivot. After
        ! completion of partitioning the pivot is swapped
        ! back into its final position, and excluded from
        ! the next subsequent sorting.
        A(E3) = A(Lower)

        ! Traditional 3-way (Dutch National Flag) partitioning
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
            AK = A(K)

            IF (AK /= Pivot) THEN
                A(K) = Pivot

                IF (COMPARE_GLT(AK, Pivot)) THEN
                    ! Move A(K) to the left side
                    Lower = Lower + 1
                    DO WHILE (COMPARE_GLT(A(Lower), Pivot))
                        Lower = Lower + 1
                    END DO

                    IF (COMPARE_GLT(Pivot, A(Lower))) THEN
                        Upper = Upper - 1
                        A(Upper) = A(Lower)
                    END IF
                    A(Lower) = AK
                ELSE
                    ! AK > Pivot - Move A(K) to the right side
                    Upper = Upper - 1
                    A(Upper) = AK
                END IF
            END IF
            K = K - 1
        END DO

        ! Swap the pivot into its final position.
        A(Low) = A(Lower)
        A(Lower) = Pivot

        ! set output indices
        LEnd   = Lower
        RStart = Upper + 1

        RETURN

    END SUBROUTINE Partition_3_Ways

    !**********************************************************************************

END SUBROUTINE Wise_Sort_Unstable

!******************************************************************************

MODULE SUBROUTINE Wise_Sort_Stable(A)

!^ **PURPOSE OF THIS SUBROUTINE**: <br>
!   To sort an array in a desired order using the *WiseSort-Stable* algorithm,
!   which is a hybrid *stable* algorithm that employs various sorting algorithms
!   including the *insertion sort*, *mergesort* and *quicksort* algorithms. <br>
!    <br>
!  **TECHNICAL INFORMATION**: <br>
!   Similar to the *WiseSort* algorithm used in the *Wise_Sort_Unstable* subroutine,
!   the *WiseSort-Stable* algorithm is a hybrid algorithm originated in this library
!   (*XpfLib*).  Unlike the *WiseSort* algorithm, which is an unstable algorithm, the
!   *WiseSort-Stable* algorithm (as the name implied) is a stable algorithm that sorts
!   the given array using a variety of stable sorting algorithms.  Similar to the
!   *WiseSort* algorithm, the *WiseSort-Stable* algorithm first inspects the specified
!   array by checking if there is any pattern in the given array and then decides which
!   stable sorting algorithm is most suitable for the array.  It then sorts the array
!   using the chosen algorithm. <br>
!   The selection of a stable sorting algorithm is based on the same facts given in
!   the *Wise_Sort_Unstable* subroutine; hence, the *WiseSort-Stable* algorithm employs
!   various stable sorting algorithms including: <br>
!   1. *Insertion sort* algorithm (guarded version only) for a small-size (sub)array. <br>
!   2. *Merge-sort-based* algorithms consisting of <br>
!    - Java's merging runs for a highly structured array (an array with a certain
!      recognized pattern, e.g. saw- or wave-like pattern), <br>
!    - Rust's merge sort (or the so-called simplified TimSort) for a mostly ordered array
!      (an array having a very long ordered run, either in ascending or descending order), <br>
!    - Merge sort with half-copying for a small-size array or a partially randomized array
!      (an array with one not-too-short and not-too-long run and many very-very short runs). <br>
!   3. Quicksort algorithm with stable partitioning scheme for a totally-randomized array
!      (an array having VERY SHORT ordered runs, e.g. an ordered run with less than 4 elements). <br>
!   4. Hybrid quick-merge sorting algorithm for a partially randomized array (an array with
!      many not-too-short and not-too-long runs). <br>
!      Note: the hybrid quick-merge sorting algorithm is a quick-sort-based algorithm that
!      switches to the mergesort with half-copying algorithm for the smaller sub-array after
!      the stable partitioning process while it recurs on the larger sub-array. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(:)   !! array to be sorted

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! parameters for sorting algorithm
    tInteger, PARAMETER :: Algo_None       = 0  ! no algorithm needed (data already sorted)
    tInteger, PARAMETER :: Algo_MergeRun   = 1  ! Java's merging run (for highly structure data)
    tInteger, PARAMETER :: Algo_RustSort   = 2  ! Tim sort (for long first run - more than half)
    tInteger, PARAMETER :: Algo_MergeQuad  = 3  ! Merge sort (for totally randomized data)
    tInteger, PARAMETER :: Algo_MergeSort  = 4  ! Merge sort (for small arrays)
    tInteger, PARAMETER :: Algo_HybridSort = 5  ! Hybrid (Quick-Merge) sort (for partly ordered data)
    ! parameters for cutoffs
    tIndex,   PARAMETER :: Insertion_CutOff_Init      = 40_kIndex  ! initial cutoff to pair insertion sort
    tIndex,   PARAMETER :: QuickInsertion_CutOff_Low  = 75_kIndex  ! cutoff to pair insertion sort for low N
    tIndex,   PARAMETER :: QuickInsertion_CutOff_High = 95_kIndex  ! cutoff to pair insertion sort for high N
    tIndex,   PARAMETER :: MergeInsertion_CutOff_Low  = 25_kIndex  ! cutoff to pair insertion sort
    tIndex,   PARAMETER :: MergeInsertion_CutOff_High = 55_kIndex  ! cutoff to pair insertion sort
    tIndex,   PARAMETER :: MergeQuad_CutOff_Low       = 35_kIndex  ! cutoff to pair insertion sort
    tIndex,   PARAMETER :: MergeQuad_CutOff_High      = 40_kIndex  ! cutoff to pair insertion sort

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
!    tVarAlloc           :: Buffer(:)    ! work space
    tIndex, ALLOCATABLE :: Run(:)       ! info for Java's merge runs
    tIndex              :: RunCount     ! info for Java's merge runs
    tIndex              :: LongRunSize  ! long run size used by TimSort
    tLogical            :: FirstRun     ! long run size used by TimSort
    tIndex              :: QCutOff      ! cutoff to pair insertion sort for quick sort
    tIndex              :: MCutOff      ! cutoff to pair insertion sort for merge sort
    tIndex              :: AlgoFlag     ! flag for sorting algorithm
    tIndex              :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(A, KIND=kIndex)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether the array is small
    IF (NA <= Insertion_CutOff_Init) THEN
        ! for small array, use pair insertion sort algorithm
        CALL Insert_Guarded(A, 1_kIndex, NA)
        RETURN
    END IF

    ! inspect runs to determine sorting algorithm to be used
    CALL Expert_Inspection(A, AlgoFlag, LongRunSize, FirstRun, Run, RunCount)

    ! perform sorting
    SELECT CASE (AlgoFlag)
    CASE (Algo_None)
        ! do nothing since the array is already sorted
    CASE (Algo_MergeRun)
        ! Note: Java's merge runs routine appears to be unstable.
!        IF (NA <= 40960) THEN
!            BLOCK
!#ifdef TYPE_IS_IN_Comparable_CLASS
!                tVarAlloc   :: Buffer(:)
!#else
!                tVarLocal   :: Buffer(NA)
!#endif
!#ifdef TYPE_IS_IN_Comparable_CLASS
!                ! allocate a buffer to use as scratch memory.
!                ALLOCATE(Buffer(NA), MOLD=A)
!#endif
!                ! perform Java's merge runs
!                Dummy = Java_Merge_Runs(A, Buffer, 0, 1, Run, 0, RunCount)
!            END BLOCK
!        ELSE
            CALL Rust_Sort(A)
!        END IF
    CASE (Algo_RustSort)
        BLOCK
#ifdef TYPE_IS_IN_Comparable_CLASS
            tVarAlloc   :: Buffer(:)
#else
            tVarLocal   :: Buffer(NA/2)
#endif
#ifdef TYPE_IS_IN_Comparable_CLASS
            ! allocate a buffer to use as scratch memory.
            ALLOCATE(Buffer(NA/2), MOLD=A)
#endif
            ! perform Rust's merge sort algorithm
            CALL Rust_Merge_Stable(A, NA, LongRunSize, FirstRun, Buffer)
        END BLOCK
    CASE (Algo_MergeQuad)
        ! set cutoff to pair insertion sort algorithm for small arrays
        IF (NA < 2000_kIndex) THEN
            QCutOff = MergeQuad_CutOff_Low
        ELSE
            QCutOff = MergeQuad_CutOff_High
        END IF
        BLOCK
#ifdef TYPE_IS_IN_Comparable_CLASS
            tVarAlloc   :: Buffer(:)
#else
            tVarLocal   :: Buffer(NA)
#endif
#ifdef TYPE_IS_IN_Comparable_CLASS
            ! allocate a buffer to use as scratch memory.
            ALLOCATE(Buffer(NA), MOLD=A)
#endif
            ! perform merge sort
            CALL Quad_Split_Merge(A, 0, NA, Buffer, QCutOff)
        END BLOCK
    CASE (Algo_MergeSort)
        ! set cutoff depending on the size of the input array
        IF (NA < 2000_kIndex) THEN
            MCutOff = MergeInsertion_CutOff_Low
        ELSE
            MCutOff = MergeInsertion_CutOff_High
        END IF
        BLOCK
#ifdef TYPE_IS_IN_Comparable_CLASS
            tVarAlloc   :: Buffer(:)
#else
            tVarLocal   :: Buffer(NA-NA/2)
#endif
#ifdef TYPE_IS_IN_Comparable_CLASS
            ! allocate a buffer to use as scratch memory.
            ALLOCATE(Buffer(NA-NA/2), MOLD=A)
#endif
            ! perform merge sort
            CALL MergeSort_HalfCopy(A, 0_kIndex, NA, Buffer)
        END BLOCK
    CASE (Algo_HybridSort)
        SELECT CASE (NA)
        CASE (:1000)
            ! set cutoff depending on the size of the input array
            MCutOff = MergeInsertion_CutOff_Low
            BLOCK
#ifdef TYPE_IS_IN_Comparable_CLASS
                tVarAlloc   :: Buffer(:)
#else
                tVarLocal   :: Buffer(NA-NA/2)
#endif
#ifdef TYPE_IS_IN_Comparable_CLASS
                ! allocate a buffer to use as scratch memory.
                ALLOCATE(Buffer(NA-NA/2), MOLD=A)
#endif
                ! perform merge sort
                CALL MergeSort_HalfCopy(A, 0_kIndex, NA, Buffer)
            END BLOCK
        CASE (1001:100000)
            IF (NA < 2000_kIndex) THEN
                QCutOff = QuickInsertion_CutOff_Low
            ELSE
                QCutOff = QuickInsertion_CutOff_High
            END IF
            BLOCK
#ifdef TYPE_IS_IN_Comparable_CLASS
                tVarAlloc   :: Buffer(:)
#else
                tVarLocal   :: Buffer(NA)
#endif
#ifdef TYPE_IS_IN_Comparable_CLASS
                ! allocate a buffer to use as scratch memory.
                ALLOCATE(Buffer(NA), MOLD=A)
#endif
                ! perform quick-merge sort
                CALL Quick_Recur_Sort(A, Buffer, 1_kIndex, NA)
            END BLOCK
        CASE (100001:)
            ! set cutoff depending on the size of the input array
            QCutOff = QuickInsertion_CutOff_High
            BLOCK
#ifdef TYPE_IS_IN_Comparable_CLASS
                tVarAlloc   :: Buffer(:)
#else
                tVarLocal   :: Buffer(NA)
#endif
#ifdef TYPE_IS_IN_Comparable_CLASS
                ! allocate a buffer to use as scratch memory.
                ALLOCATE(Buffer(NA), MOLD=A)
#endif
                ! perform quick-merge sort
                CALL Quick_Iter_Sort(A, Buffer, 1_kIndex, NA)
            END BLOCK
        END SELECT
    END SELECT

    RETURN

CONTAINS

    SUBROUTINE Expert_Inspection(A, AlgoFlag, LongRunSize, FirstRun, Run, Count)

    !** PURPOSE OF THIS SUBROUTINE
        ! To carefully inspect whether there is any pattern in the given array
        ! and determine wisely an appropriate stable sorting algorithm for the array.

    !** TECHNICAL INFORMATION
        ! - This routine is similar in concept to the 'Expert_Inspection' subroutine in
        !   the 'Wise_Sort_Unstable' subroutine but the details are quite different due
        !   to the fact that most stable sorting algorithms perform much more slower for
        !   randomized arrays.
        ! - Similar to the 'Expert_Inspection' subroutine in the 'Wise_Sort_Unstable'
        !   subroutine, if the routine detects that the current run is in an order opposite
        !   to the desired one, it will reverse the run if necessary.  However, to make the
        !   sorting stable, the reversion is used only on the truly descending run (no equal
        !   element in the run) whereas the Rust's merge sort algorithm is used to sort if
        !   the detected run is not truly descending (some equal elements in the run).

    !** REFERENCE
        ! The code is adapted from the 'Expert_Inspection' subroutine in the
        ! 'Wise_Sort_Unstable' subroutine.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument,           INTENT(INOUT)  :: A(0:)        ! array to be sorted
        tIndex,              INTENT(OUT)    :: AlgoFlag     ! flag indicating algorithm to be used
        tIndex,              INTENT(OUT)    :: LongRunSize  ! needed by Rust's TimSort algorithm
        tLogical,            INTENT(OUT)    :: FirstRun     ! needed by Rust's TimSort algorithm
        tIndex, ALLOCATABLE, INTENT(OUT)    :: Run(:)       ! needed by Java's merge-run algorithm
        tIndex,              INTENT(OUT)    :: Count        ! needed by Java's merge-run algorithm

    !** SUBROUTINE PARAMETER DECLARATIONS:
        ! parameters for expert inspection
        tInteger, PARAMETER :: MIN_FIRST_RUN_SIZE    = 16   ! Min size of the first run to continue with scanning
        tInteger, PARAMETER :: MIN_FIRST_RUNS_FACTOR = 7    ! Min factor for the first runs to continue scanning
        tInteger, PARAMETER :: MAX_RUN_CAPACITY      = 128  ! Max capacity of the index array for tracking runs
        tInteger, PARAMETER :: SMALL_CUTOFF          = 1024 ! Cutoff to merge sort algorithm

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tVarAlloc   :: WorkSpace(:)
        tVarScalar  :: Temp, AK
        tIndex      :: Low, ASize
        tIndex      :: RunSize, NewRunSize
        tIndex      :: High, Last
        tIndex      :: I, J, K
        tIndex      :: II, JJ
        tIndex      :: KFirstEqual, TrueFirstDescendRunSize
        tIndex      :: KLastEqual, TrueLastDescendRunSize
        tIndex      :: Start, Finish
        tIndex      :: MinFirstRun
        tIndex      :: FirstRunSize, LastRunSize, CurrRunSize
        tIndex      :: AscendRunLimit, DescendRunLimit
        tLogical    :: CurrDescend, FirstDescend, LastDescend
        tLogical    :: EqualCase, ShortSecondRun, ReturnNow
        tLogical    :: FirstEqualCase, LastEqualCase, CurrEqualCase
        tIndex      :: ID(5)

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(AK, MOLD=A(0))
        ALLOCATE(Temp, MOLD=A(0))
#endif

        ! Initialize working variables.
        Low = 0_kIndex
        ASize = SIZE(A)
        High = Low + ASize
        Count = 0_kIndex
        Last = Low
        LongRunSize = 0_kIndex
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
        ! these two expressions are determined experimentally.
        AscendRunLimit  = INT(0.65_kDouble*REAL(ASize, KIND=kDouble), KIND=kIndex)
        DescendRunLimit = INT(0.25_kDouble*REAL(ASize, KIND=kDouble), KIND=kIndex)

        ! Determine minimum size of the first run to continue with scanning.
        MinFirstRun = MAX(ASize/MAX_RUN_CAPACITY, MIN_FIRST_RUN_SIZE)

        ! Identify all possible runs.
        K = Low + 1
        DO WHILE (K < High)

            ! Find the end index of the current run.
            IF (COMPARE_GLT(A(K-1), A(K))) THEN

                ! Identify ascending sequence.
                K = K + 1
                DO WHILE ((K < High).AND.(COMPARE_GLE(A(K-1), A(K))))
                    IF (A(K-1) == A(K)) EqualCase = TrueVal
                    K = K + 1
                END DO
                CurrDescend = FalseVal

            ELSEIF (COMPARE_GLT(A(K), A(K-1))) THEN

                ! Identify descending sequence.
                K = K + 1
                DO WHILE ((K < High).AND.(COMPARE_GLE(A(K), A(K-1))))
                    IF (A(K-1) == A(K)) THEN
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
                AK = A(K)
                K = K + 1
                DO WHILE ((K < High).AND.(AK == A(K)))
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
                            ! Reverse into a desired order.
                            I = Last
                            J = KFirstEqual - 1
                            CALL Reverse_Order_Base0(A, I, J)
                            ! Allocate a buffer to use as scratch memory.
                            ALLOCATE(WorkSpace(1:FirstRunSize/2), MOLD=A)
                            ! perform Rust's merge sort algorithm
                            CALL Rust_Merge_Stable(A, FirstRunSize, TrueFirstDescendRunSize, TrueVal, WorkSpace)
                            DEALLOCATE(WorkSpace)
                        ELSE
                            ! Reverse into a desired order.
                            I = Last
                            J = K - 1
                            CALL Reverse_Order_Base0(A, I, J)
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
                            IF (COMPARE_GLT(A(Start), A(Start+1))) THEN
                                ! Identify ascending sequence.
                                DO WHILE (Start > 0)
                                    IF (A(Start) == A(Start-1)) THEN
                                        EqualCase = TrueVal
                                        LastEqualCase = TrueVal
                                    END IF
                                    IF (COMPARE_GLT(A(Start), A(Start-1))) EXIT
                                    Start = Start - 1
                                END DO
                                LastDescend = FalseVal
                            ELSEIF (COMPARE_GLT(A(Start+1), A(Start))) THEN
                                ! Identify descending sequence.
                                DO WHILE (Start > 0)
                                    IF (A(Start) == A(Start-1)) THEN
                                        EqualCase = TrueVal
                                        LastEqualCase = TrueVal
                                        IF (KLastEqual == Low) KLastEqual = Start
                                    END IF
                                    IF (COMPARE_GLT(A(Start-1), A(Start))) EXIT
                                    Start = Start - 1
                                END DO
                                LastDescend = TrueVal
                            ELSE
                                EqualCase = TrueVal
                                LastEqualCase = TrueVal
                                IF (KLastEqual == Low) KLastEqual = Start
                                ! Identify constant sequence.
                                AK = A(Start)
                                Start = Start - 1
                                DO WHILE ((Start > 0).AND.(AK == A(Start)))
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
                                ! Reverse into a desired order.
                                II = KLastEqual + 1
                                JJ = Finish
                                CALL Reverse_Order_Base0(A, II, JJ)
                                ! Allocate a buffer to use as scratch memory.
                                ALLOCATE(WorkSpace(1:LastRunSize/2), MOLD=A)
                                ! perform Rust's merge sort algorithm
                                CALL Rust_Merge_Stable(A(Start+1:Finish), LastRunSize, TrueLastDescendRunSize, FalseVal, WorkSpace)
                                DEALLOCATE(WorkSpace)
                            ELSE
                                ! Reverse into a desired order.
                                II = Start + 1
                                JJ = Finish
                                CALL Reverse_Order_Base0(A, II, JJ)
                            END IF
                        END IF
                    END IF
                    IF (FirstDescend) THEN
                        IF (FirstEqualCase) THEN
                            ! Reverse into a desired order.
                            I = Last
                            J = KFirstEqual - 1
                            CALL Reverse_Order_Base0(A, I, J)
                            ! Allocate a buffer to use as scratch memory.
                            ALLOCATE(WorkSpace(1:FirstRunSize/2), MOLD=A)
                            ! perform Rust's merge sort algorithm
                            CALL Rust_Merge_Stable(A, FirstRunSize, TrueFirstDescendRunSize, TrueVal, WorkSpace)
                            DEALLOCATE(WorkSpace)
                        ELSE
                            ! Reverse into a desired order.
                            I = Last
                            J = K - 1
                            CALL Reverse_Order_Base0(A, I, J)
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
                        IF (COMPARE_GLT(A(Start), A(Start+1))) THEN
                            ! Identify ascending sequence.
                            DO WHILE (Start > 0)
                                IF (A(Start) == A(Start-1)) THEN
                                    EqualCase = TrueVal
                                    LastEqualCase = TrueVal
                                END IF
                                IF (COMPARE_GLT(A(Start), A(Start-1))) EXIT
                                Start = Start - 1
                            END DO
                            LastDescend = FalseVal
                        ELSEIF (COMPARE_GLT(A(Start+1), A(Start))) THEN
                            ! Identify descending sequence.
                            DO WHILE (Start > 0)
                                IF (A(Start) == A(Start-1)) THEN
                                    EqualCase = TrueVal
                                    LastEqualCase = TrueVal
                                    IF (KLastEqual == Low) KLastEqual = Start
                                END IF
                                IF (COMPARE_GLT(A(Start-1), A(Start))) EXIT
                                Start = Start - 1
                            END DO
                            LastDescend = TrueVal
                        ELSE
                            EqualCase = TrueVal
                            LastEqualCase = TrueVal
                            IF (KLastEqual == Low) KLastEqual = Start
                            ! Identify constant sequence.
                            AK = A(Start)
                            Start = Start - 1
                            DO WHILE ((Start > 0).AND.(AK == A(Start)))
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
                                ! Reverse into a desired order.
                                I = Last
                                J = KFirstEqual - 1
                                CALL Reverse_Order_Base0(A, I, J)
                                ! Allocate a buffer to use as scratch memory.
                                ALLOCATE(WorkSpace(1:FirstRunSize/2), MOLD=A)
                                ! perform Rust's merge sort algorithm
                                CALL Rust_Merge_Stable(A, FirstRunSize, TrueFirstDescendRunSize, TrueVal, WorkSpace)
                                DEALLOCATE(WorkSpace)
                            ELSE
                                ! Reverse into a desired order.
                                I = Last
                                J = K - 1
                                CALL Reverse_Order_Base0(A, I, J)
                            END IF
                        END IF
                        IF (LastDescend) THEN
                            IF (LastEqualCase) THEN
                                ! Reverse into a desired order.
                                II = KLastEqual + 1
                                JJ = Finish
                                CALL Reverse_Order_Base0(A, II, JJ)
                                ! Allocate a buffer to use as scratch memory.
                                ALLOCATE(WorkSpace(1:LastRunSize/2), MOLD=A)
                                ! perform Rust's merge sort algorithm
                                CALL Rust_Merge_Stable(A(Start+1:Finish), LastRunSize, TrueLastDescendRunSize, FalseVal, WorkSpace)
                                DEALLOCATE(WorkSpace)
                            ELSE
                                ! Reverse into a desired order.
                                II = Start + 1
                                JJ = Finish
                                CALL Reverse_Order_Base0(A, II, JJ)
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
                                        IF (A(I) == A(J)) THEN
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
                                ! Reverse into a desired order.
                                II = KLastEqual + 1
                                JJ = Finish
                                CALL Reverse_Order_Base0(A, II, JJ)
                                ! Allocate a buffer to use as scratch memory.
                                ALLOCATE(WorkSpace(1:LastRunSize/2), MOLD=A)
                                ! perform Rust's merge sort algorithm
                                CALL Rust_Merge_Stable(A(Start+1:Finish), LastRunSize, TrueLastDescendRunSize, FalseVal, WorkSpace)
                                DEALLOCATE(WorkSpace)
                            ELSE
                                ! Reverse into a desired order.
                                II = Start + 1
                                JJ = Finish
                                CALL Reverse_Order_Base0(A, II, JJ)
                            END IF
                        END IF
                        IF (FirstDescend) THEN
                            IF (FirstEqualCase) THEN
                                ! Reverse into a desired order.
                                I = Last
                                J = KFirstEqual - 1
                                CALL Reverse_Order_Base0(A, I, J)
                                ! Allocate a buffer to use as scratch memory.
                                ALLOCATE(WorkSpace(1:FirstRunSize/2), MOLD=A)
                                ! perform Rust's merge sort algorithm
                                CALL Rust_Merge_Stable(A, FirstRunSize, TrueFirstDescendRunSize, TrueVal, WorkSpace)
                                DEALLOCATE(WorkSpace)
                            ELSE
                                ! Reverse into a desired order.
                                I = Last
                                J = K - 1
                                CALL Reverse_Order_Base0(A, I, J)
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
                                ! Reverse into a desired order.
                                I = Last
                                J = KFirstEqual - 1
                                CALL Reverse_Order_Base0(A, I, J)
                                ! Allocate a buffer to use as scratch memory.
                                ALLOCATE(WorkSpace(1:FirstRunSize/2), MOLD=A)
                                ! perform Rust's merge sort algorithm
                                CALL Rust_Merge_Stable(A, FirstRunSize, TrueFirstDescendRunSize, TrueVal, WorkSpace)
                                DEALLOCATE(WorkSpace)
                            ELSE
                                ! Reverse into a desired order.
                                I = Last
                                J = K - 1
                                CALL Reverse_Order_Base0(A, I, J)
                            END IF
                        END IF
                        IF (LastDescend) THEN
                            IF (LastEqualCase) THEN
                                ! Reverse into a desired order.
                                II = KLastEqual + 1
                                JJ = Finish
                                CALL Reverse_Order_Base0(A, II, JJ)
                                ! Allocate a buffer to use as scratch memory.
                                ALLOCATE(WorkSpace(1:LastRunSize/2), MOLD=A)
                                ! perform Rust's merge sort algorithm
                                CALL Rust_Merge_Stable(A(Start+1:Finish), LastRunSize, TrueLastDescendRunSize, FalseVal, WorkSpace)
                                DEALLOCATE(WorkSpace)
                            ELSE
                                ! Reverse into a desired order.
                                II = Start + 1
                                JJ = Finish
                                CALL Reverse_Order_Base0(A, II, JJ)
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
                                    ! Reverse into a desired order.
                                    I = Last
                                    J = KFirstEqual - 1
                                    CALL Reverse_Order_Base0(A, I, J)
                                    ! Allocate a buffer to use as scratch memory.
                                    ALLOCATE(WorkSpace(1:CurrRunSize/2), MOLD=A)
                                    ! perform Rust's merge sort algorithm
                                    CALL Rust_Merge_Stable(A(Last:K-1), CurrRunSize, TrueFirstDescendRunSize, TrueVal, WorkSpace)
                                    DEALLOCATE(WorkSpace)
                                ELSE
                                    ! Reverse into a desired order.
                                    I = Last
                                    J = K - 1
                                    CALL Reverse_Order_Base0(A, I, J)
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
                                    ! Reverse into a desired order.
                                    I = Last
                                    J = KFirstEqual - 1
                                    CALL Reverse_Order_Base0(A, I, J)
                                    ! Allocate a buffer to use as scratch memory.
                                    ALLOCATE(WorkSpace(1:CurrRunSize/2), MOLD=A)
                                    ! perform Rust's merge sort algorithm
                                    CALL Rust_Merge_Stable(A(Last:K-1), CurrRunSize, TrueFirstDescendRunSize, TrueVal, WorkSpace)
                                    DEALLOCATE(WorkSpace)
                                ELSE
                                    ! Reverse into a desired order.
                                    I = Last
                                    J = K - 1
                                    CALL Reverse_Order_Base0(A, I, J)
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
                                ! Reverse into a desired order.
                                I = Last
                                J = KFirstEqual - 1
                                CALL Reverse_Order_Base0(A, I, J)
                                ! Allocate a buffer to use as scratch memory.
                                ALLOCATE(WorkSpace(1:CurrRunSize/2), MOLD=A)
                                ! perform Rust's merge sort algorithm
                                CALL Rust_Merge_Stable(A(Last:K-1), CurrRunSize, TrueFirstDescendRunSize, TrueVal, WorkSpace)
                                DEALLOCATE(WorkSpace)
                            ELSE
                                ! Reverse into a desired order.
                                I = Last
                                J = K - 1
                                CALL Reverse_Order_Base0(A, I, J)
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
                                    ! Reverse into a desired order.
                                    I = Last
                                    J = KFirstEqual - 1
                                    CALL Reverse_Order_Base0(A, I, J)
                                    ! Allocate a buffer to use as scratch memory.
                                    ALLOCATE(WorkSpace(1:CurrRunSize/2), MOLD=A)
                                    ! perform Rust's merge sort algorithm
                                    CALL Rust_Merge_Stable(A(Last:K-1), CurrRunSize, TrueFirstDescendRunSize, TrueVal, WorkSpace)
                                    DEALLOCATE(WorkSpace)
                                ELSE
                                    ! Reverse into a desired order.
                                    I = Last
                                    J = K - 1
                                    CALL Reverse_Order_Base0(A, I, J)
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
                    ! Reverse into a desired order.
                    I = Last
                    J = KFirstEqual - 1
                    CALL Reverse_Order_Base0(A, I, J)
                    ! Allocate a buffer to use as scratch memory.
                    ALLOCATE(WorkSpace(1:CurrRunSize/2), MOLD=A)
                    ! perform Rust's merge sort algorithm
                    CALL Rust_Merge_Stable(A(Last:K-1), CurrRunSize, TrueFirstDescendRunSize, TrueVal, WorkSpace)
                    DEALLOCATE(WorkSpace)
                ELSE
                    ! Reverse into a desired order.
                    I = Last
                    J = K - 1
                    CALL Reverse_Order_Base0(A, I, J)
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

    !**************************************************************************

    RECURSIVE SUBROUTINE Quick_Stable(AVal, BVal, LStart, REnd)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To sort the array in the range [LStart,REnd] in a desired order
        ! using recursive Quicksort algorithm.  If the number of elements in
        ! the range below the cutoff, then a pair-insertion sorting routine
        ! is used instead.  If maximum depth is zero, the ninther is used to
        ! select the pivot instead of the median of three.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: AVal(:)  ! values of record items
        tArgument, INTENT(INOUT)    :: BVal(:)  ! an auxiliary array
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
            CALL Insert_Guarded(AVal, LStart, REnd)
            RETURN
        END IF

        ! perform partitioning
        CALL Partition_Stable(AVal, BVal, LStart, REnd, LEnd, RStart)

        ! perform quick sort on both sub-arrays
        CALL Quick_Stable(AVal, BVal, LStart, LEnd)
        CALL Quick_Stable(AVal, BVal, RStart, REnd)

        RETURN

    END SUBROUTINE Quick_Stable

    !**************************************************************************

    RECURSIVE SUBROUTINE Quick_Recur_Sort(AVal, BVal, LStart, REnd)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To sort the array in the range [LStart,REnd] in a desired order
        ! using recursive quick-merge sort algorithm.  If the number of elements in
        ! the range below the cutoff, then a pair-insertion sorting routine
        ! is used instead.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: AVal(:)  ! values of record items
        tArgument, INTENT(INOUT)    :: BVal(:)  ! an auxiliary array
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
            CALL Insert_Guarded(AVal, LStart, REnd)
            RETURN
        END IF

        ! perform stable partitioning
        CALL Partition_Stable(AVal, BVal, LStart, REnd, LEnd, RStart)

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
            CALL MergeSort_HalfCopy(AVal, LStart-1, LEnd, BVal)

            ! perform quick sort on right (larger) sub-array
            CALL Quick_Recur_Sort(AVal, BVal, RStart, REnd)

        ELSE

            ! set cutoff for merge sort
            IF (LenR < 2000) THEN
                MCutOff = MergeInsertion_CutOff_Low
            ELSE
                MCutOff = MergeInsertion_CutOff_High
            END IF

            ! perform merge sort on right (smaller) sub-array
            CALL MergeSort_HalfCopy(AVal, RStart-1, REnd, BVal)

            ! perform quick sort on left (larger) sub-array
            CALL Quick_Recur_Sort(AVal, BVal, LStart, LEnd)

        END IF

        RETURN

    END SUBROUTINE Quick_Recur_Sort

    !**************************************************************************

    SUBROUTINE Quick_Iter_Sort(AVal, BVal, Left, Right)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To sort the array in the range [LStart,REnd] in a desired order
        ! using iterative quick-merge sort algorithm.  If the number of elements in
        ! the range below the cutoff, then a pair-insertion sorting routine
        ! is used instead.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: AVal(:)  ! values of record items
        tArgument, INTENT(INOUT)    :: BVal(:)  ! an auxiliary array
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
                CALL Insert_Guarded(AVal, LStart, REnd)
                EXIT
            END IF

            ! perform partitioning
            CALL Partition_Stable(AVal, BVal, LStart, REnd, LEnd, RStart)

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
                CALL MergeSort_HalfCopy(AVal, LStart-1, LEnd, BVal)
                LStart = RStart
            ELSE
                ! set cutoff for merge sort
                IF (LenR < 2000) THEN
                    MCutOff = MergeInsertion_CutOff_Low
                ELSE
                    MCutOff = MergeInsertion_CutOff_High
                END IF
                ! perform merge sort on right (smaller) sub-array
                CALL MergeSort_HalfCopy(AVal, RStart-1, REnd, BVal)
                REnd = LEnd
            END IF

        END DO

        RETURN

    END SUBROUTINE Quick_Iter_Sort

    !**************************************************************************

    SUBROUTINE Partition_Stable(AVal, BVal, LStart, REnd, LEnd, RStart)

    !** PURPOSE OF THIS SUBROUTINE
        ! To partition an array using stable partitioning scheme.
        ! The given array in the range [LStart,REnd] is partitioned
        ! into two sub-arrays, Left and Right, such that all elements
        ! in the Left sub-array in the range [LStart,LEnd] are less
        ! than or equal to the selected pivot, and all elements in
        ! the Right sub-array in the range [RStart,REnd] are greater
        ! than or equal to the selected pivot.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: AVal(:)  ! values of array to be partitioned
        tArgument, INTENT(INOUT)    :: BVal(:)  ! values of buffer array
        tIndex,    INTENT(IN)       :: LStart   ! on entry, the start of the given array
                                                ! on exit, the start of the Left sub-array
        tIndex,    INTENT(IN)       :: REnd     ! on entry, the end of the given array
                                                ! on exit, the end of the Right sub-array
        tIndex,    INTENT(OUT)      :: LEnd     ! the end of the Left sub-array
        tIndex,    INTENT(OUT)      :: RStart   ! the start of the Right sub-array

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tVarScalar  :: Pivot        ! pivot used to partition the array
        tIndex      :: Less         ! current pointer for value less than the pivot
        tIndex      :: Great        ! current pointer for value greater than the pivot
        tIndex      :: Equal        ! current pointer for value equal to the pivot
        tIndex      :: Indx         ! index
        tIndex      :: BSize        ! size of BVal array
        tIndex      :: N, M1, M2, M3
        tIndex      :: Eps, Ninther

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Pivot, MOLD=A(LStart))
#endif

        N = REnd - LStart + 1

        ! use Tukey's ninther as partitioning element (the pivot)
        ! also, rearrange so that A(LStart) < A(LStart+1) < A(REnd)
        Eps = N/8
        M1 = LStart + Eps
        M2 = LStart + N/2
        M3 = REnd - Eps
        M1 = Median_Of_Three(AVal, LStart, M1, M1+Eps)
        M2 = Median_Of_Three(AVal, M2-Eps, M2, M2+Eps)
        M3 = Median_Of_Three(AVal, M3-Eps, M3, REnd)
        Ninther = Median_Of_Three(AVal, M1, M2, M3)
        Pivot = AVal(Ninther)

        ! initialize greater index
        BSize = SIZE(BVal)
        Less  = LStart - 1
        Equal = BSize + 1
        Great = 0

        ! perform supposedly-stable partition of the array
        DO Indx = LStart, REnd
            ! check whether the current element is greater in value than our pivot value.
            ! If so, save it to the buffer.  Otherwise, move it up front
            IF (COMPARE_GLT(Pivot, AVal(Indx))) THEN
                ! save greater value to the front of the buffer
                Great = Great + 1
                BVal(Great) = AVal(Indx)
            ELSEIF (COMPARE_GLT(AVal(Indx), Pivot)) THEN
                ! move less value to the front of the array
                Less = Less + 1
                IF (Less < Indx) AVal(Less) = AVal(Indx)
            ELSE
                ! save equal value to the back of the buffer
                Equal = Equal - 1
                BVal(Equal) = AVal(Indx)
            END IF
        END DO

        ! set maximum index of sub-arrays with less values
        LEnd = Less

        ! transfer data back from the buffer
        IF (Equal <= BSize) THEN
            ! first, transfer data with equal values
            DO Indx = BSize, Equal, -1
                Less = Less + 1
                AVal(Less) = BVal(Indx)
            END DO
        END IF
        Less = Less + 1
        IF (Great >= 1) THEN
            ! next, transfer data with greater values
            AVal(Less:REnd) = BVal(1:Great)
        END IF
        ! set minimum index of sub-arrays with greater values
        RStart = Less

        RETURN

    END SUBROUTINE Partition_Stable

    !**************************************************************************

    SUBROUTINE MergeSort_HalfCopy(A, P, K, B)

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
        tArgument, INTENT(INOUT)    :: A(0:)    ! working array
        tIndex,    INTENT(IN)       :: P        ! the start (inclusive) of the range of array to be sorted
        tIndex,    INTENT(IN)       :: K        ! the end (exclusive) of the range of array to be sorted
        tArgument, INTENT(INOUT)    :: B(0:)    ! working array

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: NE, S, T, KMS

    !** FLOW:

        NE = K - P
        IF (NE > MCutOff) THEN
            ! split the array into halves.
            S = P + NE/2
            T = K-(S-P)
            ! CopyMerge(A2, B)
            CALL HalfCopy_Split_Merge(A, S, K, B, 0)
            ! CopyMerge(A1, A2)
            CALL HalfCopy_Split_Merge(A, P, S, A, T)

            KMS = K - S
            IF (COMPARE_GLE(B(KMS-1), A(T))) THEN
                ! just copy array B to the first half of array A
                A(T-KMS:T-1) = B(0:KMS-1)
            ELSE
                ! merge the resulting sub-arrays
                ! MergeR(B, A2, A)
                CALL MergingR_HalfCopy(B, 0, KMS, A, T, K)
            END IF
        ELSE
            CALL Insert_Guarded(A, P+1, K)
        END IF

        RETURN

    END SUBROUTINE MergeSort_HalfCopy

    !**************************************************************************

    RECURSIVE SUBROUTINE HalfCopy_Split_Merge(A, P, K, B, T)

    !** PURPOSE OF THIS SUBROUTINE
        ! Based on the half-copying algorithm, a very efficient copying version
        ! of Mergesort may be written sorting the elements form the range [P,K)
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
        tArgument, INTENT(INOUT)    :: A(0:)    ! working array
        tIndex,    INTENT(IN)       :: P        ! the start (inclusive) of the range of array to be sorted
        tIndex,    INTENT(IN)       :: K        ! the end (exclusive) of the range of array to be sorted
        tArgument, INTENT(INOUT)    :: B(0:)    ! working array
        tIndex,    INTENT(IN)       :: T        ! the start (inclusive) of the range of array to be sorted

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: NE, S, S1, T1

    !** FLOW:

        NE = K - P
        IF (NE > MCutOff) THEN
            ! split the array into halves.
            S = P + (K-P)/2
            ! CopyMerge(A2, B2)
            CALL HalfCopy_Split_Merge(A, S, K, B, T+(S-P))
            ! CopyMerge(A1, B2)
            CALL HalfCopy_Split_Merge(A, P, S, A, S)

            S1 = S + (S-P)
            T1 = T+(S-P)
            IF (COMPARE_GLE(A(S1-1), B(T1))) THEN
                ! just copy array A to the first half of array B
                B(T1-(S1-S):T1-1) = A(S:S1-1)
            ELSE
                ! merge the resulting sub-arrays
                ! Merge(A2, B2, B)
                CALL Merging_HalfCopy(A, S, S1, B, T1, T+(K-P))
            END IF
        ELSE
            ! copy data from A to B
            B(T:T+NE-1) = A(P:K-1)

            ! perform pair-insertion sort on B
            CALL Insert_Guarded(B, T+1, T+NE)
        END IF

        RETURN

    END SUBROUTINE HalfCopy_Split_Merge

    !**************************************************************************

    RECURSIVE SUBROUTINE Quad_Split_Merge(A, Left, Right, B, CutOff)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To split A into 4 sub-arrays and sort them into B, and
        ! then merge the four sub-arrays from B to A.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(0:)    ! array to be sorted
        tIndex,    INTENT(IN)       :: Left     ! the start of the range
        tIndex,    INTENT(IN)       :: Right    ! the end of the range
        tArgument, INTENT(INOUT)    :: B(0:)    ! buffer (auxiliary array)
        tIndex,    INTENT(IN)       :: CutOff   ! cutoff to pair insertion sort

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
            CALL Quad_Split_Merge(A, Left,     LeftMid,  B, CutOff)
            CALL Quad_Split_Merge(A, LeftMid,  Mid,      B, CutOff)
            CALL Quad_Split_Merge(A, Mid,      RightMid, B, CutOff)
            CALL Quad_Split_Merge(A, RightMid, Right,    B, CutOff)

            IF (COMPARE_GLE(A(LeftMid-1), A(LeftMid))) THEN
                ! merely copy the resulting sub-arrays into array B
                B(Left:Mid-1) = A(Left:Mid-1)
            ELSE
                ! merge the resulting sub-arrays into array B
                CALL Merging_Wiki(A, Left, LeftMid, Mid, B)
            END IF
            IF (COMPARE_GLE(A(RightMid-1), A(RightMid))) THEN
                ! merely copy the resulting sub-arrays into array B
                B(Mid:Right-1) = A(Mid:Right-1)
            ELSE
                ! merge the resulting sub-arrays into array B
                CALL Merging_Wiki(A, Mid, RightMid, Right, B)
            END IF

            IF (COMPARE_GLE(B(Mid-1), B(Mid))) THEN
                ! array B has already been sorted so just copy it to array A
                A(Left:Right-1) = B(Left:Right-1)
            ELSE
                ! merge the sub-arrays of array B into array A
                CALL Merging_Wiki(B, Left, Mid, Right, A)
            END IF
        ELSE
            CALL Insert_Guarded(A, Left+1, Right)
        END IF

        RETURN

    END SUBROUTINE Quad_Split_Merge

    !**************************************************************************

END SUBROUTINE Wise_Sort_Stable

!------------------------------------------------------------------------------
!                   SORTING ENGINE ROUTINES
!------------------------------------------------------------------------------

RECURSIVE FUNCTION Java_Merge_Runs(A, B, Offset, Aim, Run, Low, High) RESULT(DstPtr)

!** PURPOSE OF THIS SUBROUTINE
    ! To merge the specified runs.

!** REFERENCE
    ! The routine is based on a part of Java's sorting algorithm.
    ! OpenJDK's Java codes for Java's sorting algorithm can be obtained from
    ! https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/util/DualPivotQuicksort.java.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(0:)    ! source array
    tArgument, INTENT(INOUT)    :: B(0:)    ! buffer array used in merging
    tIndex,    INTENT(IN)       :: Offset   ! starting index in the source (inclusive)
    tIndex,    INTENT(IN)       :: Aim      ! flag specifying how to merge:
                                            ! to source ( > 0), buffer ( < 0) or any ( == 0)
    tIndex,    INTENT(IN)       :: Run(0:)  ! start indices of the runs (inclusive)
    tIndex,    INTENT(IN)       :: Low      ! start index of the first run (inclusive)
    tIndex,    INTENT(IN)       :: High     ! start index of the last run (inclusive)
    tIndex                      :: DstPtr   ! destination pointer where runs are merged
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
                B(J) = A(I)
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
    A1Ptr = Java_Merge_Runs(A, B, Offset, -Aim, Run, Lo, Mi)
    A2Ptr = Java_Merge_Runs(A, B, Offset,    0, Run, Mi, Hi)

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
            CALL MergeParts(A, K, A, Lo1, Hi1, A, Lo2, Hi2, A1_Assoc, A2_Assoc)
        ELSEIF ((A1Ptr == 1).AND.(A2Ptr == -1)) THEN
            CALL MergeParts(A, K, A, Lo1, Hi1, B, Lo2, Hi2, A1_Assoc, A2_Assoc)
        ELSEIF ((A1Ptr == -1).AND.(A2Ptr == 1)) THEN
            CALL MergeParts(A, K, B, Lo1, Hi1, A, Lo2, Hi2, A1_Assoc, A2_Assoc)
        ELSEIF ((A1Ptr == -1).AND.(A2Ptr == -1)) THEN
            CALL MergeParts(A, K, B, Lo1, Hi1, B, Lo2, Hi2, A1_Assoc, A2_Assoc)
        ELSE
            CALL Handle_ErrLevel('Java_Merge_Runs', ModName, ErrWarning, 'A1 and A2 are NULL.')
        END IF
    ELSEIF (DstPtr == -1) THEN
        IF ((A1Ptr == 1).AND.(A2Ptr == 1)) THEN
            CALL MergeParts(B, K, A, Lo1, Hi1, A, Lo2, Hi2, A1_Assoc, A2_Assoc)
        ELSEIF ((A1Ptr == 1).AND.(A2Ptr == -1)) THEN
            CALL MergeParts(B, K, A, Lo1, Hi1, B, Lo2, Hi2, A1_Assoc, A2_Assoc)
        ELSEIF ((A1Ptr == -1).AND.(A2Ptr == 1)) THEN
            CALL MergeParts(B, K, B, Lo1, Hi1, A, Lo2, Hi2, A1_Assoc, A2_Assoc)
        ELSEIF ((A1Ptr == -1).AND.(A2Ptr == -1)) THEN
            CALL MergeParts(B, K, B, Lo1, Hi1, B, Lo2, Hi2, A1_Assoc, A2_Assoc)
        ELSE
            CALL Handle_ErrLevel('Java_Merge_Runs', ModName, ErrWarning, 'A1 and A2 are NULL.')
        END IF
    ELSE
        CALL Handle_ErrLevel('Java_Merge_Runs', ModName, ErrWarning, &
                   'DstPtr = 0.  This should not happen.')
    END IF

    RETURN

    CONTAINS

    SUBROUTINE MergeParts(Dest, Kin, A1, Low1, High1, A2, Low2, High2, A1_Assoc, A2_Assoc)

    !** PURPOSE OF THIS SUBROUTINE
        ! To merge the sorted parts.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT) :: Dest(0:) ! the destination where parts are merged
        tIndex,    INTENT(IN)    :: Kin      ! the start index of the destination, inclusive
        tArgument, INTENT(IN)    :: A1(0:)   ! the first part
        tIndex,    INTENT(IN)    :: Low1     ! the start index of the first part, inclusive
        tIndex,    INTENT(IN)    :: High1    ! the end index of the first part, exclusive
        tArgument, INTENT(IN)    :: A2(0:)   ! the second part
        tIndex,    INTENT(IN)    :: Low2     ! the start index of the second part, inclusive
        tIndex,    INTENT(IN)    :: High2    ! the end index of the second part, exclusive
        tLogical,  INTENT(IN)    :: A1_Assoc ! true if Dest is associated with A1
        tLogical,  INTENT(IN)    :: A2_Assoc ! true if Dest is associated with A2

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex  :: Lo1, Lo2, Hi1, Hi2, K

    !** FLOW:

        ! initialize
        K = Kin
        Lo1 = Low1
        Hi1 = High1
        Lo2 = Low2
        Hi2 = High2

        ! Merge parts sequentially.
        DO WHILE ((Lo1 < Hi1).AND.(Lo2 < Hi2))
            IF (COMPARE_GLT(A1(Lo1), A2(Lo2))) THEN
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
                Dest(K) = A1(Lo1)
                K = K + 1
                Lo1 = Lo1 + 1
            END DO
        END IF

        IF ((.NOT.A2_Assoc).OR.(K < Lo2)) THEN
            DO WHILE (Lo2 < Hi2)
                Dest(K) = A2(Lo2)
                K = K + 1
                Lo2 = Lo2 + 1
            END DO
        END IF

        RETURN

    END SUBROUTINE MergeParts

    !**************************************************************************

END FUNCTION Java_Merge_Runs

!******************************************************************************

SUBROUTINE Rust_Merge_Sort(A, ALen, LongRunSize, FirstRun, B)

!** PURPOSE OF THIS SUBROUTINE
    ! To sort an array in a desired order using Rust's merge sort
    ! that borrows some (but not all) of the ideas from TimSort.

!** REFERENCES
    ! This routine is a modified version of the 'Rust_Sort' subroutine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(0:)
    tIndex,    INTENT(IN)       :: ALen
    tIndex,    INTENT(IN)       :: LongRunSize
    tLogical,  INTENT(IN)       :: FirstRun     ! true if long run size is of the first run
                                                ! otherwise, long run size is of the last run
    tArgument, INTENT(INOUT)    :: B(0:)

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! The maximum number of entries in a run stack, good for an array of 2**64 elements.
    tInteger, PARAMETER :: Max_Merge_Stack = INT(CEILING(LOG(2.0_kFP**64) / &
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
        CALL Rust_Merge_Sort_LongFirst(A, ALen, LongRunSize, Min_Run, B)
    ELSE
        CALL Rust_Merge_Sort_LongLast(A, ALen, LongRunSize, Min_Run, B)
    END IF

    RETURN

    CONTAINS

    SUBROUTINE Rust_Merge_Sort_LongFirst(A, ALen, FirstRunSize, Min_Run, B)

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
        tArgument, INTENT(INOUT)    :: A(0:)
        tIndex,    INTENT(IN)       :: ALen
        tIndex,    INTENT(IN)       :: FirstRunSize
        tIndex,    INTENT(IN)       :: Min_Run
        tArgument, INTENT(INOUT)    :: B(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Finish, nRun, R, RCount, Start
        tIndex      :: RunLen(0:Max_Merge_Stack-1)
        tIndex      :: RunBase(0:Max_Merge_Stack-1)
        tIndex      :: LeftLen, LeftBase
        tIndex      :: RightLen, RightBase
        tIndex      :: Lo, Hi, I
        tVarScalar  :: Temp

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Temp, MOLD=A(0))
#endif

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
                IF (COMPARE_GLT(A(Start+1), A(Start))) THEN
                    Descending: DO WHILE (Start > 0)
                        IF (COMPARE_GLE(A(Start-1), A(Start))) EXIT Descending
                        Start = Start - 1
                    END DO Descending
                    ! reverse the segment into a desired order
                    Lo = Start
                    Hi = Finish
                    CALL Reverse_Order_Base0(A, Lo, Hi)
                ELSE
                    Ascending: DO WHILE(Start > 0)
                        IF (COMPARE_GLT(A(Start), A(Start-1))) EXIT Ascending
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
                    CALL DualInsert_Guarded(A, Start+1, Finish+1)
                ELSE
                    ! nRun is long enough, use insertion sort
                    Insert_Loop: DO WHILE (Start > 0)
                        IF ((Finish-Start) >= (Min_Run-1)) EXIT Insert_Loop
                        Start = Start - 1
                        ! perform insertion sort by inserting A(Start) into the pre-sorted
                        ! sequence A(Start+1:Finish) so that the whole A(Start:Finish)
                        ! becomes sorted.
                        Temp = A(Start)
                        Find_Hole: DO I = Start+1, Finish
                            IF (COMPARE_GLE(Temp, A(I))) EXIT Find_Hole
                            A(I-1) = A(I)
                        END DO Find_Hole
                        A(I-1) = Temp
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
                CALL Merging(A(LeftBase:RightBase+RightLen-1), LeftLen, B )

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

    !**************************************************************************

    SUBROUTINE Rust_Merge_Sort_LongLast(A, ALen, LastRunSize, Min_Run, B)

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
        tArgument, INTENT(INOUT)    :: A(0:)
        tIndex,    INTENT(IN)       :: ALen
        tIndex,    INTENT(IN)       :: LastRunSize
        tIndex,    INTENT(IN)       :: Min_Run
        tArgument, INTENT(INOUT)    :: B(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Finish, nRun, R, RCount, Start
        tIndex      :: RunLen(0:Max_Merge_Stack-1)
        tIndex      :: RunBase(0:Max_Merge_Stack-1)
        tIndex      :: LeftLen, LeftBase
        tIndex      :: RightLen, RightBase
        tIndex      :: Lo, Hi, I
        tVarScalar  :: Temp

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Temp, MOLD=A(0))
#endif

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
                IF (COMPARE_GLT(A(Start+1), A(Start))) THEN
                    Descending: DO WHILE (Start > 0)
                        IF (COMPARE_GLE(A(Start-1), A(Start))) EXIT Descending
                        Start = Start - 1
                    END DO Descending
                    ! reverse the segment into a desired order
                    Lo = Start
                    Hi = Finish
                    CALL Reverse_Order_Base0(A, Lo, Hi)
                ELSE
                    Ascending: DO WHILE(Start > 0)
                        IF (COMPARE_GLT(A(Start), A(Start-1))) EXIT Ascending
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
                    CALL DualInsert_Guarded(A, Start+1, Finish+1)
                ELSE
                    ! nRun is long enough, use insertion sort
                    Insert_Loop: DO WHILE (Start > 0)
                        IF ((Finish-Start) >= (Min_Run-1)) EXIT Insert_Loop
                        Start = Start - 1
                        ! perform insertion sort by inserting A(Start) into the pre-sorted
                        ! sequence A(Start+1:Finish) so that the whole A(Start:Finish)
                        ! becomes sorted.
                        Temp = A(Start)
                        Find_Hole: DO I = Start+1, Finish
                            IF (COMPARE_GLE(Temp, A(I))) EXIT Find_Hole
                            A(I-1) = A(I)
                        END DO Find_Hole
                        A(I-1) = Temp
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
                CALL Merging(A(LeftBase:RightBase+RightLen-1), LeftLen, B )

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

    !**************************************************************************

    FUNCTION Collapse(Base, Length) RESULT (R)

    !** PURPOSE OF THIS SUBROUTINE
        ! To examine the stack of runs waiting to be merged, identifying adjacent runs
        ! to be merged until the stack invariants are reestablished:
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

    !**************************************************************************

    SUBROUTINE Merging(A, Mid, B)

    !** PURPOSE OF THIS SUBROUTINE
        ! To merge the two non-decreasing runs 'A(0:Mid-1)' and 'A(Mid:)'
        ! using 'B' as a temporary storage, and stores the merged runs into
        ! 'A(0:)'.  'Mid' must be > 0, and < 'SIZE(A)-1'. The buffer 'B'
        ! must be long enough to hold the shorter of the two runs.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(0:)
        tIndex,    INTENT(IN)       :: Mid
        tArgument, INTENT(INOUT)    :: B(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: ALen, I, J, K
        tVarScalar  :: Temp

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Temp, MOLD=A(0))
#endif

        ALen = SIZE(A)

        ! Merge first copies the shorter run into 'B'. Then, depending on which
        ! run was shorter, it traces the copied run and the longer run forwards
        ! (or backwards), comparing their next unprocessed elements and then
        ! copying the lesser (or greater) one into 'A'.

        IF (Mid <= (ALen - Mid)) THEN
            ! The left run is shorter.
            B(0:Mid-1) = A(0:Mid-1)
            I = 0
            J = Mid
            Merge_Lower: DO K = 0, ALen-1
                IF (COMPARE_GLE(B(I), A(J))) THEN
                    A(K) = B(I)
                    I = I + 1
                    IF (I >= Mid) EXIT Merge_Lower
                ELSE
                    A(K) = A(J)
                    J = J + 1
                    IF (J >= ALen) THEN
                        A(K+1:) = B(I:Mid-1)
                        EXIT Merge_Lower
                    END IF
                END IF
            END DO Merge_Lower
        ELSE
            ! The right run is shorter so check that it is stable
            B(0:ALen-Mid-1) = A(Mid:ALen-1)
            I = Mid - 1
            J = ALen - Mid -1
            Merge_Upper: DO K = ALen-1, 0, -1
                IF (COMPARE_GLE(A(I), B(J))) THEN
                    A(K) = B(J)
                    J = J - 1
                    IF (J < 0) EXIT Merge_Upper
                ELSE
                    A(K) = A(I)
                    I = I - 1
                    IF (I < 0) THEN
                        A(0:K-1) = B(0:J)
                        EXIT Merge_Upper
                    END IF
                END IF
            END DO Merge_Upper
        END IF

        RETURN

    END SUBROUTINE Merging

    !**************************************************************************

END SUBROUTINE Rust_Merge_Sort

!******************************************************************************

SUBROUTINE Rust_Merge_Stable(A, ALen, LongRunSize, FirstRun, B)

!** PURPOSE OF THIS SUBROUTINE
    ! To sort an array in a desired order using Rust's merge sort
    ! that borrows some (but not all) of the ideas from TimSort.

!** REFERENCES
    ! This routine is a modified version of the 'Rust_Sort' subroutine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(0:)
    tIndex,    INTENT(IN)       :: ALen
    tIndex,    INTENT(IN)       :: LongRunSize
    tLogical,  INTENT(IN)       :: FirstRun     ! true if long run size is of the first run
                                                ! otherwise, long run size is of the last run
    tArgument, INTENT(INOUT)    :: B(0:)

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! The maximum number of entries in a run stack, good for an array of 2**64 elements.
    tInteger, PARAMETER :: Max_Merge_Stack = INT(CEILING(LOG(2.0_kFP**64) / &
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
        CALL Rust_Merge_Sort_LongFirst(A, ALen, LongRunSize, Min_Run, B)
    ELSE
        CALL Rust_Merge_Sort_LongLast(A, ALen, LongRunSize, Min_Run, B)
    END IF

    RETURN

    CONTAINS

    SUBROUTINE Rust_Merge_Sort_LongFirst(A, ALen, FirstRunSize, Min_Run, B)

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
        !
        ! The invariants ensure that the total running time is 'O(n log n)' worst-case.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(0:)
        tIndex,    INTENT(IN)       :: ALen
        tIndex,    INTENT(IN)       :: FirstRunSize
        tIndex,    INTENT(IN)       :: Min_Run
        tArgument, INTENT(INOUT)    :: B(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Finish, nRun, R, RCount, Start
        tIndex      :: RunLen(0:Max_Merge_Stack-1)
        tIndex      :: RunBase(0:Max_Merge_Stack-1)
        tIndex      :: LeftLen, LeftBase
        tIndex      :: RightLen, RightBase
        tIndex      :: Lo, Hi, I
        tVarScalar  :: Temp

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Temp, MOLD=A(0))
#endif

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
                IF (COMPARE_GLT(A(Start+1), A(Start))) THEN
                    Descending: DO WHILE (Start > 0)
                        IF (COMPARE_GLE(A(Start-1), A(Start))) EXIT Descending
                        Start = Start - 1
                    END DO Descending
                    ! reverse the segment into a desired order
                    Lo = Start
                    Hi = Finish
                    CALL Reverse_Order_Base0(A, Lo, Hi)
                ELSE
                    Ascending: DO WHILE(Start > 0)
                        IF (COMPARE_GLT(A(Start), A(Start-1))) EXIT Ascending
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
                    CALL Insert_Guarded(A, Start+1, Finish+1)
                ELSE
                    ! nRun is long enough, use insertion sort
                    Insert_Loop: DO WHILE (Start > 0)
                        IF ((Finish-Start) >= (Min_Run-1)) EXIT Insert_Loop
                        Start = Start - 1
                        ! perform insertion sort by inserting A(Start) into the pre-sorted
                        ! sequence A(Start+1:Finish) so that the whole A(Start:Finish)
                        ! becomes sorted.
                        Temp = A(Start)
                        Find_Hole: DO I = Start+1, Finish
                            IF (COMPARE_GLE(Temp, A(I))) EXIT Find_Hole
                            A(I-1) = A(I)
                        END DO Find_Hole
                        A(I-1) = Temp
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
                CALL Merging(A(LeftBase:RightBase+RightLen-1), LeftLen, B )

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

    !**************************************************************************

    SUBROUTINE Rust_Merge_Sort_LongLast(A, ALen, LastRunSize, Min_Run, B)

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
        tArgument, INTENT(INOUT)    :: A(0:)
        tIndex,    INTENT(IN)       :: ALen
        tIndex,    INTENT(IN)       :: LastRunSize
        tIndex,    INTENT(IN)       :: Min_Run
        tArgument, INTENT(INOUT)    :: B(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Finish, nRun, R, RCount, Start
        tIndex      :: RunLen(0:Max_Merge_Stack-1)
        tIndex      :: RunBase(0:Max_Merge_Stack-1)
        tIndex      :: LeftLen, LeftBase
        tIndex      :: RightLen, RightBase
        tIndex      :: Lo, Hi, I
        tVarScalar  :: Temp

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Temp, MOLD=A(0))
#endif

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
                IF (COMPARE_GLT(A(Start+1), A(Start))) THEN
                    Descending: DO WHILE (Start > 0)
                        IF (COMPARE_GLE(A(Start-1), A(Start))) EXIT Descending
                        Start = Start - 1
                    END DO Descending
                    ! reverse the segment into a desired order
                    Lo = Start
                    Hi = Finish
                    CALL Reverse_Order_Base0(A, Lo, Hi)
                ELSE
                    Ascending: DO WHILE(Start > 0)
                        IF (COMPARE_GLT(A(Start), A(Start-1))) EXIT Ascending
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
                    CALL Insert_Guarded(A, Start+1, Finish+1)
                ELSE
                    ! nRun is long enough, use insertion sort
                    Insert_Loop: DO WHILE (Start > 0)
                        IF ((Finish-Start) >= (Min_Run-1)) EXIT Insert_Loop
                        Start = Start - 1
                        ! perform insertion sort by inserting A(Start) into the pre-sorted
                        ! sequence A(Start+1:Finish) so that the whole A(Start:Finish)
                        ! becomes sorted.
                        Temp = A(Start)
                        Find_Hole: DO I = Start+1, Finish
                            IF (COMPARE_GLE(Temp, A(I))) EXIT Find_Hole
                            A(I-1) = A(I)
                        END DO Find_Hole
                        A(I-1) = Temp
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
                CALL Merging(A(LeftBase:RightBase+RightLen-1), LeftLen, B )

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

    !**************************************************************************

    FUNCTION Collapse(Base, Length) RESULT (R)

    !** PURPOSE OF THIS SUBROUTINE
        ! To examine the stack of runs waiting to be merged, identifying adjacent runs
        ! to be merged until the stack invariants are restablished:
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

    !**************************************************************************

    SUBROUTINE Merging(A, Mid, B)

    !** PURPOSE OF THIS SUBROUTINE
        ! To merge the two non-decreasing runs 'A(0:Mid-1)' and 'A(Mid:)'
        ! using 'B' as a temporary storage, and stores the merged runs into
        ! 'A(0:)'.  'Mid' must be > 0, and < 'SIZE(A)-1'. The buffer 'B'
        ! must be long enough to hold the shorter of the two runs.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(0:)
        tIndex,    INTENT(IN)       :: Mid
        tArgument, INTENT(INOUT)    :: B(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: ALen, I, J, K
        tVarScalar  :: Temp

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Temp, MOLD=A(0))
#endif

        ALen = SIZE(A)

        ! Merge first copies the shorter run into 'B'. Then, depending on which
        ! run was shorter, it traces the copied run and the longer run forwards
        ! (or backwards), comparing their next unprocessed elements and then
        ! copying the lesser (or greater) one into 'A'.

        IF (Mid <= (ALen - Mid)) THEN
            ! The left run is shorter.
            B(0:Mid-1) = A(0:Mid-1)
            I = 0
            J = Mid
            Merge_Lower: DO K = 0, ALen-1
                IF (COMPARE_GLE(B(I), A(J))) THEN
                    A(K) = B(I)
                    I = I + 1
                    IF (I >= Mid) EXIT Merge_Lower
                ELSE
                    A(K) = A(J)
                    J = J + 1
                    IF (J >= ALen) THEN
                        A(K+1:) = B(I:Mid-1)
                        EXIT Merge_Lower
                    END IF
                END IF
            END DO Merge_Lower
        ELSE
            ! The right run is shorter so check that it is stable
            B(0:ALen-Mid-1) = A(Mid:ALen-1)
            I = Mid - 1
            J = ALen - Mid -1
            Merge_Upper: DO K = ALen-1, 0, -1
                IF (COMPARE_GLE(A(I), B(J))) THEN
                    A(K) = B(J)
                    J = J - 1
                    IF (J < 0) EXIT Merge_Upper
                ELSE
                    A(K) = A(I)
                    I = I - 1
                    IF (I < 0) THEN
                        A(0:K-1) = B(0:J)
                        EXIT Merge_Upper
                    END IF
                END IF
            END DO Merge_Upper
        END IF

        RETURN

    END SUBROUTINE Merging

    !**************************************************************************

END SUBROUTINE Rust_Merge_Stable

!------------------------------------------------------------------------------
!                      HYBRID SORT ROUTINES
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!                   NON-STABLE DRIVER ROUTINES
!------------------------------------------------------------------------------

MODULE SUBROUTINE Intro_Sort(A)

!^ **PURPOSE OF THIS SUBROUTINE**: <br>
!  To sort an array in a desired order using the *IntroSort* algorithm [1],
!  which is a hybrid algorithm employing the QuickSort (with median-of-three
!  partitioning scheme), the heapsort and the pair-insertion sort. <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] <a href="https://web.archive.org/web/20230307185457/http://www.cs.rpi.edu/~musser/gp/introsort.ps">
!       Musser, D.R. 1997.  Introspective Sorting and Selection Algorithms.
!       Software: Practice and Experience. 27(8):983-993. </a> <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(:)     !! array to be sorted

!** SUBROUTINE PARAMETER DECLARATIONS:
    tIndex, PARAMETER   :: Insertion_CutOff_Low  = 40_kIndex    ! cutoff to pair insertion sort
    tIndex, PARAMETER   :: Insertion_CutOff_Mid  = 50_kIndex    ! cutoff to pair insertion sort
    tIndex, PARAMETER   :: Insertion_CutOff_High = 70_kIndex    ! cutoff to pair insertion sort

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: CutOff       ! cutoff to pair insertion sort
    tIndex      :: MaxDepth     ! maximum depth used to switch partitioning routine
    tIndex      :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(A, KIND=kIndex)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Insertion_CutOff_Low) THEN
        CALL DualInsert_Guarded(A, 1_kIndex, NA)
        RETURN
    END IF

    ! set cutoff depending on the size of the input array
    IF (NA < 500_kIndex) THEN
        CutOff = Insertion_CutOff_Low
    ELSEIF (NA < 2000_kIndex) THEN
        CutOff = Insertion_CutOff_Mid
    ELSE
        CutOff = Insertion_CutOff_High
    END IF

    ! determine maximum depth based on the size of the input array
    MaxDepth = MaximumDepth(NA)

    ! perform quick sort
    CALL QuickSort(A, 1_kIndex, NA, CutOff, TrueVal, MaxDepth)

    RETURN

    CONTAINS

    RECURSIVE SUBROUTINE QuickSort(A,LStart,REnd,CutOff,LeftMost,MaxDepth)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To sort the array in the range [LStart,REnd] in a desired order
        ! using a recursive quicksort algorithm.  If the number of elements in
        ! the range below the cutoff, then a pair-insertion sorting routine
        ! is used instead.  If maximum depth is zero, the heapsort is utilized
        ! in place of the quicksort.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(:)     ! the array to be sorted
        tIndex,    INTENT(IN)       :: LStart   ! the start of the range
        tIndex,    INTENT(IN)       :: REnd     ! the end of the range
        tIndex,    INTENT(IN)       :: CutOff   ! cutoff to pair insertion sort
        tLogical,  INTENT(IN)       :: LeftMost ! flag indicating whether the specified range
                                                ! is the left most of the given array
                                                ! If false, an unguarded pair insertion sorting
                                                ! algorithm is used instead of the guarded one.
        tIndex,    INTENT(IN)       :: MaxDepth ! maximum depth used to switch partitioning routine

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: LEnd     ! the end of the left sub-array
        tIndex      :: RStart   ! the start of the right sub-array
        tIndex      :: NE       ! number of elements in the range

    !** FLOW:

        ! determine the number of elements in the specified range
        NE = REnd - LStart + 1

        ! check whether to perform pair insertion sort
        IF (NE <= CutOff) THEN
            ! check whether the specified range is the left most range of the given array
            IF (LeftMost) THEN
                CALL DualInsert_Guarded(A, LStart, REnd)
            ELSE
                CALL DualInsert_UnGuarded(A, LStart, REnd)
            END IF
            RETURN
        END IF

        IF (MaxDepth > 0) THEN
            ! perform partitioning
            CALL Partition_Mo3(A, LStart, REnd, LEnd, RStart)
        ELSE
            ! for worst-case situations, use heap sort instead
            CALL Heap_Sort(NE, A(LStart:REnd))
            RETURN
        END IF

        ! perform quick sort on the two sub-arrays
        CALL QuickSort(A, LStart, LEnd, CutOff, LeftMost, MaxDepth-1)
        CALL QuickSort(A, RStart, REnd, CutOff, FalseVal, MaxDepth-1)

        RETURN

    END SUBROUTINE QuickSort

    !******************************************************************************

    SUBROUTINE Partition_Mo3(A,LStart,REnd,LEnd,RStart)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To partition an array using median-of-three partitioning scheme.
        ! The given array in the range [LStart,REnd] is partitioned
        ! into two sub-arrays, Left and Right, such that all elements
        ! in the Left sub-array in the range [LStart,LEnd] are less
        ! than or equal to the selected pivot, and all elements in
        ! the Right sub-array in the range [RStart,REnd] are greater
        ! than or equal to the selected pivot.

    !** REFERENCES
        ! Adapted from subroutine 'Sort' of Numerical Recipe F90

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(:)     ! the array
        tIndex,    INTENT(IN)       :: LStart   ! on entry, the start of the given array
                                                ! on exit,  the start of the Left sub-array
        tIndex,    INTENT(IN)       :: REnd     ! on entry, the end of the given array
                                                ! on exit,  the end of the Right sub-array
        tIndex,    INTENT(OUT)      :: LEnd     ! the end of the Left sub-array
        tIndex,    INTENT(OUT)      :: RStart   ! the start of the Right sub-array

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tVarScalar  :: Pivot, Temp
        tIndex      :: Mid, LPtr, UPtr
        tIndex      :: I, J, K

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Pivot, MOLD=A(LStart))
        ALLOCATE(Temp, MOLD=A(LStart))
#endif

        ! choose median of left, center, and right elements as partitioning element (the pivot)
        ! also, rearrange so that A(LStart) < A(LStart+1) < A(REnd)
        Mid = LStart + (REnd-LStart) / 2
        CALL Sort_3_Items(A, LStart, Mid, REnd)
        EXCHANGE(A, Mid, LStart+1)

        ! set pivot value
        Pivot = A(LStart+1)

        ! set working pointers
        LPtr = LStart + 1
        UPtr = REnd

        DO
            ! increment the left pointer until we find an element
            ! that is greater than the pivot
            DO
                LPtr = LPtr + 1
                IF (COMPARE_GLE(Pivot, A(LPtr))) EXIT
            END DO

            ! decrement the right pointer until we find an element
            ! that is less than the pivot
            DO
                UPtr = UPtr - 1
                IF (COMPARE_GLE(A(UPtr), Pivot)) EXIT
            END DO

            ! exit if the pointers cross
            IF (UPtr < LPtr) EXIT

            ! swap A(LPtr) and A(UPtr)
            EXCHANGE(A, LPtr, UPtr)
        END DO

        ! insert partitioning element
        A(LStart+1) = A(UPtr)
        A(UPtr)     = Pivot

        ! set output indices
        LEnd   = UPtr - 1
        RStart = LPtr

        RETURN

    END SUBROUTINE Partition_Mo3

    !******************************************************************************

END SUBROUTINE Intro_Sort

!******************************************************************************

MODULE SUBROUTINE Java_Sort(A)

!^ **PURPOSE OF THIS SUBROUTINE**: <br>
!  To sort an array in a desired order using a Java's sorting algorithm [1],
!  which is a hybrid algorithm employing the dual-pivot quicksort as the main
!  algorithm.  The hybrid algorithm also utilizes a variant of the introsort
!  (where the heapsort and the mixed insertion sort are used) when the recursion
!  depth exceeds its limit.  In addition, the hybrid algorithm (with a routine
!  that inspects the pattern of the specified array) uses a variant of the
!  mergesort (a merging of runs) if the given array is highly structured. <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] <a href="https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/util/DualPivotQuicksort.java">
!       Java's DualPivotQuicksort class. </a> <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(:)   ! array to be sorted

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: NA   ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(A, KIND=kIndex)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! perform quick sort
    CALL Quick_DualPivot(A, 0_kIndex, 0_kIndex, NA)

    RETURN

CONTAINS

    RECURSIVE SUBROUTINE Quick_DualPivot(A, iBit, Lo, Hi)

    !** PURPOSE OF THIS SUBROUTINE
        ! To sort the specified array using the Dual-Pivot Quicksort and/or
        ! other sorts in special-cases, possibly with parallel partitions.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(0:)    ! the array to be sorted
        tIndex,    INTENT(IN)       :: iBit     ! the combination of recursion depth and bit flag, where
                                                ! the right bit "0" indicates that array is the leftmost part
        tIndex,    INTENT(IN)       :: Lo       ! the index of the first element, inclusive, to be sorted
        tIndex,    INTENT(IN)       :: Hi       ! the index of the last element, exclusive, to be sorted

    !** SUBROUTINE PARAMETER DECLARATIONS:
        ! Max array size to use mixed insertion sort.
        tInteger, PARAMETER :: MAX_MIXED_INSERTION_SORT_SIZE = 65
        ! Max array size to use insertion sort.
        tInteger, PARAMETER :: MAX_INSERTION_SORT_SIZE = 44
        ! Min array size to try merging of runs.
        tInteger, PARAMETER :: MIN_TRY_MERGE_SIZE = ISHFT(4, 10)
        ! Threshold of mixed insertion sort is incremented by this value.
        tInteger, PARAMETER :: DELTA = ISHFT(3, 1)
        ! Max recursive partitioning depth before using heap sort.
        tInteger, PARAMETER :: MAX_RECURSION_DEPTH = 64 * DELTA

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Bits, Low, High, iEnd, Size, Last
        tVarScalar  :: A3, AK, Temp
        tVarScalar  :: Pivot, Pivot1, Pivot2
        tIndex      :: Step
        tIndex      :: K
        tIndex      :: E1, E2, E3, E4, E5
        tIndex      :: Lower, Upper

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Pivot, MOLD=A(Lo))
        ALLOCATE(Pivot1, MOLD=A(Lo))
        ALLOCATE(Pivot2, MOLD=A(Lo))
        ALLOCATE(A3, MOLD=A(Lo))
        ALLOCATE(AK, MOLD=A(Lo))
        ALLOCATE(Temp, MOLD=A(Lo))
#endif

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
                CALL Insertion_Mixed(A, Low, Last, High)
                RETURN
            END IF

            ! Invoke insertion sort on small leftmost part.
            IF (Size < MAX_INSERTION_SORT_SIZE) THEN
                CALL Insertion_Java(A, Low, High)
                RETURN
            END IF

            ! Check if the whole array or large non-leftmost
            ! parts are nearly sorted and then merge runs.
            IF ((Bits == 0).OR.(Size > MIN_TRY_MERGE_SIZE).AND.(IAND(Bits, 1) > 0)) THEN
                IF (TryMergeRuns(A, Low, Size)) THEN
                    RETURN
                END IF
            END IF

            ! Switch to heap sort if execution
            ! time is becoming quadratic.
            Bits = Bits + DELTA
            IF (Bits > MAX_RECURSION_DEPTH) THEN
                CALL HeapSort_Java(A, Low, High)
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
            A3 = A(E3)

            ! Sort these elements in place by the combination
            ! of 4-element sorting network and insertion sort.
            !    5 ------o-----------o------------
            !            |           |
            !    4 ------|-----o-----o-----o------
            !            |     |           |
            !    2 ------o-----|-----o-----o------
            !                  |     |
            !    1 ------------o-----o------------
            IF (COMPARE_GLT(A(E5), A(E2))) THEN
                ! t = A(E5); A(E5) = A(E2); A(E2) = t
                EXCHANGE(A, E5, E2)
            END IF
            IF (COMPARE_GLT(A(E4), A(E1))) THEN
                ! t = A(E4); A(E4) = A(E1); A(E1) = t
                EXCHANGE(A, E4, E1)
            END IF
            IF (COMPARE_GLT(A(E5), A(E4))) THEN
                ! t = A(E5); A(E5) = A(E4); A(E4) = t
                EXCHANGE(A, E5, E4)
            END IF
            IF (COMPARE_GLT(A(E2), A(E1))) THEN
                ! t = A(E2); A(E2) = A(E1); A(E1) = t
                EXCHANGE(A, E2, E1)
            END IF
            IF (COMPARE_GLT(A(E4), A(E2))) THEN
                ! t = A(E4); A(E4) = A(E2); A(E2) = t
                EXCHANGE(A, E4, E2)
            END IF

            IF (COMPARE_GLT(A3, A(E2))) THEN
                IF (COMPARE_GLT(A3, A(E1))) THEN
                    A(E3) = A(E2)
                    A(E2) = A(E1)
                    A(E1) = A3
                ELSE
                    A(E3) = A(E2)
                    A(E2) = A3
                END IF
            ELSEIF (COMPARE_GLT(A(E4), A3)) THEN
                IF (COMPARE_GLT(A(E5), A3)) THEN
                    A(E3) = A(E4)
                    A(E4) = A(E5)
                    A(E5) = A3
                ELSE
                    A(E3) = A(E4)
                    A(E4) = A3
                END IF
            END IF

            ! Pointers
            Lower = Low     ! The index of the last element of the left part
            Upper = iEnd    ! The index of the first element of the right part

            ! Partitioning with 2 pivots in case of different elements.
            IF (COMPARE_GLT(A(E1), A(E2)) .AND. COMPARE_GLT(A(E2), A(E3)) .AND. &
                COMPARE_GLT(A(E3), A(E4)) .AND. COMPARE_GLT(A(E4), A(E5))) THEN

                ! Use the first and fifth of the five sorted elements as
                ! the pivots. These values are inexpensive approximation
                ! of tertiles. Note, that Pivot1 < Pivot2.
                Pivot1 = A(E1)
                Pivot2 = A(E5)

                ! The first and the last elements to be sorted are moved
                ! to the locations formerly occupied by the pivots. When
                ! partitioning is completed, the pivots are swapped back
                ! into their final positions, and excluded from the next
                ! subsequent sorting.
                A(E1) = A(Lower)
                A(E5) = A(Upper)

                ! Skip elements, which are less or greater than the pivots.
                Lower = Lower + 1
                DO WHILE (COMPARE_GLT(A(Lower), Pivot1))
                    Lower = Lower + 1
                END DO
                Upper = Upper - 1
                DO WHILE (COMPARE_GLT(Pivot2, A(Upper)))
                    Upper = Upper - 1
                END DO

                ! Backward 3-interval partitioning
                !   left part                 central part          right part
                ! +------------------------------------------------------------+
                ! |  < Pivot1  |   ?   |  Pivot1 <= && <= Pivot2  |  > Pivot2  |
                ! +------------------------------------------------------------+
                !             ^       ^                            ^
                !             |       |                            |
                !           Lower     K                          Upper
                ! Invariants:
                !              all in (Low, Lower) < Pivot1
                !    Pivot1 <= all in (K, Upper)  <= Pivot2
                !              all in [Upper, iEnd) > Pivot2
                ! Pointer K is the last index of ?-part
                Lower = Lower - 1
                Upper = Upper + 1
                K = Upper - 1
                DO WHILE (K > Lower)
                    AK = A(K)
                    IF (COMPARE_GLT(AK, Pivot1)) THEN
                        ! Move A(K) to the left side
                        DO WHILE (Lower < K)
                            Lower = Lower + 1
                            IF (COMPARE_GLE(Pivot1, A(Lower))) THEN
                                IF (COMPARE_GLT(Pivot2, A(Lower))) THEN
                                    Upper = Upper - 1
                                    A(K) = A(Upper)
                                    A(Upper) = A(Lower)
                                ELSE
                                    A(K) = A(Lower)
                                END IF
                                A(Lower) = AK
                                EXIT
                            END IF
                        END DO
                    ELSEIF (COMPARE_GLT(Pivot2, AK)) THEN
                        ! Move A(K) to the right side
                        Upper = Upper - 1
                        A(K) = A(Upper)
                        A(Upper) = AK
                    END IF
                    K = K - 1
                END DO

                ! Swap the pivots into their final positions.
                A(Low) = A(Lower)
                A(Lower) = Pivot1
                A(iEnd) = A(Upper)
                A(Upper) = Pivot2

                ! Sort non-left parts recursively ,
                ! excluding known pivots.
                CALL Quick_DualPivot(A, IOR(Bits, 1), Lower + 1, Upper)
                CALL Quick_DualPivot(A, IOR(Bits, 1), Upper + 1, High)

            ELSE    ! Use single pivot in case of many equal elements

                ! Use the third of the five sorted elements as the pivot.
                ! This value is inexpensive approximation of the median.
                Pivot = A(E3)

                ! The first element to be sorted is moved to the
                ! location formerly occupied by the pivot. After
                ! completion of partitioning the pivot is swapped
                ! back into its final position, and excluded from
                ! the next subsequent sorting.
                A(E3) = A(Lower)

                ! Traditional 3-way (Dutch National Flag) partitioning
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
                    AK = A(K)

                    IF (AK /= Pivot) THEN
                        A(K) = Pivot

                        IF (COMPARE_GLT(AK, Pivot)) THEN
                            ! Move A(K) to the left side
                            Lower = Lower + 1
                            DO WHILE (COMPARE_GLT(A(Lower), Pivot))
                                Lower = Lower + 1
                            END DO

                            IF (COMPARE_GLT(Pivot, A(Lower))) THEN
                                Upper = Upper - 1
                                A(Upper) = A(Lower)
                            END IF
                            A(Lower) = AK
                        ELSE
                            ! AK > Pivot - Move A(K) to the right side
                            Upper = Upper - 1
                            A(Upper) = AK
                        END IF
                    END IF
                    K = K - 1
                END DO

                ! Swap the pivot into its final position.
                A(Low) = A(Lower)
                A(Lower) = Pivot

                ! Sort the right part (possibly in parallel), excluding
                ! known pivot. All elements from the central part are
                ! equal and therefore already sorted.
                CALL Quick_DualPivot(A, IOR(Bits, 1), Upper, High)
            END IF
            High = Lower    ! Iterate along the left part

        END DO

        RETURN

    END SUBROUTINE Quick_DualPivot

    !**************************************************************************

    SUBROUTINE Insertion_Java(A, Low, High)

    !** PURPOSE OF THIS SUBROUTINE
        ! To sort the specified range of the array using insertion sort.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(0:)    ! the array to be sorted
        tIndex,    INTENT(IN)       :: Low      ! the index of the first element, inclusive, to be sorted
        tIndex,    INTENT(IN)       :: High     ! the index of the last element, exclusive, to be sorted

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: I, K
        tVarScalar  :: AI

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(AI, MOLD=A(Low))
#endif

        K = Low + 1
        DO WHILE (K < High)
            I = K
            AI = A(I)
            IF (COMPARE_GLT(AI, A(I-1))) THEN
                I = I - 1
                DO WHILE ((I >= Low).AND.(COMPARE_GLT(AI, A(I))))
                    A(I+1) = A(I)
                    I = I - 1
                END DO
                A(I+1) = AI
            END IF
            K = K + 1
        END DO

        RETURN

    END SUBROUTINE Insertion_Java

    !**************************************************************************

    SUBROUTINE Insertion_Mixed(A, Left, Last, Right)

    !** PURPOSE OF THIS SUBROUTINE
        ! To sort the specified range of the array using mixed insertion sort.
        ! Mixed insertion sort is combination of simple insertion sort, pin
        ! insertion sort and pair insertion sort.
        ! In the context of Dual-Pivot Quicksort, the pivot element from the
        ! left part plays the role of sentinel, because it is less than any
        ! elements from the given part. Therefore, expensive check of the
        ! left range can be skipped on each iteration unless it is the
        ! leftmost call.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(0:)    ! the array to be sorted
        tIndex,    INTENT(IN)       :: Left     ! the index of the first element, inclusive, to be sorted
        tIndex,    INTENT(IN)       :: Last     ! the index of the last element for simple insertion sort
        tIndex,    INTENT(IN)       :: Right    ! the index of the last element, exclusive, to be sorted

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tVarScalar  :: AI, A1, A2, Pin
        tIndex      :: Low, End, High
        tIndex      :: I, P

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(AI, MOLD=A(Left))
        ALLOCATE(A1, MOLD=A(Left))
        ALLOCATE(A2, MOLD=A(Left))
        ALLOCATE(Pin, MOLD=A(Left))
#endif

        ! initialize
        Low = Left
        End = Last
        High = Right

        IF (End == High) THEN

            ! Invoke simple insertion sort on tiny array.
            Low = Low + 1
            DO WHILE (Low < End)
                I = Low
                AI = A(I)

                I = I - 1
                DO WHILE (COMPARE_GLT(AI, A(I)))
                    A(I+1) = A(I)
                    I = I - 1
                END DO

                A(I+1) = AI
                Low = Low + 1
            END DO

        ELSE

            ! Start with pin insertion sort on small part.
            ! Pin insertion sort is extended simple insertion sort.
            ! The main idea of this sort is to put elements larger
            ! than an element called pin to the end of array (the
            ! proper area for such elements). It avoids expensive
            ! movements of these elements through the whole array.
            Pin = A(End)

            P = High
            Low = Low + 1
            DO WHILE (Low < End)

                I = Low
                AI = A(I)

                IF (COMPARE_GLT(AI, A(I-1))) THEN   ! Small element

                    ! Insert small element into sorted part.
                    A(I) = A(I-1)
                    I = I - 2

                    DO WHILE (COMPARE_GLT(AI, A(I)))
                        A(I+1) = A(I)
                        I = I - 1
                    END DO
                    A(I+1) = AI

                ELSEIF ((P > I).AND.(COMPARE_GLT(Pin, AI))) THEN    ! Large element

                    ! Find element smaller than pin.
                    P = P - 1
                    DO WHILE (COMPARE_GLT(Pin, A(P)))
                        P = P - 1
                    END DO

                    ! Swap it with large element.
                    IF (P > I) THEN
                        AI = A(P)
                        A(P) = A(I)
                    END IF

                    ! Insert small element into sorted part.
                    I = I - 1
                    DO WHILE (COMPARE_GLT(AI, A(I)))
                        A(I+1) = A(I)
                        I = I - 1
                    END DO
                    A(I+1) = AI
                END IF
                Low = Low + 1
            END DO

            ! Continue with pair insertion sort on remain part.
            DO WHILE (Low < High)

                I = Low
                A1 = A(I)
                Low = Low + 1
                A2 = A(Low)

                ! Insert two elements per iteration: at first, insert the
                ! larger element and then insert the smaller element, but
                ! from the position where the larger element was inserted.
                IF (COMPARE_GLT(A2, A1)) THEN

                    I = I - 1
                    DO WHILE (COMPARE_GLT(A1, A(I)))
                        A(I+2) = A(I)
                        I = I - 1
                    END DO
                    A(I+2) = A1
                    DO WHILE (COMPARE_GLT(A2, A(I)))
                        A(I+1) = A(I)
                        I = I - 1
                    END DO
                    A(I+1) = A2

                ELSEIF (COMPARE_GLT(A1, A(I-1))) THEN

                    I = I - 1
                    DO WHILE (COMPARE_GLT(A2, A(I)))
                        A(I+2) = A(I)
                        I = I - 1
                    END DO
                    A(I+2) = A2
                    DO WHILE (COMPARE_GLT(A1, A(I)))
                        A(I+1) = A(I)
                        I = I - 1
                    END DO
                    A(I+1) = A1

                END IF
                Low = Low + 1

            END DO

        END IF

        RETURN

    END SUBROUTINE Insertion_Mixed

    !**************************************************************************

    FUNCTION TryMergeRuns(A, Low, Size) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE
        ! To try to sort the specified range of the array.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, TARGET, INTENT(INOUT)    :: A(0:)    ! the array to be sorted
        tIndex,            INTENT(IN)       :: Low      ! the index of the first element to be sorted
        tIndex,            INTENT(IN)       :: Size     ! the array size
        tLogical                            :: Flag     ! true if finally sorted, false otherwise

    !** SUBROUTINE PARAMETER DECLARATIONS:
        ! Min size of the first run to continue with scanning.
        tInteger, PARAMETER :: MIN_FIRST_RUN_SIZE = 16
        ! Min factor for the first runs to continue scanning.
        tInteger, PARAMETER :: MIN_FIRST_RUNS_FACTOR = 7
        ! Max capacity of the index array for tracking runs.
        tInteger, PARAMETER :: MAX_RUN_CAPACITY = ISHFT(5, 10)
        ! Min number of runs, required by parallel merging.
        tInteger, PARAMETER :: MIN_RUN_COUNT = 4

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! The run array is constructed only if initial runs are
        ! long enough to continue, run[i] then holds start index
        ! of the i-th sequence of elements in desired order.
        tIndex, ALLOCATABLE :: Run(:)
        tVarAlloc           :: B(:)                 ! work space
        tVarScalar          :: Temp, AK
        tIndex              :: RunSize, NewRunSize
        tIndex              :: High, Last, Count, Offset
        tIndex              :: I, J, K, Dummy

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(AK, MOLD=A(Low))
        ALLOCATE(Temp, MOLD=A(Low))
#endif

        ! initialize
        High = Low + Size
        Count = 1
        Last = Low

        ! Identify all possible runs.
        K = Low + 1
        DO WHILE (K < High)

            ! Find the end index of the current run.
            IF (COMPARE_GLT(A(K-1), A(K))) THEN

                ! Identify ascending sequence
                K = K + 1
                DO WHILE ((K < High).AND.(COMPARE_GLE(A(K-1), A(K))))
                    K = K + 1
                END DO

            ELSEIF (COMPARE_GLT(A(K), A(K-1))) THEN

                ! Identify descending sequence
                K = K + 1
                DO WHILE ((K < High).AND.(COMPARE_GLE(A(K), A(K-1))))
                    K = K + 1
                END DO

                ! Reverse into a desired order
                I = Last
                J = K - 1
                DO WHILE ((I < J).AND.(COMPARE_GLT(A(J), A(I))))
                    ! AI = A(I); A(I) = A(J); A(J) = AI
                    EXCHANGE(A, I, J)
                    I = I + 1
                    J = J - 1
                END DO
            ELSE
                ! Identify constant sequence
                AK = A(K)
                K = K + 1
                DO WHILE ((K < High).AND.(AK == A(K)))
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

            ELSEIF (COMPARE_GLT(A(Last), A(Last-1))) THEN

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
            ALLOCATE(B(0:Size-1), MOLD=A)
            Dummy = Java_Merge_Runs(A, B, Offset, 1, Run, 0, Count)
        END IF

        Flag = TrueVal

        RETURN

    END FUNCTION TryMergeRuns

    !******************************************************************************

END SUBROUTINE Java_Sort

!******************************************************************************

MODULE SUBROUTINE PDQ_Sort(A)

!^ **PURPOSE OF THIS SUBROUTINE**: <br>
!  To sort an array in a desired order using the pattern-defeating quicksort
!  (pdqsort) [1], which is a variant of the introsort with various improvements
!  including median-of-three pivoting scheme, *BlockQuickSort* partitioning
!  scheme to lesson the branch mis-prediction penalties, an adaptive sort to
!  deals with an array with certain patterns, and a shuffling of array elements
!  to help the heapsort works better. <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] <a href="https://arxiv.org/abs/2106.05123">Peters, O.R.L. 2021.
!       Pattern-defeating Quicksort. </a> <br>
!   [2] <a href="https://github.com/orlp/pdqsort">pdqsort: Pattern-defeating
!       quicksort. Reference implementation. </a> <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(:)   !! array to be sorted

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! cutoff to insertion sort
    tIndex, PARAMETER   :: Insertion_CutOff     = 50_kIndex
    tIndex, PARAMETER   :: Insertion_Threshold  = 44_kIndex
    tIndex, PARAMETER   :: kSmallSort_Threshold = 16_kIndex
    ! Partitions above this size use Tukey's ninther to select the pivot
    tIndex, PARAMETER   :: Ninther_Threshold    = 128_kIndex
    ! When we detect an already sorted partition, attempt an insertion sort that allows this
    ! amount of element moves before giving up.
    tIndex, PARAMETER   :: Partial_Insertion_Limit = 8_kIndex

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: MaxDepth     ! maximum depth used to switch partitioning routine
    tIndex      :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(A, KIND=kIndex)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Insertion_CutOff) THEN
        CALL DualInsert_Guarded(A, 1_kIndex, NA)
        RETURN
    END IF

    ! determine maximum depth based on the size of the input array
    MaxDepth = MaximumDepth(NA)

    ! perform quick sort
    CALL PDQ_Recur(A, 0_kIndex, NA, MaxDepth, TrueVal)

    RETURN

    CONTAINS

    RECURSIVE SUBROUTINE PDQ_Recur(A,Left,Right,BadAllowed,LeftMost)

    !** PURPOSE OF THIS SUBROUTINE
        ! This is the main engine of the pdqsort.  Note that this implementation
        ! does not include the branchless part of the original implementation
        ! (i.e. the *partition_right_branchless* function).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(0:)        ! (sub)array to be sorted
        tIndex,    INTENT(IN)       :: Left         ! starting index of the (sub)array, inclusive
        tIndex,    INTENT(IN)       :: Right        ! ending index of the (sub)array, exclusive
        tIndex,    INTENT(IN)       :: BadAllowed   ! maximum depth recursion allowed
        tLogical,  INTENT(IN)       :: LeftMost     ! true if the given array is the leftmost subarray

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tVarScalar  :: Temp
        tIndex      :: PivotIndx
        tIndex      :: Size, HalfSize, LSize, RSize
        tIndex      :: HSM1, HSP1, LSOver4, RSOver4
        tIndex      :: iBegin, iEnd, MaxDepth
        tLogical    :: Already_Partitioned, Highly_Unbalanced
        tLogical    :: IsLeftMost

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Temp, MOLD=A(Left))
#endif

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
                    CALL DualInsert_Guarded(A, iBegin+1, iEnd)
                ELSE
                    CALL DualInsert_UnGuarded(A, iBegin+1, iEnd)
                END IF
                RETURN
            END IF

            ! Choose pivot as median of 3 or pseudo-median of 9.
            HalfSize = Size/2
            IF (Size > Ninther_Threshold) THEN
                HSM1 = HalfSize - 1
                HSP1 = HalfSize + 1
                CALL Sort_3_Items_Base0(A, iBegin, iBegin+HalfSize, iEnd-1)
                CALL Sort_3_Items_Base0(A, iBegin+1, iBegin+HSM1, iEnd-2)
                CALL Sort_3_Items_Base0(A, iBegin+2, iBegin+HSP1, iEnd-3)
                CALL Sort_3_Items_Base0(A, iBegin+HSM1, iBegin+HalfSize, iBegin+HSP1)
                EXCHANGE(A, iBegin, iBegin+HalfSize)
            ELSE
                CALL Sort_3_Items_Base0(A, iBegin+HalfSize, iBegin, iEnd-1)
            END IF

            ! If A(begin - 1) is the end of the right partition of a previous partition operation
            ! there is no element in [begin, end) that is smaller than A(begin - 1). Then if our
            ! pivot compares equal to A(begin - 1) we change strategy, putting equal elements in
            ! the left partition, greater elements in the right partition. We do not have to
            ! recurse on the left partition, since it's sorted (all equal).
            IF ((.NOT.IsLeftMost).AND.(COMPARE_GLE(A(iBegin), A(iBegin-1)))) THEN
                iBegin = PartitionLeft(A, iBegin, iEnd) + 1
                CYCLE
            END IF

            ! Partition and get results
            Already_Partitioned = PartitionRight(A, iBegin, iEnd, PivotIndx)

            ! Check for a highly unbalanced partition.
            LSize = PivotIndx - iBegin
            RSize = iEnd - (PivotIndx + 1)
            Highly_Unbalanced = (LSize < Size/8).OR.(RSize < Size/8)

            ! If we got a highly unbalanced partition we shuffle elements to break many patterns.
            IF (Highly_Unbalanced) THEN

                ! If we had too many bad partitions, switch to heapsort to guarantee O(n log n).
                MaxDepth = MaxDepth - 1
                IF (MaxDepth == 0) THEN
                    CALL HeapSort_Java(A, iBegin, iEnd)
                    RETURN
                END IF

                IF (LSize >= Insertion_Threshold) THEN
                    LSOver4 = LSize / 4
                    EXCHANGE(A, iBegin,      iBegin+LSOver4)
                    EXCHANGE(A, PivotIndx-1, PivotIndx-LSOver4)

                    IF (LSize > Ninther_Threshold) THEN
                        EXCHANGE(A, iBegin+1,    iBegin+(LSOver4+1))
                        EXCHANGE(A, iBegin+2,    iBegin+(LSOver4+2))
                        EXCHANGE(A, PivotIndx-2, PivotIndx-(LSOver4+1))
                        EXCHANGE(A, PivotIndx-3, PivotIndx-(LSOver4+2))
                    END IF
                END IF

                IF (RSize >= Insertion_Threshold) THEN
                    RSOver4 = RSize / 4
                    EXCHANGE(A, PivotIndx+1, PivotIndx+(1+RSOver4))
                    EXCHANGE(A, iEnd-1,      iEnd-RSOver4)

                    IF (RSize > Ninther_Threshold) THEN
                        EXCHANGE(A, PivotIndx+2, PivotIndx+(2+RSOver4))
                        EXCHANGE(A, PivotIndx+3, PivotIndx+(3+RSOver4))
                        EXCHANGE(A, iEnd-2,      iEnd-(1+RSOver4))
                        EXCHANGE(A, iEnd-3,      iEnd-(2+RSOver4))
                    END IF
                END IF
            ELSE
                ! If we were decently balanced and we tried to sort an already partitioned
                ! sequence try to use insertion sort.
                IF (Already_Partitioned .AND. Insertion_Partial(A, iBegin, PivotIndx) &
                                        .AND. Insertion_Partial(A, PivotIndx+1, iEnd)) THEN
                    RETURN
                END IF
            END IF

            ! Sort the left partition first using recursion and do tail recursion elimination for
            ! the right-hand partition.
            CALL PDQ_Recur(A, iBegin, PivotIndx, MaxDepth, IsLeftMost)
            iBegin = PivotIndx + 1
            IsLeftMost = FalseVal

        END DO

        RETURN

    END SUBROUTINE PDQ_Recur

    !**************************************************************************

    FUNCTION PartitionRight(A,Left,Right,PivotIndx) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE
        ! To partition the range [Left, Right] around a pivot A(Left).
        ! Elements equal to the pivot are put in the right-hand partition.
        ! Returns the position of the pivot after partitioning and whether
        ! the passed sequence already was correctly partitioned.  Assumes that
        ! the pivot is a median of at least 3 elements and that the range
        ! [Left, Right] is at least insertion_sort_threshold long.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(0:)        ! the array to be partitioned
        tIndex,    INTENT(IN)       :: Left         ! the start of the range
        tIndex,    INTENT(IN)       :: Right        ! the end of the range
        tIndex,    INTENT(OUT)      :: PivotIndx    ! position of the pivot after partitioning
        tLogical                    :: Flag         ! true if the passed sequence already was correctly partitioned

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tVarScalar  :: PivotVal, Temp
        tIndex      :: First, Last, LastMinus1
        tLogical    :: Already_Partitioned

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(PivotVal, MOLD=A(Left))
        ALLOCATE(Temp, MOLD=A(Left))
#endif

        ! set pivot value
        PivotVal = A(Left)

        ! initialize
        First = Left
        Last  = Right

        ! Find the first element greater than or equal to the pivot
        ! (the median of 3 guarantees this exists).
        First = First + 1
        DO WHILE (COMPARE_GLT(A(First), PivotVal))
            First = First + 1
        END DO

        ! Find the first element strictly smaller than the pivot.
        ! We have to guard this search if there was no element before A(First).
        IF ((First-1) == Left) THEN
            LastMinus1 = Last - 1
            DO WHILE ((First < Last).AND.(COMPARE_GLE(PivotVal, A(LastMinus1))))
                Last = LastMinus1
                LastMinus1 = Last - 1
            END DO
            Last = LastMinus1
        ELSE
            Last = Last - 1
            DO WHILE (COMPARE_GLE(PivotVal, A(Last)))
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
            EXCHANGE(A, First, Last)
            First = First + 1
            DO WHILE (COMPARE_GLT(A(First), PivotVal))
                First = First + 1
            END DO
            Last = Last - 1
            DO WHILE (COMPARE_GLE(PivotVal, A(Last)))
                Last = Last - 1
            END DO
        END DO

        ! Put the pivot in the right place.
        PivotIndx = First - 1
        A(Left) = A(PivotIndx)
        A(PivotIndx) = PivotVal

        Flag = Already_Partitioned

        RETURN

    END FUNCTION PartitionRight

    !**************************************************************************

    FUNCTION PartitionLeft(A,Left,Right) RESULT(PivotIndx)

    !** PURPOSE OF THIS SUBROUTINE
        ! Similar to routine 'PartitionRight', except elements equal to
        ! the pivot are put to the left of the pivot and it doesn't check
        ! or return if the passed sequence already was partitioned.
        ! Since this is rarely used (the many equal case), and in that case
        ! PDQSort already has O(n) performance, no block Quicksort is applied
        ! here for simplicity.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(0:)        ! the array to be partitioned
        tIndex,    INTENT(IN)       :: Left         ! the start of the range
        tIndex,    INTENT(IN)       :: Right        ! the end of the range
        tIndex                      :: PivotIndx    ! position of the pivot after partitioning

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tVarScalar  :: PivotVal, Temp
        tIndex      :: First, Last, FirstPlus1

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(PivotVal, MOLD=A(Left))
        ALLOCATE(Temp, MOLD=A(Left))
#endif

        ! set pivot value
        PivotVal = A(Left)

        ! initialize
        First = Left
        Last  = Right

        Last = Last - 1
        DO WHILE (COMPARE_GLT(PivotVal, A(Last)))
            Last = Last - 1
        END DO

        IF ((Last+1) == Right) THEN
            FirstPlus1 = First + 1
            DO WHILE ((First < Last).AND.(COMPARE_GLE(A(FirstPlus1), PivotVal)))
                First = FirstPlus1
                FirstPlus1 = First + 1
            END DO
            First = FirstPlus1
        ELSE
            First = First + 1
            DO WHILE (COMPARE_GLE(A(First), PivotVal))
                First = First + 1
            END DO
        END IF

        DO WHILE (First < Last)
            EXCHANGE(A, First, Last)
            Last = Last - 1
            DO WHILE (COMPARE_GLT(PivotVal, A(Last)))
                Last = Last - 1
            END DO
            First = First + 1
            DO WHILE (COMPARE_GLE(A(First), PivotVal))
                First = First + 1
            END DO
        END DO

        ! Put the pivot in the right place.
        PivotIndx = Last
        A(Left) = A(PivotIndx)
        A(PivotIndx) = PivotVal

        RETURN

    END FUNCTION PartitionLeft

    !**************************************************************************

    FUNCTION Insertion_Partial(A, Low, High) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE
        ! To sort the specified range of the array using insertion sort.  Will
        ! return FalseVal if more than partial limit elements were moved and
        ! abort sorting.  Otherwise, it will successfully sort and return TrueVal.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(0:)    ! the array to be sorted
        tIndex,    INTENT(IN)       :: Low      ! the index of the first element, inclusive, to be sorted
        tIndex,    INTENT(IN)       :: High     ! the index of the last element, exclusive, to be sorted
        tLogical                    :: Flag

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: I, K, Limit
        tVarScalar  :: AI

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(AI, MOLD=A(Low))
#endif

        Flag = TrueVal

        Limit = 0
        K = Low + 1
        DO WHILE (K < High)
            I = K
            AI = A(I)
            IF (COMPARE_GLT(AI, A(I-1))) THEN
                I = I - 1
                DO WHILE ((I >= Low).AND.(COMPARE_GLT(AI, A(I))))
                    A(I+1) = A(I)
                    I = I - 1
                END DO
                A(I+1) = AI
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

    !**************************************************************************

END SUBROUTINE PDQ_Sort

!------------------------------------------------------------------------------
!                     STABLE DRIVER ROUTINES
!------------------------------------------------------------------------------

MODULE SUBROUTINE Tim_Sort(A)

!^ **PURPOSE OF THIS SUBROUTINE**: <br>
!  To sort an array in a desired order using the Timsort algorithm [1],
!  which is an adaptive, natural mergesort that works well for many kinds
!  of partially ordered arrays.  Timsort is a hybrid, *stable* sorting
!  algorithm that uses the insertion sort for small arrays and the adaptive
!  mergesort for large arrays.  The implementation here is mainly based on
!  Java implementation [2]. <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] <a href="https://bugs.python.org/file4451/timsort.txt">Timsort by Tim Peters. </a> <br>
!   [2] <a href="https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/util/TimSort.java">
!       Java's Timsort class. </a> <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT), TARGET    :: A(:)   !! array to be sorted

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! cutoff to insertion sort
    tIndex, PARAMETER   :: Insertion_CutOff   = 32_kIndex
    ! This is the minimum sized sequence that will be merged.  Shorter
    ! sequences will be lengthened by calling binary sort.  If the entire
    ! array is less than this length, no merges will be performed.
    tIndex, PARAMETER   :: Default_MinMerge   = 64_kIndex
    ! When we get into galloping mode, we stay there until both runs win less
    ! often than MIN_GALLOP consecutive times.
    tIndex, PARAMETER   :: Default_MinGallop  = 7_kIndex
    ! Maximum initial size of tmp array, which is used for merging.  The array
    ! can grow to accommodate demand.
    tIndex, PARAMETER   :: Default_TmpStorage = 256_kIndex

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! This controls when we get *into* galloping mode.  It is initialized
    ! to MIN_GALLOP.  The mergeLo and mergeHi methods nudge it higher for
    ! random data, and lower for highly structured data.
    tIndex              :: Minimum_Gallop
    ! A stack of pending runs yet to be merged.  Run i starts at
    ! address base[i] and extends for len[i] elements.  It's always
    ! true (so long as the indices are in bounds) that:
    !     runBase[i] + runLen[i] == runBase[i + 1]
    ! so we could cut the storage for this, but it's a minor amount,
    ! and keeping all the info explicit simplifies the code.
    tIndex              :: StackSize            ! Number of pending runs on stack
    tIndex, ALLOCATABLE :: RunBase(:)
    tIndex, ALLOCATABLE :: RunLen(:)
    tVarAlloc,   TARGET :: ATmp(:)              ! Temp storage for merges
    tVarLocal,  POINTER :: APtr(:) => NULL()    ! pointer to the array to be sorted
    ! local variables used only in this main routine
    tIndex              :: Low, High, nRemains
    tIndex              :: nRun, MinRun, ForceRun
    tIndex              :: NA                   ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(A, KIND=kIndex)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Insertion_CutOff) THEN
        CALL Insert_Guarded(A, 1_kIndex, NA)
        RETURN
    END IF

    ! initialize
    Low      = 0_kIndex
    High     = NA
    nRemains = High - Low
    nRun     = 0_kIndex

    ! initialize working variables
    CALL InitWrkVar()

    ! March over the array once, left to right, finding natural runs,
    ! extending short natural runs to minRun elements, and merging runs
    ! to maintain stack invariant.
    MinRun = MinRunLength(nRemains)

    DO
        ! identify next run
        nRun = CountRunAndMakeAscending(A, Low, High)

        IF (nRun < MinRun) THEN
            ! If run is short, extend to min(MinRun, nRemains)
            ForceRun = nRemains
            IF (ForceRun > MinRun) ForceRun = MinRun
            CALL Insert_Guarded(A, Low+1_kIndex, Low+ForceRun)
            nRun = ForceRun
        END IF

        ! Push new run into the pending stack and merge if necessary
        CALL PushRun(Low, nRun)
        CALL MergeCollapse()

        ! advance to next run
        Low = Low + nRun
        nRemains = nRemains - nRun
        IF (nRemains == 0_kIndex) EXIT

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
        APtr(0:) => A

        ! set initial minimum gallop
        Minimum_Gallop = Default_MinGallop

        ! set initial values of main-procedure variables
        StackSize  = 0

        ! Allocate temp storage (which may be increased later if necessary)
        Length = SIZE(A)
        IF (Length < 2*Default_TmpStorage) THEN
            tLen = SHIFTR(Length,1)
        ELSE
            tLen = Default_TmpStorage
        END IF
        ALLOCATE(ATmp(0:tLen-1), MOLD=A)
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
        CALL MemAlloc(RunBase, StackLen, StartID=0_kIndex)
        CALL MemAlloc(RunLen, StackLen, StartID=0_kIndex)

        RETURN

    END SUBROUTINE InitWrkVar

    !**************************************************************************

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

    !**************************************************************************

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
        tIndex      :: R, N

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

    !**************************************************************************

    FUNCTION CountRunAndMakeAscending(AVal, Low, Hi) RESULT(NRun)

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
        tArgument, INTENT(INOUT)    :: AVal(0:) ! the array in which a run is to be counted and possibly reversed
        tIndex,    INTENT(IN)       :: Low      ! index of the first element in the run (inclusive)
        tIndex,    INTENT(IN)       :: Hi       ! index after the last element that may be contained in the run
        tIndex                      :: NRun     ! the length of the run beginning at the specified position

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: RunHi

    !** FLOW:

        ! initialize
        RunHi = Low + 1
        NRun  = 1

        IF (RunHi == Hi) RETURN

        ! Find end of run, and reverse range if descending
        IF (COMPARE_GLT(AVal(RunHi), AVal(Low))) THEN
            ! descending
            RunHi = RunHi + 1
            DO WHILE ((RunHi < Hi).AND.(COMPARE_GLT(AVal(RunHi), AVal(RunHi-1))))
                RunHi = RunHi + 1
            END DO
            CALL Reverse_Order_Base0(AVal, Low, RunHi-1)
        ELSE
            ! ascending
            RunHi = RunHi + 1
            DO WHILE ((RunHi < Hi).AND.(COMPARE_GLE(AVal(RunHi-1), AVal(RunHi))))
                RunHi = RunHi + 1
            END DO
        END IF

        NRun = RunHi - Low

        RETURN

    END FUNCTION CountRunAndMakeAscending

    !**************************************************************************

    FUNCTION GallopLeft(Key, AVal, Base, Length, Hint) RESULT(Offset)

    !** PURPOSE OF THIS SUBROUTINE
        ! To locate the position at which to insert the specified key into the
        ! specified sorted range; if the range contains an element equal to key,
        ! returns the index of the leftmost equal element.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(IN)   :: Key      ! the key whose insertion point to be searched for
        tArgument, INTENT(IN)   :: AVal(0:) ! the array to be searched
        tIndex,    INTENT(IN)   :: Base     ! the index of the first element in the range
        tIndex,    INTENT(IN)   :: Length   ! the length of the range(> 0)
        tIndex,    INTENT(IN)   :: Hint     ! the index at which to begin the search, 0 <= hint < n.
        tIndex                  :: Offset   ! the index where to insert the key value.

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: LastOffset, MaxOffset
        tIndex      :: Temp, M

    !** FLOW:

        ! initialize
        LastOffset = 0
        Offset     = 1

        IF (COMPARE_GLT(AVal(Base+Hint), Key)) THEN
            ! gallop right until AVal(Base+Hint+LastOffset) < Key <= AVal(Base+Hint+Offset)
            MaxOffset = Length - Hint
            DO WHILE ((Offset < MaxOffset).AND.(COMPARE_GLT(AVal(Base+Hint+Offset), Key)))
                LastOffset = Offset
                Offset = SHIFTL(Offset,1) + 1
                ! integer overflow
                IF (Offset <= 0) Offset = MaxOffset
            END DO
            IF (Offset > MaxOffset) Offset = MaxOffset
            ! Make offsets relative to base
            LastOffset = LastOffset + Hint
            Offset = Offset + Hint
        ELSE    ! Key <= AVal(Base+Hint)
            ! gallop left until AVal(Base+Hint-Offset) < Key <= AVal(Base+Hint-LastOffset)
            MaxOffset = Hint + 1
            DO WHILE ((Offset < MaxOffset).AND.(COMPARE_GLE(Key, AVal(Base+Hint-Offset))))
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

        ! Now AVal(Base+LastOffset) < Key <= AVal(Base+Offset), so Key belongs somewhere
        ! in the range (Base + LastOffset, Base + Offset]. Do a binary search, with
        ! invariant AVal(Base+LastOffset-1] < Key <= AVal(Base+Offset).
        LastOffset = LastOffset + 1
        DO WHILE (LastOffset < Offset)
            M = LastOffset + SHIFTR((Offset-LastOffset),1)
            IF (COMPARE_GLT(AVal(Base+M), Key)) THEN
                LastOffset = M + 1
            ELSE
                Offset = M
            END IF
        END DO

        RETURN

    END FUNCTION GallopLeft

    !**************************************************************************

    FUNCTION GallopRight(Key, AVal, Base, Length, Hint) RESULT(Offset)

    !** PURPOSE OF THIS SUBROUTINE
        ! Like gallopLeft, except that if the range contains an element equal to
        ! key, gallopRight returns the index after the rightmost equal element.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(IN)   :: Key      ! the key whose insertion point to be searched for
        tArgument, INTENT(IN)   :: AVal(0:) ! the array to be searched
        tIndex,    INTENT(IN)   :: Base     ! the index of the first element in the range
        tIndex,    INTENT(IN)   :: Length   ! the length of the range(> 0)
        tIndex,    INTENT(IN)   :: Hint     ! the index at which to begin the search, 0 <= hint < n.
        tIndex                  :: Offset   ! the index where to insert the key value.

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: LastOffset, MaxOffset
        tIndex      :: Temp, M

    !** FLOW:

        ! initialize
        LastOffset = 0
        Offset     = 1

        IF (COMPARE_GLT(Key, AVal(Base+Hint))) THEN
            ! gallop left until AVal(Base+Hint-Offset) <= Key < AVal(Base+Hint-LastOffset)
            MaxOffset = Hint + 1
            DO WHILE ((Offset < MaxOffset).AND.(COMPARE_GLT(Key, AVal(Base+Hint-Offset))))
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
        ELSE    ! Key >= AVal(Base+Hint)
            ! gallop right until AVal(Base+Hint+LastOffset) <= Key < AVal(Base+Hint+Offset)
            MaxOffset = Length - Hint
            DO WHILE ((Offset < MaxOffset).AND.(COMPARE_GLE(AVal(Base+Hint+Offset), Key)))
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

        ! Now AVal(Base+LastOffset) <= Key < AVal(Base+Offset), so Key belongs somewhere
        ! in the range (Base + LastOffset, Base + Offset]. Do a binary search, with
        ! invariant AVal(Base+LastOffset-1] <= Key < AVal(Base+Offset).
        LastOffset = LastOffset + 1
        DO WHILE (LastOffset < Offset)
            M = LastOffset + SHIFTR((Offset-LastOffset),1)
            IF (COMPARE_GLT(Key, AVal(Base+M))) THEN
                Offset = M
            ELSE
                LastOffset = M + 1
            END IF
        END DO

        RETURN

    END FUNCTION GallopRight

    !**************************************************************************

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

    !**************************************************************************

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
        tIndex      :: N

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

    !**************************************************************************

    SUBROUTINE MergeForceCollapse()

    !** PURPOSE OF THIS SUBROUTINE
        ! To merge all runs on the stack until only one remains.  This method is
        ! called once, to complete the sort.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        ! na

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: N

    !** FLOW:

        DO WHILE (StackSize > 1)
            N = StackSize - 2
            IF ((N > 0).AND.(RunLen(N-1) < RunLen(N+1))) N = N -1
            CALL MergeAt(N)
        END DO

        RETURN

    END SUBROUTINE MergeForceCollapse

    !**************************************************************************

    SUBROUTINE MergeAt(I)

    !** PURPOSE OF THIS SUBROUTINE
        ! To merge the two runs at stack indices i and i+1.  Run i must be
        ! the penultimate or ante-penultimate run on the stack.  In other words,
        ! I must be equal to stackSize-2 or stackSize-3.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex, INTENT(IN)  :: I    ! stack index of the first of the two runs to merge

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Base1, Len1
        tIndex      :: Base2, Len2
        tIndex      :: K

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

    !**************************************************************************

    SUBROUTINE MergeLow(Base1, Length1, Base2, Length2, AVal, Tmp)

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
        tIndex,    INTENT(IN)       :: Base1    ! index of first element in first run to be merged
        tIndex,    INTENT(IN)       :: Length1  ! length of first run to be merged (must be > 0)
        tIndex,    INTENT(IN)       :: Base2    ! index of first element in second run to be merged
                                                !   (must be aBase + aLen)
        tIndex,    INTENT(IN)       :: Length2  ! length of second run to be merged (must be > 0)
        tArgument, INTENT(INOUT)    :: AVal(0:) ! array (globally) to be sorted and
                                                !       (locally) to be merged into
        tArgument, INTENT(INOUT)    :: Tmp(0:)  ! work space

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Len1, Len2
        tIndex      :: Index, Dest
        tIndex      :: Cursor1, Cursor2

    !** FLOW:

        ! initialize
        Len1 = Length1
        Len2 = Length2
        Cursor1 = 0         ! Index into ATmp array
        Cursor2 = Base2     ! Index into APtr array
        Dest    = Base1     ! Index into APtr array

        ! copy first run into temp array
        COPY_ARRAY_ZERO_BASED(AVal, Base1, Tmp, Cursor1, Len1)

        ! Move first element of second run and deal with degenerate cases
        AVal(Dest) = AVal(Cursor2)
        Dest    = Dest + 1
        Cursor2 = Cursor2 + 1

        Len2 = Len2 - 1
        IF ((Len2) == 0) THEN
            COPY_ARRAY_ZERO_BASED(Tmp, Cursor1, AVal, Dest, Len1)
            RETURN
        END IF

        IF (Len1 == 1) THEN
            COPY_ARRAY_ZERO_BASED(AVal, Cursor2, AVal, Dest, Len2)
            ! Last element of run 1 to end of merge
            AVal(Dest+Len2) = Tmp(Cursor1)
            RETURN
        END IF

        ! Do the straightforward thing
        DO
            IF (COMPARE_GLT(AVal(Cursor2), Tmp(Cursor1))) THEN
                AVal(Dest) = AVal(Cursor2)
                Dest    = Dest + 1
                Cursor2 = Cursor2 + 1
                Len2    = Len2 - 1
                IF (Len2 == 0) EXIT
            ELSE
                AVal(Dest) = Tmp(Cursor1)
                Dest    = Dest + 1
                Cursor1 = Cursor1 + 1
                Len1    = Len1 - 1
                IF (Len1 == 1) EXIT
            END IF
        END DO

        IF (Len1 == 1) THEN
            COPY_ARRAY_ZERO_BASED(AVal, Cursor2, AVal, Dest, Len2)
            ! Move last element of run 1 to end of merge
            AVal(Dest+Len2) = Tmp(Cursor1)
        ELSEIF (Len1 == 0) THEN
            CALL Handle_ErrLevel('MergeLow', ModName, ErrSevere, 'Preconditions were not met.')
        ELSE
            COPY_ARRAY_ZERO_BASED(Tmp, Cursor1, AVal, Dest, Len1)
        END IF

        RETURN

    END SUBROUTINE MergeLow

    !**************************************************************************

    SUBROUTINE MergeLow_With_Gallop(Base1, Length1, Base2, Length2, AVal, Tmp)

    !** PURPOSE OF THIS SUBROUTINE
        ! To merge two adjacent runs in place, in a stable fashion.  The first
        ! element of the first run must be greater than the first element of the
        ! second run (a[base1] > a[base2]), and the last element of the first run
        ! (a[base1 + len1-1]) must be greater than all elements of the second run.
        ! For performance, this method should be called only when len1 <= len2;
        ! its twin, mergeHi should be called if len1 >= len2.  (Either method
        ! may be called if len1 == len2.)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex,    INTENT(IN)       :: Base1    ! index of first element in first run to be merged
        tIndex,    INTENT(IN)       :: Length1  ! length of first run to be merged (must be > 0)
        tIndex,    INTENT(IN)       :: Base2    ! index of first element in second run to be merged
                                                !   (must be aBase + aLen)
        tIndex,    INTENT(IN)       :: Length2  ! length of second run to be merged (must be > 0)
        tArgument, INTENT(INOUT)    :: AVal(0:) ! array (globally) to be sorted and
                                                !       (locally) to be merged into
        tArgument, INTENT(INOUT)    :: Tmp(0:)  ! work space

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
        COPY_ARRAY_ZERO_BASED(AVal, Base1, Tmp, Cursor1, Len1)

        ! Move first element of second run and deal with degenerate cases
        AVal(Dest) = AVal(Cursor2)
        Dest    = Dest + 1
        Cursor2 = Cursor2 + 1

        Len2 = Len2 - 1
        IF ((Len2) == 0) THEN
            COPY_ARRAY_ZERO_BASED(Tmp, Cursor1, AVal, Dest, Len1)
            RETURN
        END IF

        IF (Len1 == 1) THEN
            COPY_ARRAY_ZERO_BASED(AVal, Cursor2, AVal, Dest, Len2)
            ! Last element of run 1 to end of merge
            AVal(Dest+Len2) = Tmp(Cursor1)
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
                IF (COMPARE_GLT(AVal(Cursor2), Tmp(Cursor1))) THEN
                    AVal(Dest) = AVal(Cursor2)
                    Dest    = Dest + 1
                    Cursor2 = Cursor2 + 1
                    Count2  = Count2 + 1
                    Count1  = 0
                    Len2    = Len2 - 1
                    IF (Len2 == 0) EXIT OUTER
                ELSE
                    AVal(Dest) = Tmp(Cursor1)
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
                Count1 = GallopRight(AVal(Cursor2), Tmp, Cursor1, Len1, 0)
                IF (Count1 /= 0) THEN
                    COPY_ARRAY_ZERO_BASED(Tmp, Cursor1, AVal, Dest, Count1)
                    Dest    = Dest + Count1
                    Cursor1 = Cursor1 + Count1
                    Len1    = Len1 - Count1
                    ! len1 == 1 || len1 == 0
                    IF (Len1 <= 1) EXIT OUTER
                END IF
                AVal(Dest) = AVal(Cursor2)
                Dest    = Dest + 1
                Cursor2 = Cursor2 + 1
                Len2    = Len2 - 1
                IF (Len2 == 0) EXIT OUTER

                Count2 = GallopLeft(Tmp(Cursor1), AVal, Cursor2, Len2, 0)
                IF (Count2 /= 0) THEN
                    COPY_ARRAY_ZERO_BASED(AVal, Cursor2, AVal, Dest, Count2)
                    Dest    = Dest + Count2
                    Cursor2 = Cursor2 + Count2
                    Len2    = Len2 - Count2
                    IF (Len2 == 0) EXIT OUTER
                END IF

                AVal(Dest) = Tmp(Cursor1)
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
            COPY_ARRAY_ZERO_BASED(AVal, Cursor2, AVal, Dest, Len2)
            ! Move last element of run 1 to end of merge
            AVal(Dest+Len2) = Tmp(Cursor1)
        ELSEIF (Len1 == 0) THEN
            CALL Handle_ErrLevel('MergeLow_With_Gallop', ModName, ErrSevere, 'Preconditions were not met.')
        ELSE
            COPY_ARRAY_ZERO_BASED(Tmp, Cursor1, AVal, Dest, Len1)
        END IF

        RETURN

    END SUBROUTINE MergeLow_With_Gallop

    !**************************************************************************

    SUBROUTINE MergeHigh(Base1, Length1, Base2, Length2, AVal, Tmp)

    !** PURPOSE OF THIS SUBROUTINE
        ! Like mergeLo, except that this method should be called only if
        ! len1 >= len2; mergeLo should be called if len1 <= len2. (Either method
        ! may be called if len1 == len2.)
        ! ************************* IMPORTANT NOTE **************************
        !   The galloping mode is removed.
        ! *******************************************************************

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex,    INTENT(IN)       :: Base1    ! index of first element in first run to be merged
        tIndex,    INTENT(IN)       :: Length1  ! length of first run to be merged (must be > 0)
        tIndex,    INTENT(IN)       :: Base2    ! index of first element in second run to be merged
                                                !   (must be aBase + aLen)
        tIndex,    INTENT(IN)       :: Length2  ! length of second run to be merged (must be > 0)
        tArgument, INTENT(INOUT)    :: AVal(0:) ! array (globally) to be sorted and
                                                !       (locally) to be merged into
        tArgument, INTENT(INOUT)    :: Tmp(0:)  ! work space

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Len1, Len2
        tIndex      :: Index, Dest
        tIndex      :: Cursor1, Cursor2

    !** FLOW:

        ! initialize
        Len1 = Length1
        Len2 = Length2
        Cursor1 = Base1 + Len1 - 1  ! Index into ATmp array
        Cursor2 = Len2 - 1          ! Index into APtr array
        Dest    = Base2 + Len2 - 1  ! Index into APtr array

        ! copy second run into temp array
        COPY_ARRAY_ZERO_BASED(AVal, Base2, Tmp, 0, Len2)

        ! Move last element of first run and deal with degenerate cases
        AVal(Dest) = AVal(Cursor1)
        Dest    = Dest - 1
        Cursor1 = Cursor1 - 1

        Len1 = Len1 - 1
        IF (Len1 == 0) THEN
            COPY_ARRAY_ZERO_BASED(Tmp, 0, AVal, Dest-(Len2-1), Len2)
            RETURN
        END IF

        IF (Len2 == 1) THEN
            Dest    = Dest - Len1
            Cursor1 = Cursor1 - Len1
            COPY_ARRAY_ZERO_BASED(AVal, Cursor1+1, AVal, Dest+1, Len1)
            ! First element of run 2 to end of merge
            AVal(Dest) = Tmp(Cursor2)
            RETURN
        END IF

        ! Do the straightforward thing
        DO
            IF (COMPARE_GLT(Tmp(Cursor2), AVal(Cursor1))) THEN
                AVal(Dest) = AVal(Cursor1)
                Dest    = Dest - 1
                Cursor1 = Cursor1 - 1
                Len1 = Len1 - 1
                IF (Len1 == 0) EXIT
            ELSE
                AVal(Dest) = Tmp(Cursor2)
                Dest    = Dest - 1
                Cursor2 = Cursor2 - 1
                Len2    = Len2 - 1
                IF (Len2 == 1) EXIT
            END IF
        END DO

        IF (Len2 == 1) THEN
            Dest    = Dest - Len1
            Cursor1 = Cursor1 - Len1
            COPY_ARRAY_ZERO_BASED(AVal, Cursor1+1, AVal, Dest+1, Len1)
            ! Move first element of run2 to front of merge
            AVal(Dest) = Tmp(Cursor2)
        ELSEIF (Len2 == 0) THEN
            CALL Handle_ErrLevel('MergeHigh', ModName, ErrSevere, 'Preconditions were not met.')
        ELSE
            COPY_ARRAY_ZERO_BASED(Tmp, 0, AVal, Dest-(Len2-1), Len2)
        END IF

        RETURN

    END SUBROUTINE MergeHigh

    !**************************************************************************

    SUBROUTINE MergeHigh_With_Gallop(Base1, Length1, Base2, Length2, AVal, Tmp)

    !** PURPOSE OF THIS SUBROUTINE
        ! Like mergeLo, except that this method should be called only if
        ! len1 >= len2; mergeLo should be called if len1 <= len2.  (Either
        ! method may be called if len1 == len2.)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex,    INTENT(IN)       :: Base1    ! index of first element in first run to be merged
        tIndex,    INTENT(IN)       :: Length1  ! length of first run to be merged (must be > 0)
        tIndex,    INTENT(IN)       :: Base2    ! index of first element in second run to be merged
                                                !   (must be aBase + aLen)
        tIndex,    INTENT(IN)       :: Length2  ! length of second run to be merged (must be > 0)
        tArgument, INTENT(INOUT)    :: AVal(0:) ! array (globally) to be sorted and
                                                !       (locally) to be merged into
        tArgument, INTENT(INOUT)    :: Tmp(0:)  ! work space

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
        COPY_ARRAY_ZERO_BASED(AVal, Base2, Tmp, 0, Len2)

        ! Move last element of first run and deal with degenerate cases
        AVal(Dest) = AVal(Cursor1)
        Dest    = Dest - 1
        Cursor1 = Cursor1 - 1

        Len1 = Len1 - 1
        IF (Len1 == 0) THEN
            COPY_ARRAY_ZERO_BASED(Tmp, 0, AVal, Dest-(Len2-1), Len2)
            RETURN
        END IF

        IF (Len2 == 1) THEN
            Dest    = Dest - Len1
            Cursor1 = Cursor1 - Len1
            COPY_ARRAY_ZERO_BASED(AVal, Cursor1+1, AVal, Dest+1, Len1)
            ! First element of run 2 to end of merge
            AVal(Dest) = Tmp(Cursor2)
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
                IF (COMPARE_GLT(Tmp(Cursor2), AVal(Cursor1))) THEN
                    AVal(Dest) = AVal(Cursor1)
                    Dest    = Dest - 1
                    Cursor1 = Cursor1 - 1
                    Count1  = Count1 + 1
                    Count2  = 0
                    Len1 = Len1 - 1
                    IF (Len1 == 0) EXIT OUTER
                ELSE
                    AVal(Dest) = Tmp(Cursor2)
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
                Count1 = Len1 - GallopRight(Tmp(Cursor2), AVal, Base1, Len1, Len1-1)
                IF (Count1 /= 0) THEN
                    Dest    = Dest - Count1
                    Cursor1 = Cursor1 - Count1
                    Len1    = Len1 - Count1
                    COPY_ARRAY_ZERO_BASED(AVal, Cursor1+1, AVal, Dest+1, Count1)
                    IF (Len1 == 0) EXIT OUTER
                END IF

                AVal(Dest) = Tmp(Cursor2)
                Dest    = Dest - 1
                Cursor2 = Cursor2 - 1
                Len2    = Len2 - 1
                IF (Len2 == 1) EXIT OUTER

                Count2 = Len2 - GallopLeft(AVal(Cursor1), Tmp, 0, Len2, Len2 - 1)
                IF (Count2 /= 0) THEN
                    Dest    = Dest - Count2
                    Cursor2 = Cursor2 - Count2
                    Len2    = Len2 - Count2
                    COPY_ARRAY_ZERO_BASED(Tmp, Cursor2+1, AVal, Dest+1, Count2)
                    ! len2 == 1 || len2 == 0
                    IF (Len2 <= 1) EXIT OUTER
                END IF

                AVal(Dest) = AVal(Cursor1)
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
            COPY_ARRAY_ZERO_BASED(AVal, Cursor1+1, AVal, Dest+1, Len1)
            ! Move first element of run2 to front of merge
            AVal(Dest) = Tmp(Cursor2)
        ELSEIF (Len2 == 0) THEN
            CALL Handle_ErrLevel('MergeHigh_With_Gallop', ModName, ErrSevere, 'Preconditions were not met.')
        ELSE
            COPY_ARRAY_ZERO_BASED(Tmp, 0, AVal, Dest-(Len2-1), Len2)
        END IF

        RETURN

    END SUBROUTINE MergeHigh_With_Gallop

    !**************************************************************************

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
            ALLOCATE(ATmp(0:NewSize-1), MOLD=A)

        END IF

        RETURN

    END SUBROUTINE EnsureCapacity

    !**************************************************************************

END SUBROUTINE Tim_Sort

!******************************************************************************

MODULE SUBROUTINE Rust_Sort(A)

!^ **PURPOSE OF THIS SUBROUTINE**: <br>
!  To sort an array in a desired order using the Rust's mergesort algorithm [1],
!  which is a simplified version of the Timsort algorithm.  The implementation
!  here is mainly based on the *Ord_Sort* subroutine of the Fortran Standard
!  Library implementation [2]. <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] <a href="https://github.com/rust-lang/rust/blob/90eb44a5897c39e3dff9c7e48e3973671dcd9496/src/liballoc/slice.rs#L2159">
!       Rust's mergesort. </a> <br>
!   [2] <a href="https://github.com/fortran-lang/stdlib/blob/master/src/stdlib_sorting_ord_sort.fypp">
!       Fortran Standard Library's ordered sorting. </a> <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(:)   !! array to be sorted

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! initial cutoff to pair insertion sort
    tIndex,   PARAMETER :: Insertion_CutOff = 32_kIndex
    ! The maximum number of entries in a run stack, good for an array of 2**64 elements.
    tInteger, PARAMETER :: Max_Merge_Stack = INT(CEILING(LOG(2.0_kFP**64) / &
                                                 LOG(1.6180339887_kFP)))

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
!    tVarAlloc   :: Buffer(:)
    tIndex      :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(A, KIND=kIndex)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to use pair insertion instead
    IF (NA <= Insertion_CutOff) THEN
        CALL Insert_Guarded(A, 1, NA)
        RETURN
    END IF

    BLOCK
#ifdef TYPE_IS_IN_Comparable_CLASS
        tVarAlloc   :: Buffer(:)
#else
        tVarLocal   :: Buffer(0:NA/2-1)
#endif
#ifdef TYPE_IS_IN_Comparable_CLASS
        ! allocate a buffer to use as scratch memory.
        ALLOCATE(Buffer(0:NA/2-1), MOLD=A)
#endif
        ! perform merge sort
        CALL Rust_MergeSort(A, NA, Buffer)
    END BLOCK


    RETURN

CONTAINS

    SUBROUTINE Rust_MergeSort(A, ALen, B)

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
        tArgument, INTENT(INOUT)    :: A(0:)
        tIndex,    INTENT(IN)       :: ALen
        tArgument, INTENT(INOUT)    :: B(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Finish, Min_Run, nRun, R, RCount, Start
        tIndex      :: RunLen(0:Max_Merge_Stack-1)
        tIndex      :: RunBase(0:Max_Merge_Stack-1)
        tIndex      :: LeftLen, LeftBase
        tIndex      :: RightLen, RightBase

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
                IF (COMPARE_GLT(A(Start+1), A(Start))) THEN
                    Descending: DO WHILE (Start > 0)
                        IF (COMPARE_GLE(A(Start-1), A(Start))) EXIT Descending
                        Start = Start - 1
                    END DO Descending
                    CALL Reverse_Order_Base0(A, Start, Finish)
                ELSE
                    Ascending: DO WHILE(Start > 0)
                        IF (COMPARE_GLT(A(Start), A(Start-1))) EXIT Ascending
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
                    CALL Insert_Guarded(A, Start+1, Finish+1)
                ELSE
                    ! nRun is long enough, use insertion sort
                    Insert_Loop: DO WHILE (Start > 0)
                        IF ((Finish-Start) >= (Min_Run-1)) EXIT Insert_Loop
                        Start = Start - 1
                        CALL Insert_Head(A(Start:Finish))
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
                CALL Merging(A(LeftBase:RightBase+RightLen-1), LeftLen, B )

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

    END SUBROUTINE Rust_MergeSort

    !**********************************************************************************

    FUNCTION Calc_Min_Run(N) RESULT(Min_Run)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To determine the minimum length of a run from 32-63 so that N/MIN_RUN is
        ! less than or equal to a power of two. See
        ! https://svn.python.org/projects/python/trunk/Objects/listsort.txt.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex, INTENT(IN)  :: N
        tIndex              :: Min_Run

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Num
        tIndex      :: R

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

    !**********************************************************************************

    FUNCTION Collapse(Base, Length) RESULT (R)

    !** PURPOSE OF THIS SUBROUTINE
        ! To examine the stack of runs waiting to be merged, identifying adjacent runs
        ! to be merged until the stack invariants are reestablished:
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

    !**********************************************************************************

    SUBROUTINE Insert_Head(A)

    !** PURPOSE OF THIS SUBROUTINE
        ! To insert 'A(0)' into the pre-sorted sequence 'A(1:)' so that the
        ! whole 'A(0:)' becomes sorted, copying the first element into
        ! a temporary variable, iterating until the right place for it is found.
        ! copying every traversed element into the slot preceding it, and finally,
        ! copying data from the temporary variable into the resulting hole.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: I
        tVarScalar  :: Temp

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Temp, MOLD=A(0))
#endif

        Temp = A(0)
        Find_Hole: DO I = 1, SIZE(A)-1
            IF (COMPARE_GLE(Temp, A(I))) EXIT Find_Hole
            A(I-1) = A(I)
        END DO Find_Hole
        A(I-1) = Temp

        RETURN

    END SUBROUTINE Insert_Head

    !**********************************************************************************

    SUBROUTINE Merging(A, Mid, B)

    !** PURPOSE OF THIS SUBROUTINE
        ! To merge the two non-decreasing runs 'A(0:Mid-1)' and 'A(Mid:)'
        ! using 'B' as a temporary storage, and stores the merged runs into
        ! 'A(0:)'.  'Mid' must be > 0, and < 'SIZE(A)-1'. The buffer 'B'
        ! must be long enough to hold the shorter of the two runs.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(0:)
        tIndex,    INTENT(IN)       :: Mid
        tArgument, INTENT(INOUT)    :: B(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: ALen, I, J, K
        tVarScalar  :: Temp

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Temp, MOLD=A(0))
#endif

        ALen = SIZE(A)

        ! Merge first copies the shorter run into 'B'. Then, depending on which
        ! run was shorter, it traces the copied run and the longer run forwards
        ! (or backwards), comparing their next unprocessed elements and then
        ! copying the lesser (or greater) one into 'A'.

        IF (Mid <= (ALen - Mid)) THEN
            ! The left run is shorter.
            B(0:Mid-1) = A(0:Mid-1)
            I = 0
            J = Mid
            Merge_Lower: DO K = 0, ALen-1
                IF (COMPARE_GLE(B(I), A(J))) THEN
                    A(K) = B(I)
                    I = I + 1
                    IF (I >= Mid) EXIT Merge_Lower
                ELSE
                    A(K) = A(J)
                    J = J + 1
                    IF (J >= ALen) THEN
                        A(K+1:) = B(I:Mid-1)
                        EXIT Merge_Lower
                    END IF
                END IF
            END DO Merge_Lower
        ELSE
            ! The right run is shorter so check that it is stable
            B(0:ALen-Mid-1) = A(Mid:ALen-1)
            I = Mid - 1
            J = ALen - Mid -1
            Merge_Upper: DO K = ALen-1, 0, -1
                IF (COMPARE_GLE(A(I), B(J))) THEN
                    A(K) = B(J)
                    J = J - 1
                    IF (J < 0) EXIT Merge_Upper
                ELSE
                    A(K) = A(I)
                    I = I - 1
                    IF (I < 0) THEN
                        A(0:K-1) = B(0:J)
                        EXIT Merge_Upper
                    END IF
                END IF
            END DO Merge_Upper
        END IF

        RETURN

    END SUBROUTINE Merging

    !**********************************************************************************

END SUBROUTINE Rust_Sort

!------------------------------------------------------------------------------
!                           AUXILIARY ROUTINES
!------------------------------------------------------------------------------

SUBROUTINE HeapSort_Java(A, Left, Right)

!^ **PURPOSE OF THIS SUBROUTINE**: <br>
!^  To sort the specified range of the array using the heap sort algorithm.
!   This routine is based on Java implementation [1]. <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] <a href="https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/util/DualPivotQuicksort.java">
!       Java's DualPivotQuicksort class. </a> <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(0:)    ! the array to be sorted
    tIndex,    INTENT(IN)       :: Left     ! the index of the first element, inclusive, to be sorted
    tIndex,    INTENT(IN)       :: Right    ! the index of the last element, exclusive, to be sorted

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: K
    tIndex          :: Low
    tIndex          :: High
    tVarScalar      :: Max

!** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Max, MOLD=A(Left))
#endif

    ! initialize
    Low = Left
    High = Right

    ! perform heap sort
    K = SHIFTR(Low+High, 1)
    DO WHILE (K > Low)
        K = K - 1
        CALL PushDown(A, K, A(K), Low, High)
    END DO

    High = High - 1
    DO WHILE (High > Low)
        Max = A(Low)
        CALL PushDown(A, Low, A(High), Low, High)
        A(High) = Max
        High = High - 1
    END DO

    RETURN

    CONTAINS

    SUBROUTINE PushDown(A, Q, Value, Low, High)

    !** PURPOSE OF THIS SUBROUTINE
        ! To push specified element down during heap sort.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(0:)    ! the given array
        tIndex,    INTENT(IN)       :: Q        ! the start index
        tArgument, INTENT(IN)       :: Value    ! the given element
        tIndex,    INTENT(IN)       :: Low      ! the index of the first element, inclusive, to be sorted
        tIndex,    INTENT(IN)       :: High     ! the index of the last element, exclusive, to be sorted

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: K, P

    !** FLOW:

        ! initialize
        P = Q

        DO
            ! Index of the right child
            K = SHIFTL(P, 1) - Low + 2

            IF (K > High) EXIT

            IF ((K == High).OR.(COMPARE_GLT(A(K), A(K-1)))) K = K -1

            IF (COMPARE_GLE(A(K), Value)) EXIT

            ! update
            A(P) = A(K)
            P = K
        END DO

        A(P) = Value

        RETURN

    END SUBROUTINE PushDown

    !**************************************************************************

END SUBROUTINE HeapSort_Java

!******************************************************************************

SUBROUTINE Heap_Sort(NA, A)

!** PURPOSE OF THIS SUBROUTINE
	! To sort array in a desired order using heap sort

!** REFERENCES
    ! Adapted from Routines 'HeapSort' of Numerical Recipe F90 and Rosetta Code

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,    INTENT(IN)       :: NA       ! size of the arrays
    tArgument, INTENT(INOUT)    :: A(:)     ! array to be sorted

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: Start
    tIndex          :: Bottom
    tVarScalar      :: Temp

!** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Temp, MOLD=A(1))
#endif

    ! perform heap sort ranking
    DO Start = NA/2, 1, -1
        CALL Siftdown(A, Start, NA);
    END DO

    DO Bottom = NA, 2, -1
        SWAP_SCALAR(A(1), A(Bottom))
        CALL Siftdown(A, 1, Bottom-1)
    END DO

    RETURN

CONTAINS

    SUBROUTINE Siftdown(A, Start, Bottom)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To assist heap sort ranking

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT) :: A(:)
        tIndex,   INTENT(IN)    :: Start
        tIndex,   INTENT(IN)    :: Bottom

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: J, JOld
        tVarScalar      :: Temp

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Temp, MOLD=A(Start))
#endif

        Temp = A(Start)
        Jold = Start
        J    = Start + Start
        ! do while j <= bottom
        DO
            IF (J > Bottom) EXIT
            IF (J < Bottom) THEN
                ! compare to the better underling
                IF (COMPARE_GLT(A(J), A(J+1))) THEN
                    J = J + 1
                END IF
            END IF
            ! found a's level. Terminate the sift-down.
            IF (COMPARE_GLE(A(J), Temp)) EXIT
            ! otherwise, demote A and continue.
            A(Jold) = A(J)
            Jold = J
            J = J + J
        END DO
        ! put A into its slot.
        A(Jold) = Temp

        RETURN

    END SUBROUTINE Siftdown

    !**************************************************************************

END SUBROUTINE Heap_Sort

!------------------------------------------------------------------------------
!                       QUICK SORT ROUTINES
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!                   RECURSIVE QUICKSORT DRIVER ROUTINES
!------------------------------------------------------------------------------

MODULE SUBROUTINE Quick_Sort_Hoare(A)

!^ **PURPOSE OF THIS SUBROUTINE**: <br>
!  To sort an array in a desired order using the *Quicksort* algorithm with
!  Hoare's partitioning scheme [1].  The algorithm employed is actually a
!  hybrid one where the pair-insertion sort algorithm is utilized for small
!  (sub)array(s).  Also, the routine uses a tail recursion (instead of a
!  pure recursion) to minimize the recursive depth and make sure at most
!  O(log(n)) space is used [2]. <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] <a href="https://en.wikipedia.org/wiki/Quicksort">Quicksort. </a> <br>
!   [2] <a href="https://www.techiedelight.com/boost-quicksort-performance/">
!       How to Boost QuickSort Performance? </a> <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(:)   !! array to be sorted

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: CutOff       ! cutoff to pair insertion sort
    tIndex      :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(A, KIND=kIndex)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Quick_Insert_CutOff_Low) THEN
        CALL DualInsert_Guarded(A, 1_kIndex, NA)
        RETURN
    END IF

    ! set cutoff depending on the size of the input array
    IF (NA < 2000_kIndex) THEN
        CutOff = Quick_Insert_CutOff_Low
    ELSE
        CutOff = Quick_Insert_CutOff_High
    END IF

    ! perform quick sort using tail recursion
    CALL QuickSort_TailRecur(A, 1_kIndex, NA, CutOff, TrueVal, Partition_Hoare)

    RETURN

    CONTAINS

    SUBROUTINE Partition_Hoare(A,LStart,REnd,LEnd,RStart)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To partition an array using Hoare's partitioning scheme.
        ! The given array in the range [LStart,REnd] is partitioned
        ! into two sub-arrays, Left and Right, such that all elements
        ! in the Left sub-array in the range [LStart,LEnd] are less
        ! than or equal to the selected pivot, and all elements in
        ! the Right sub-array in the range [RStart,REnd] are greater
        ! than or equal to the selected pivot.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(:)     ! the array
        tIndex,    INTENT(IN)       :: LStart   ! on entry, the start of the given array
                                                ! on exit, the start of the Left sub-array
        tIndex,    INTENT(IN)       :: REnd     ! on entry, the end of the given array
                                                ! on exit, the end of the Right sub-array
        tIndex,    INTENT(OUT)      :: LEnd     ! the end of the Left sub-array
        tIndex,    INTENT(OUT)      :: RStart   ! the start of the Right sub-array

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tVarScalar  :: Pivot, Temp
        tIndex      :: Mid, LPtr, RPtr

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Pivot, MOLD=A(LStart))
        ALLOCATE(Temp, MOLD=A(LStart))
#endif

        ! Select the pivot
        Mid = LStart + (REnd-LStart) / 2
        Pivot = A(FLOOR(REAL(Mid,KIND=kFP)))

        ! Perform partitioning.
        LPtr = LStart
        RPtr = REnd
        DO WHILE (LPtr <= RPtr)

            ! Increment the left pointer until we find an element
            ! that is greater than or equal to the pivot.
            DO WHILE (COMPARE_GLT(A(LPtr), Pivot))
                LPtr = LPtr + 1
            END DO

            ! Decrement the right pointer until we find an element
            ! that is less than or equal to the pivot.
            DO WHILE (COMPARE_GLT(Pivot, A(RPtr)))
                RPtr = RPtr - 1
            END DO

            ! A pair of values have been found where A(LPtr) is greater than the pivot
            ! and A(RPtr) is less than the pivot; thus, while the left pointer is less
            ! than or equal to the right pointer, swap A(LPtr) with A(RPtr).
            IF (LPtr <= RPtr) THEN
                EXCHANGE(A, LPtr, RPtr)
                LPtr = LPtr + 1
                RPtr = RPtr - 1
            END IF

        END DO

        ! Set output indices
        LEnd   = LPtr-1
        RStart = LPtr

        RETURN

    END SUBROUTINE Partition_Hoare

    !**************************************************************************

END SUBROUTINE Quick_Sort_Hoare

!******************************************************************************

MODULE SUBROUTINE Quick_Sort_Lomuto(A)

!^ **PURPOSE OF THIS SUBROUTINE**: <br>
!  To sort an array in a desired order using the *Quicksort* algorithm with
!  Lomuto's partitioning scheme [1].  The algorithm employed is actually a
!  hybrid one where the pair-insertion sort algorithm is utilized for small
!  (sub)array(s).  Also, the routine uses a tail recursion (instead of a
!  pure recursion) to minimize the recursive depth and make sure at most
!  O(log(n)) space is used [2]. <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] <a href="https://en.wikipedia.org/wiki/Quicksort">Quicksort. </a> <br>
!   [2] <a href="https://www.techiedelight.com/boost-quicksort-performance/">
!       How to Boost QuickSort Performance? </a> <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(:)   !! array to be sorted

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: CutOff       ! cutoff to pair insertion sort
    tIndex      :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(A, KIND=kIndex)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Quick_Insert_CutOff_Low) THEN
        CALL DualInsert_Guarded(A, 1_kIndex, NA)
        RETURN
    END IF

    ! set cutoff depending on the size of the input array
    IF (NA < 2000_kIndex) THEN
        CutOff = Quick_Insert_CutOff_Low
    ELSE
        CutOff = Quick_Insert_CutOff_High
    END IF

    ! perform quick sort using tail recursion
    CALL QuickSort_TailRecur(A, 1_kIndex, NA, CutOff, TrueVal, Partition_Lomuto)

    RETURN

    CONTAINS

    SUBROUTINE Partition_Lomuto(A,LStart,REnd,LEnd,RStart)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To partition an array using Lomuto's partitioning scheme.
        ! The given array in the range [LStart,REnd] is partitioned
        ! into two sub-arrays, Left and Right, such that all elements
        ! in the Left sub-array in the range [LStart,LEnd] are less
        ! than or equal to the selected pivot, and all elements in
        ! the Right sub-array in the range [RStart,REnd] are greater
        ! than or equal to the selected pivot.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(:)     ! the array
        tIndex,    INTENT(IN)       :: LStart   ! on entry, the start of the given array
                                                ! on exit, the start of the Left sub-array
        tIndex,    INTENT(IN)       :: REnd     ! on entry, the end of the given array
                                                ! on exit, the end of the Right sub-array
        tIndex,    INTENT(OUT)      :: LEnd     ! the end of the Left sub-array
        tIndex,    INTENT(OUT)      :: RStart   ! the start of the Right sub-array

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: J, Mid, Indx
        tVarScalar  :: Pivot, Temp

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Pivot, MOLD=A(LStart))
        ALLOCATE(Temp, MOLD=A(LStart))
#endif

        ! Select the pivot
        Mid = LStart + (REnd-LStart) / 2
        IF (COMPARE_GLT(A(Mid), A(LStart))) THEN
            EXCHANGE(A, Mid, LStart)
        END IF
        IF (COMPARE_GLT(A(REnd), A(LStart))) THEN
            EXCHANGE(A, REnd, LStart)
        END IF
        IF (COMPARE_GLT(A(Mid),A(REnd))) THEN
            EXCHANGE(A, Mid, REnd)
        END IF
        Pivot = A(REnd)

        ! Perform partitioning
        Indx = LStart
        DO J = LStart, REnd
            IF (COMPARE_GLT(A(J), Pivot)) THEN
                EXCHANGE(A, Indx, J)
                Indx = Indx + 1
            END IF
        END DO
        EXCHANGE(A, Indx, REnd)

        ! Set output indices
        LEnd   = Indx - 1
        RStart = Indx + 1

        RETURN

    END SUBROUTINE Partition_Lomuto

    !**************************************************************************

END SUBROUTINE Quick_Sort_Lomuto

!******************************************************************************

MODULE SUBROUTINE Quick_Sort_Mo3(A)

!^ **PURPOSE OF THIS SUBROUTINE**: <br>
!  To sort an array in a desired order using the *Quicksort* algorithm with
!  median-of-three (Mo3) partitioning scheme [1].  The algorithm employed is
!  actually a hybrid one where the pair-insertion sort algorithm is utilized
!  for small (sub)array(s).  Unlike other *Quicksort* routines, this routine
!  uses a pure recursion instead of a tail recursion since it experimentally
!  appears that the pure recursion provides a better performance for this
!  particular implementation. <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] <a href="https://en.wikipedia.org/wiki/Quicksort">Quicksort. </a> <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(:)     !! array to be sorted

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: CutOff       ! cutoff to pair insertion sort
    tIndex      :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(A, KIND=kIndex)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Quick_Insert_CutOff_Low) THEN
        CALL DualInsert_Guarded(A, 1_kIndex, NA)
        RETURN
    END IF

    ! set cutoff depending on the size of the input array
    IF (NA < 2000_kIndex) THEN
        CutOff = Quick_Insert_CutOff_Low
    ELSE
        CutOff = Quick_Insert_CutOff_High
    END IF

    ! perform quick sort using pure recursion
    CALL QuickSort_Recursive(A, 1_kIndex, NA, CutOff, TrueVal, Partition_Mo3)

    RETURN

    CONTAINS

    SUBROUTINE Partition_Mo3(A,LStart,REnd,LEnd,RStart)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To partition an array using median-of-three partitioning scheme.
        ! The given array in the range [LStart,REnd] is partitioned
        ! into two sub-arrays, Left and Right, such that all elements
        ! in the Left sub-array in the range [LStart,LEnd] are less
        ! than or equal to the selected pivot, and all elements in
        ! the Right sub-array in the range [RStart,REnd] are greater
        ! than or equal to the selected pivot.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(:)     ! the array
        tIndex,    INTENT(IN)       :: LStart   ! on entry, the start of the given array
                                                ! on exit, the start of the Left sub-array
        tIndex,    INTENT(IN)       :: REnd     ! on entry, the end of the given array
                                                ! on exit, the end of the Right sub-array
        tIndex,    INTENT(OUT)      :: LEnd     ! the end of the Left sub-array
        tIndex,    INTENT(OUT)      :: RStart   ! the start of the Right sub-array

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tVarScalar  :: Pivot, Temp
        tIndex      :: Mid, LPtr, UPtr
        tIndex      :: I, J, K

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Pivot, MOLD=A(LStart))
        ALLOCATE(Temp, MOLD=A(LStart))
#endif

        ! choose median of left, center, and right elements as partitioning element (the pivot)
        ! also, rearrange so that A(LStart) < A(LStart+1) < A(REnd)
        Mid = LStart + (REnd-LStart) / 2
        CALL Sort_3_Items(A, LStart, Mid, REnd)
        EXCHANGE(A, Mid, LStart+1)

        ! set pivot value
        Pivot = A(LStart+1)

        ! set working pointers
        LPtr = LStart + 1
        UPtr = REnd

        DO
            ! increment the left pointer until we find an element
            ! that is greater than the pivot
            DO
                LPtr = LPtr + 1
                IF (COMPARE_GLE(Pivot, A(LPtr))) EXIT
            END DO

            ! decrement the right pointer until we find an element
            ! that is less than the pivot
            DO
                UPtr = UPtr - 1
                IF (COMPARE_GLE(A(UPtr), Pivot)) EXIT
            END DO

            ! exit if the pointers cross
            IF (UPtr < LPtr) EXIT

            ! swap A(LPtr) and A(UPtr)
            EXCHANGE(A, LPtr, UPtr)
        END DO

        ! insert partitioning element
        A(LStart+1) = A(UPtr)
        A(UPtr)     = Pivot

        ! set output indices
        LEnd   = UPtr - 1
        RStart = LPtr

        RETURN

    END SUBROUTINE Partition_Mo3

    !**************************************************************************

END SUBROUTINE Quick_Sort_Mo3

!******************************************************************************

MODULE SUBROUTINE Quick_Sort_3Way(A)

!^ **PURPOSE OF THIS SUBROUTINE**: <br>
!  To sort an array in a desired order using the *Quicksort* algorithm with
!  Bentley and McIlroy's three-way partitioning scheme [1].  The algorithm
!  employed is actually a hybrid one where the pair-insertion sort algorithm
!  is utilized for small (sub)array(s).  Also, the routine uses a tail recursion
!  (instead of a pure recursion) to minimize the recursive depth and make sure
!  at most O(log(n)) space is used [2]. <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] <a href="https://en.wikipedia.org/wiki/Quicksort">Quicksort. </a> <br>
!   [2] <a href="https://www.techiedelight.com/boost-quicksort-performance/">
!       How to Boost QuickSort Performance? </a> <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(:)   !! array to be sorted

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: Quick_Median_of_3_CutOff = 150   ! cutoff to median-of-3 partitioning

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: CutOff       ! cutoff to pair insertion sort
    tIndex      :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(A, KIND=kIndex)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Quick_Insert_CutOff_Low) THEN
        CALL DualInsert_Guarded(A, 1_kIndex, NA)
        RETURN
    END IF

    ! set cutoff depending on the size of the input array
    IF (NA < 2000_kIndex) THEN
        CutOff = Quick_Insert_CutOff_Low
    ELSE
        CutOff = Quick_Insert_CutOff_High
    END IF

    ! perform quick sort using tail recursion
    CALL QuickSort_TailRecur(A, 1_kIndex, NA, CutOff, TrueVal, Partition_Bentley)

    RETURN

    CONTAINS

    SUBROUTINE Partition_Bentley(A,LStart,REnd,LEnd,RStart)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To partition an array using Bentley-McIlroy's 3-way partitioning scheme.
        ! The given array in the range [LStart,REnd] is partitioned into three
        ! sub-arrays, Left, Right and Middle, such that all elements in the Left
        ! sub-array in the range [LStart,LEnd] are less than the selected pivot,
        ! and all elements in the Right sub-array in the range [RStart,REnd] are
        ! greater than the selected pivot, and all elements in the Middle sub-array
        ! (if exists) in the range [LEnd+1,RStart-1] are equal to the selected pivot.

    !** REFERENCES**:
        !   This routine is based on <a href="https://algs4.cs.princeton.edu/code/edu/princeton/cs/algs4/Quick3way.java.html">
        !   Quick3way.java</a> of 'Algorithms, 4th Edition' by Robert Sedgewick and Kevin Wayne.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(:)     ! the array
        tIndex,    INTENT(IN)       :: LStart   ! on entry, the start of the given array
                                                ! on exit, the start of the Left sub-array
        tIndex,    INTENT(IN)       :: REnd     ! on entry, the end of the given array
                                                ! on exit, the end of the Right sub-array
        tIndex,    INTENT(OUT)      :: LEnd     ! the end of the Left sub-array
        tIndex,    INTENT(OUT)      :: RStart   ! the start of the Right sub-array

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tVarScalar  :: Pivot, Temp
        tIndex      :: I, J, K
        tIndex      :: P, Q, N
        tIndex      :: Eps, Mid
        tIndex      :: M1, M2, M3
        tIndex      :: Ninther

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Pivot, MOLD=A(LStart))
        ALLOCATE(Temp, MOLD=A(LStart))
#endif

        N = REnd - LStart + 1
        IF (N <= Quick_Median_of_3_CutOff) THEN
            ! use median-of-3 as partitioning element
            M1 = Median_Of_Three(A, LStart, LStart+N/2, REnd)
            IF (M1 /= LStart) THEN
                EXCHANGE(A, M1, LStart)
            END IF
        ELSE
            ! use Tukey's ninther as partitioning element
            Eps = N/8
            Mid = LStart + N/2
            M1 = Median_Of_Three(A, LStart, LStart + Eps, LStart + Eps + Eps)
            M2 = Median_Of_Three(A, Mid - Eps, Mid, Mid + Eps)
            M3 = Median_Of_Three(A, REnd - Eps - Eps, REnd - Eps, REnd)
            Ninther = Median_Of_Three(A, M1, M2, M3)
            IF (Ninther /= LStart) THEN
                EXCHANGE(A, Ninther, LStart)
            END IF
        END IF

        ! Bentley-McIlroy's 3-way partitioning
        Pivot = A(LStart)
        RStart = LStart
        LEnd = REnd+1
        P = RStart
        Q = LEnd
        OUTLOOP: DO

            RStart = RStart + 1
            DO WHILE (COMPARE_GLT(A(RStart), Pivot))
                IF (RStart == REnd) EXIT
                RStart = RStart + 1
            END DO

            LEnd = LEnd - 1
            DO WHILE (COMPARE_GLT(Pivot, A(LEnd)))
                IF (LEnd == LStart) EXIT
                LEnd = LEnd - 1
            END DO

            ! pointers cross
            IF ((RStart == LEnd).AND.(A(RStart) == Pivot)) THEN
                P = P + 1
                EXCHANGE(A, P, RStart)
            END IF
            IF (RStart >= LEnd) EXIT OUTLOOP

            EXCHANGE(A, RStart, LEnd)
            IF (A(RStart) == Pivot) THEN
                P = P + 1
                EXCHANGE(A, P, RStart)
            END IF
            IF (A(LEnd) == Pivot) THEN
                Q = Q - 1
                EXCHANGE(A, Q, LEnd)
            END IF
        END DO OUTLOOP

        RStart = LEnd + 1
        DO K = LStart, P
            EXCHANGE(A, K, LEnd)
            LEnd = LEnd - 1
        END DO
        DO K = REnd, Q, -1
            EXCHANGE(A, K, RStart)
            RStart = RStart + 1
        END DO

        RETURN

    END SUBROUTINE Partition_Bentley

    !**************************************************************************

END SUBROUTINE Quick_Sort_3Way

!******************************************************************************

MODULE SUBROUTINE Quick_Sort_Vowels(A)

!^ **PURPOSE OF THIS SUBROUTINE**: <br>
!  To sort an array in a desired order using the *Quicksort* algorithm based
!  on the *QuickSort Version 3* (the professional version) by R.A. Vowels [1].
!  Like other quicksort routines, the algorithm employed here is actually a
!  hybrid one where the pair-insertion sort algorithm is utilized for small
!  (sub)array(s). <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] <a href="http://pages.swcp.com/~walt/fortran_store/Html/Info/books/adsff.html">
!       Robin A. Vowels. 1998. Algorithms and Data Structures in F and Fortran,
!       Unicomp. </a> <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(:)   !! array to be sorted

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: CutOff       ! cutoff to pair insertion sort
    tIndex      :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(A, KIND=kIndex)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Quick_Insert_CutOff_Low) THEN
        CALL DualInsert_Guarded(A, 1_kIndex, NA)
        RETURN
    END IF

    ! set cutoff depending on the size of the input array
    IF (NA < 2000_kIndex) THEN
        CutOff = Quick_Insert_CutOff_Low
    ELSE
        CutOff = Quick_Insert_CutOff_High
    END IF

    ! perform quick sort
    CALL QuickProfession(A, 1_kIndex, NA, CutOff, TrueVal)

    RETURN

CONTAINS

    RECURSIVE SUBROUTINE QuickProfession(A,Low,High,CutOff,LeftMost)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To sort array in the range [Low,High] using the profession version
        ! of Quicksort by Vowels.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(:)     ! the array to be sorted
        tIndex,    INTENT(IN)       :: Low      ! the start of the range
        tIndex,    INTENT(IN)       :: High     ! the end of the range
        tIndex,    INTENT(IN)       :: CutOff   ! cutoff to pair insertion sort
        tLogical,  INTENT(IN)       :: LeftMost ! flag indicating whether the specified range
                                                ! is the left most of the given array
                                                ! If false, an unguarded pair insertion sorting
                                                ! algorithm is used instead of the guarded one.

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Mid, Left, Right
        tVarScalar  :: Ref              ! To hold the Reference Element.
        tVarScalar  :: Temp             ! A temporary for swapping elements, same type as A.
        tIndex      :: L                ! L = a pointer used when searching from the left.
        tIndex      :: R                ! R = a pointer, used when searching from the right.
        tIndex      :: No_Swaps         ! To count the number of swaps.
        tIndex      :: La, J, Rml, NP

    ! FLOW

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Ref, MOLD=A(Low))
        ALLOCATE(Temp, MOLD=A(Low))
#endif

        Left = Low
        Right = High
        IF (Right <= Left) THEN             !! The partition is empty or contains one element.
            RETURN                          !! There's no work to do.
        END IF

        NP = Right - Left + 1
        ! check whether to perform insertion sort instead
        IF (NP < CutOff) THEN
            IF (LeftMost) THEN
                CALL DualInsert_Guarded(A, Left, Right)
            ELSE
                CALL DualInsert_UnGuarded(A, Left, Right)
            END IF
            RETURN
        END IF

        !! Section to select the median of three elements, and to move it to the left.
        Mid = (Left + Right)/2
        IF (COMPARE_GLT(A(Right), A(Mid)))  THEN
            EXCHANGE(A, Mid, Right)
        END IF

        IF (Left+1 == Right) THEN           !! There are 2 elements in the partition,
            RETURN                          !! & they are now in sort.
        END IF
        IF (COMPARE_GLT(A(Mid), A(Left)))  THEN
            EXCHANGE(A, Left, Mid)
        END IF
        IF (COMPARE_GLT(A(Right), A(Mid)))  THEN
            EXCHANGE(A, Mid, Right)
        END IF
        IF (Left+ 2 == Right) THEN          !! There are 3 elements in the partition,
            RETURN                          !! & they are now in sort.
        END IF
        IF (A(Mid) == A(Right)) THEN        !! Some elements are equal!
            Ref = A(Left)                   !! Forces the left partition to omit equal elements.
        ELSE
            EXCHANGE(A, Left, Mid)
            Ref = A(Left)                   !! Select the Reference Element.
        END IF

        L = Left
        R = Right + 1

        !! Partition the elements into three groups.
        No_Swaps = 0
        DO
            IF (L >= R) THEN
                EXIT
            END IF
            DO L = L + 1, R - 1             !! Scan from the left for an element
                IF (COMPARE_GLT(Ref, A(L))) THEN        !! larger than the Reference.
                    EXIT
                END IF
            END DO

            DO                              !! Scan from the right for an element
                R = R - 1
                IF (COMPARE_GLE(A(R), Ref)) THEN        !! less than or equal to the Reference.
                    EXIT
                END IF
            END DO

            IF (L < R) THEN                 !! Swap two elements that are in the
                                            !! wrong partitions.
                EXCHANGE(A, R, L)
                No_Swaps = No_Swaps + 1     !! Count each swap as we go.
            END IF
        END DO
                                            !! partitioning is complete.
        IF (Left < R) THEN                  !! Swap the Reference Element into its
                                            !! final position R in the array.
            A(Left) = A(R)
            A(R)    = Ref
        END IF
        !! At this point, A(R) is in its correct position in the list.  Elements A(Left) to A(R-1)
        !! are less than or equal to A(R), and elements A(R+1) to A(Right) are greater then A(R).

        !! Section to find out why no swaps were performed.
        IF (No_Swaps == 0) THEN             !! Something funny happened: not one
                                            !! element was moved. Investigate cause.
            INCREASING: DO
                !! Look for any pre-existing order.
                DO J = Left, Right-1
                    IF (COMPARE_GLT(A(J+1), A(J)))  THEN
                        EXIT INCREASING
                    END IF
                END DO
                RETURN                      !! The elements are already in order.
            END DO INCREASING

        !! Section to take a strong hand when the maximum number of elements is swapped.
        !! It's possible that the elements were in decreasing order.  Check it.
        ELSE IF (No_Swaps+1 == (Right-Left)/2) THEN
            !! All possible pairs were swapped.
            !! Perhaps the elements were in reverse order?  Find out why.
            DECREASING: DO
                Rml = Right - Left
                IF (IAND(Rml, 1) /= 0) THEN             !! A partition containing an even number
                                                        !! of elements was disarranged during
                                                        !! partitioning.
                    IF (Left < R-1)   THEN
                        EXCHANGE(A, Left, R-1)          !! Restore order.
                    END IF
                END IF
                DO J = Left, Right-1                    !! Check that the elements are sorted.
                    IF (COMPARE_GLT(A(J+1), A(J))) THEN
                        EXIT DECREASING
                    END IF
                END DO
                RETURN                                  !! The entire sub-list is sorted.
            END DO DECREASING
        END IF

        DO La = R-1, Left+1, -1             !! Pass over any elements that are equal to Ref.
            IF (A(La) /= Ref) THEN
                EXIT
            END IF
        END DO

        !! At this point, elements A(La+1) through A(R) are equal.
        !! A(L) is in its correct position too, even if it is not equal to A(Mid)!
        !! But if Left=La already, the partition was just lop-sided.

        IF (Left < La) THEN
            CALL QuickProfession(A, Left, La, CutOff, LeftMost)    !! Partition the left segment.
        END IF
                                            !! The element at R is in its correct position.
        IF (R+1 < Right) THEN
            CALL QuickProfession(A, R+1, Right, CutOff, FalseVal)  !! Partition the right segment.
        END IF

        RETURN

    END SUBROUTINE QuickProfession

    !**************************************************************************

END SUBROUTINE Quick_Sort_Vowels

!******************************************************************************

MODULE SUBROUTINE Quick_Sort_Stable(A)

!^ **PURPOSE OF THIS SUBROUTINE**: <br>
!  To sort an array in a desired order using the *Quicksort* algorithm with
!  stable median-of-medians partitioning scheme.  The algorithm employed is
!  actually a hybrid and *stable* one where the insertion sort algorithm is
!  utilized for small (sub)array(s).  Also, the routine uses a tail recursion
!  (instead of a pure recursion) to minimize the recursive depth and make sure
!  at most O(log(n)) space is used [1]. <br>
!  It should be noted that although the pair-insertion sort algorithm should
!  conceptually be a stable algorithm, it appears that the implementation here
!  is NOT stable.  Therefore, unlike other routines, the insertion sort is
!  used here instead. <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] <a href="https://www.techiedelight.com/boost-quicksort-performance/">
!       How to Boost QuickSort Performance? </a> <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(:)   !! array to be sorted

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: Insert_CutOff_Low  = 75    ! cutoff to pair insertion
    tInteger, PARAMETER :: Insert_CutOff_High = 95    ! cutoff to pair insertion

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: CutOff       ! cutoff to pair insertion sort
    tIndex      :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(A, KIND=kIndex)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Insert_CutOff_Low) THEN
        CALL Insert_Guarded(A, 1_kIndex, NA)
        RETURN
    END IF

    ! set cutoff depending on the size of the input array
    IF (NA < 2000_kIndex) THEN
        CutOff = Insert_CutOff_Low
    ELSE
        CutOff = Insert_CutOff_High
    END IF

    ! perform quick sort using tail recursion
    CALL QuickStableRecur(A, 1_kIndex, NA, CutOff, TrueVal)

    RETURN

    CONTAINS

    SUBROUTINE Partition_Stable(AVal, LStart, REnd, LEnd, RStart)

    !** PURPOSE OF THIS SUBROUTINE
        ! To partition an array using stable partitioning scheme.
        ! The given array in the range [LStart,REnd] is partitioned
        ! into two sub-arrays, Left and Right, such that all elements
        ! in the Left sub-array in the range [LStart,LEnd] are less
        ! than or equal to the selected pivot, and all elements in
        ! the Right sub-array in the range [RStart,REnd] are greater
        ! than or equal to the selected pivot.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: AVal(:)  ! values of array to be partitioned
        tIndex,    INTENT(IN)       :: LStart   ! on entry, the start of the given array
                                                ! on exit, the start of the Left sub-array
        tIndex,    INTENT(IN)       :: REnd     ! on entry, the end of the given array
                                                ! on exit, the end of the Right sub-array
        tIndex,    INTENT(OUT)      :: LEnd     ! the end of the Left sub-array
        tIndex,    INTENT(OUT)      :: RStart   ! the start of the Right sub-array

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
#ifdef TYPE_IS_IN_Comparable_CLASS
        tVarAlloc   :: BVal(:)          ! values of buffer array
#else
        tVarScalar  :: BVal(SIZE(AVal)) ! values of buffer array
#endif
        tVarScalar  :: Pivot            ! pivot used to partition the array
        tIndex      :: Less             ! current pointer for value less than the pivot
        tIndex      :: Great            ! current pointer for value greater than the pivot
        tIndex      :: Equal            ! current pointer for value equal to the pivot
        tIndex      :: Indx             ! index
        tIndex      :: BSize            ! size of BVal array
        tIndex      :: N, M1, M2, M3
        tIndex      :: Eps, Ninther

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Pivot, MOLD=A(LStart))
        ALLOCATE(BVal(SIZE(AVal)), MOLD=A)
#endif

        N = REnd - LStart + 1

        ! use Tukey's ninther as partitioning element (the pivot)
        ! also, rearrange so that A(LStart) < A(LStart+1) < A(REnd)
        Eps = N/8
        M1 = LStart + Eps
        M2 = LStart + N/2
        M3 = REnd - Eps
        M1 = Median_Of_Three(AVal, LStart, M1, M1+Eps)
        M2 = Median_Of_Three(AVal, M2-Eps, M2, M2+Eps)
        M3 = Median_Of_Three(AVal, M3-Eps, M3, REnd)
        Ninther = Median_Of_Three(AVal, M1, M2, M3)
        Pivot = AVal(Ninther)

        ! initialize greater index
        BSize = SIZE(BVal)
        Less  = LStart - 1
        Equal = BSize + 1
        Great = 0

        ! perform supposedly-stable partition of the array
        DO Indx = LStart, REnd
            ! check whether the current element is greater in value than our pivot value.
            ! If so, save it to the buffer.  Otherwise, move it up front
            IF (COMPARE_GLT(Pivot, AVal(Indx))) THEN
                ! save greater value to the front of the buffer
                Great = Great + 1
                BVal(Great) = AVal(Indx)
            ELSEIF (COMPARE_GLT(AVal(Indx), Pivot)) THEN
                ! move less value to the front of the array
                Less = Less + 1
                IF (Less < Indx) AVal(Less) = AVal(Indx)
            ELSE
                ! save equal value to the back of the buffer
                Equal = Equal - 1
                BVal(Equal) = AVal(Indx)
            END IF
        END DO

        ! set maximum index of sub-arrays with less values
        LEnd = Less

        ! transfer data back from the buffer
        IF (Equal <= BSize) THEN
            ! first, transfer data with equal values
            DO Indx = BSize, Equal, -1
                Less = Less + 1
                AVal(Less) = BVal(Indx)
            END DO
        END IF
        Less = Less + 1
        IF (Great >= 1) THEN
            ! next, transfer data with greater values
            AVal(Less:REnd) = BVal(1:Great)
        END IF
        ! set mimimum index of sub-arrays with greater values
        RStart = Less

        RETURN

    END SUBROUTINE Partition_Stable

    !******************************************************************************

    RECURSIVE SUBROUTINE QuickStableRecur(A,LStart,REnd,CutOff,LeftMost)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To sort the array in the range [LStart,REnd] in a desired order
        ! using recursive Quicksort algorithm.  If the number of elements in
        ! the range below the cutoff, then an insertion sorting routine
        ! is used instead.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(:)     ! the array to be sorted
        tIndex,    INTENT(IN)       :: LStart   ! the start of the range
        tIndex,    INTENT(IN)       :: REnd     ! the end of the range
        tIndex,    INTENT(IN)       :: CutOff   ! cutoff to pair insertion sort
        tLogical,  INTENT(IN)       :: LeftMost ! flag indicating whether the specified range
                                                ! is the left most of the given array
                                                ! If false, an unguarded pair insertion sorting
                                                ! algorithm is used instead of the guarded one.

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: LEnd     ! the end of the left sub-array
        tIndex      :: RStart   ! the start of the right sub-array
        tIndex      :: NE       ! number of elements in the range

    !** FLOW:

        ! determine the number of elements in the specified range
        NE = REnd - LStart + 1

        ! check whether to perform pair insertion sort
        IF (NE <= CutOff) THEN
            ! check whether the specified range is the left most range of the given array
            IF (LeftMost) THEN
                CALL Insert_Guarded(A, LStart, REnd)
            ELSE
                CALL Insert_UnGuarded(A, LStart, REnd)
            END IF
            RETURN
        END IF

        ! perform partitioning
        CALL Partition_Stable(A, LStart, REnd, LEnd, RStart)

        ! perform quick sort on the two sub-arrays
        CALL QuickStableRecur(A, LStart, LEnd, CutOff, LeftMost)
        CALL QuickStableRecur(A, RStart, REnd, CutOff, FalseVal)

        RETURN

    END SUBROUTINE QuickStableRecur

    !******************************************************************************

END SUBROUTINE Quick_Sort_Stable

!------------------------------------------------------------------------------
!                   ITERATIVE QUICKSORT DRIVER ROUTINES
!------------------------------------------------------------------------------

MODULE SUBROUTINE Quick_Sort_Iterative(A)

!^ **PURPOSE OF THIS SUBROUTINE**: <br>
!  To sort an array in a desired order using an *iterative Quicksort*
!  algorithm with median-of-three (Mo3) partitioning scheme.  The algorithm
!  employed is actually a hybrid one where the pair-insertion sort algorithm
!  is utilized for small (sub)array(s).  The implementation here is based
!  mainly on the *Sort* subroutine of Numerical Recipes in Fortran 90 [1]. <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] <a href="http://numerical.recipes/oldverswitcher.html">Numerical
!       Recipes Books Online. </a> <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(:)   !! array to be sorted

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: iStart, iEnd
    tIndex      :: CutOff
    tIndex      :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(A, KIND=kIndex)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! initialize
    iStart = 1_kIndex
    iEnd = NA
    IF (NA < 2000_kIndex) THEN
        CutOff = Quick_Insert_CutOff_Low
    ELSE
        CutOff = Quick_Insert_CutOff_High
    END IF

    ! perform quick sort
    CALL QuickIterative(A, iStart, iEnd, CutOff)

    RETURN

CONTAINS

    SUBROUTINE QuickIterative(A,Left,Right,CutOff)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(:)
        tIndex,    INTENT(IN)       :: Left
        tIndex,    INTENT(IN)       :: Right
        tIndex,    INTENT(IN)       :: CutOff

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tVarScalar  :: Pivot, Temp
        tIndex      :: I, J, K, L, R
        tIndex      :: Ptr
        tIndex      :: Stack(2,STORAGE_SIZE(NA))

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Pivot, MOLD=A(Left))
        ALLOCATE(Temp, MOLD=A(Left))
#endif

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
                    CALL DualInsert_Guarded(A, L, R)
                ELSE
                    ! not left-most sub-array
                    CALL DualInsert_UnGuarded(A, L, R)
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
            EXCHANGE(A, K, L+1)
            IF (COMPARE_GLT(A(R), A(L))) THEN
                EXCHANGE(A, L, R)
            END IF
            IF (COMPARE_GLT(A(R), A(L+1))) THEN
                EXCHANGE(A, L+1, R)
            END IF
            IF (COMPARE_GLT(A(L+1), A(L))) THEN
                EXCHANGE(A, L, L+1)
            END IF
            ! initialize pointers for partitioning
            I = L+1
            J = R
            ! partitioning element
            Pivot = A(L+1)
            DO
                ! here is the meat
                DO
                    ! scan up to find element >= Pivot
                    I = I+1
                    IF (COMPARE_GLE(Pivot, A(I))) EXIT
                END DO
                DO
                    ! scan down to find element <= Pivot
                    J = J-1
                    IF (COMPARE_GLE(A(J), Pivot)) EXIT
                END DO
                ! pointers crossed. exit with partitioning complete.
                IF (J < I) EXIT
                EXCHANGE(A, I, J)
            END DO
            ! insert partitioning element
            A(L+1) = A(J)
            A(J)   = Pivot
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

    END SUBROUTINE QuickIterative

    !**************************************************************************

END SUBROUTINE Quick_Sort_Iterative

!------------------------------------------------------------------------------
!                   DUAL-PIVOT QUICKSORT DRIVER ROUTINES
!------------------------------------------------------------------------------

MODULE SUBROUTINE Quick_Sort_Java(A)

!^ **PURPOSE OF THIS SUBROUTINE**: <br>
!  To sort an array in a desired order using a *dual-pivot Quicksort*
!  algorithm based on Java's sorting algorithm [1].  The algorithm
!  employed is actually a hybrid one where the pair-insertion sort algorithm
!  is utilized for small (sub)array(s). <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] <a href="https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/util/DualPivotQuicksort.java">
!       Java's DualPivotQuicksort class. </a> <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(:)   !! array to be sorted

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: CutOff       ! cutoff to pair insertion sort
    tIndex      :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(A, KIND=kIndex)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Quick_Insert_CutOff_Low) THEN
        CALL DualInsert_Guarded(A, 1_kIndex, NA)
        RETURN
    END IF

    ! set cutoff depending on the size of the input array
    IF (NA < 2000_kIndex) THEN
        CutOff = Quick_Insert_CutOff_Low
    ELSE
        CutOff = Quick_Insert_CutOff_High
    END IF

    ! perform double-pivot quick sort
    CALL QuickSortDP(A, 0_kIndex, NA, CutOff)

    RETURN

CONTAINS

    RECURSIVE SUBROUTINE QuickSortDP(A,Lo,Hi,CutOff)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To sort the array in the range [Lo,Hi-1] in a desired order
        ! using double-pivot Quicksort algorithm.  If the number of elements in
        ! the range below the cutoff, then a pair-insertion sorting routine
        ! is used instead.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(0:)    ! the array to be sorted
        tIndex,    INTENT(IN)       :: Lo       ! the start of the range (inclusive)
        tIndex,    INTENT(IN)       :: Hi       ! the end of the range (exclusive)
        tIndex,    INTENT(IN)       :: CutOff   ! cutoff to pair insertion sort

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Low, High, iEnd, Size
        tVarScalar  :: A3, AK, Temp, Pivot, Pivot1, Pivot2
        tIndex      :: Step
        tIndex      :: K
        tIndex      :: E1, E2, E3, E4, E5
        tIndex      :: Lower, Upper

    !** FLOW:

#ifdef TYPE_IS_IN_Comparable_CLASS
        ALLOCATE(Pivot, MOLD=A(Lo))
        ALLOCATE(Pivot1, MOLD=A(Lo))
        ALLOCATE(Pivot2, MOLD=A(Lo))
        ALLOCATE(A3, MOLD=A(Lo))
        ALLOCATE(AK, MOLD=A(Lo))
        ALLOCATE(Temp, MOLD=A(Lo))
#endif

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
                    CALL DualInsert_Guarded(A, 1, High)
                ELSE
                    CALL DualInsert_UnGuarded(A, Low+1, High)
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
            A3 = A(E3)

            ! Sort these elements in place by the combination
            ! of 4-element sorting network and insertion sort.
            !    5 ------o-----------o------------
            !            |           |
            !    4 ------|-----o-----o-----o------
            !            |     |           |
            !    2 ------o-----|-----o-----o------
            !                  |     |
            !    1 ------------o-----o------------
            IF (COMPARE_GLT(A(E5), A(E2))) THEN
                ! t = A(E5); A(E5) = A(E2); A(E2) = t
                EXCHANGE(A, E5, E2)
            END IF
            IF (COMPARE_GLT(A(E4), A(E1))) THEN
                ! t = A(E4); A(E4) = A(E1); A(E1) = t
                EXCHANGE(A, E4, E1)
            END IF
            IF (COMPARE_GLT(A(E5), A(E4))) THEN
                ! t = A(E5); A(E5) = A(E4); A(E4) = t
                EXCHANGE(A, E5, E4)
            END IF
            IF (COMPARE_GLT(A(E2), A(E1))) THEN
                ! t = A(E2); A(E2) = A(E1); A(E1) = t
                EXCHANGE(A, E2, E1)
            END IF
            IF (COMPARE_GLT(A(E4), A(E2))) THEN
                ! t = A(E4); A(E4) = A(E2); A(E2) = t
                EXCHANGE(A, E4, E2)
            END IF

            IF (COMPARE_GLT(A3, A(E2))) THEN
                IF (COMPARE_GLT(A3, A(E1))) THEN
                    A(E3) = A(E2)
                    A(E2) = A(E1)
                    A(E1) = A3
                ELSE
                    A(E3) = A(E2)
                    A(E2) = A3
                END IF
            ELSEIF (COMPARE_GLT(A(E4), A3)) THEN
                IF (COMPARE_GLT(A(E5), A3)) THEN
                    A(E3) = A(E4)
                    A(E4) = A(E5)
                    A(E5) = A3
                ELSE
                    A(E3) = A(E4)
                    A(E4) = A3
                END IF
            END IF

            ! Pointers
            Lower = Low     ! The index of the last element of the left part
            Upper = iEnd    ! The index of the first element of the right part

            ! Partitioning with 2 pivots in case of different elements.
            IF (COMPARE_GLT(A(E1), A(E2)) .AND. COMPARE_GLT(A(E2), A(E3)) .AND. &
                COMPARE_GLT(A(E3), A(E4)) .AND. COMPARE_GLT(A(E4), A(E5))) THEN

                ! Use the first and fifth of the five sorted elements as
                ! the pivots. These values are inexpensive approximation
                ! of tertiles. Note, that Pivot1 < Pivot2.
                Pivot1 = A(E1)
                Pivot2 = A(E5)

                ! The first and the last elements to be sorted are moved
                ! to the locations formerly occupied by the pivots. When
                ! partitioning is completed, the pivots are swapped back
                ! into their final positions, and excluded from the next
                ! subsequent sorting.
                A(E1) = A(Lower)
                A(E5) = A(Upper)

                ! Skip elements, which are less or greater than the pivots.
                Lower = Lower + 1
                DO WHILE (COMPARE_GLT(A(Lower), Pivot1))
                    Lower = Lower + 1
                END DO
                Upper = Upper - 1
                DO WHILE (COMPARE_GLT(Pivot2, A(Upper)))
                    Upper = Upper - 1
                END DO

                ! Backward 3-interval partitioning
                !   left part                 central part          right part
                ! +------------------------------------------------------------+
                ! |  < Pivot1  |   ?   |  Pivot1 <= && <= Pivot2  |  > Pivot2  |
                ! +------------------------------------------------------------+
                !             ^       ^                            ^
                !             |       |                            |
                !           Lower     K                          Upper
                ! Invariants:
                !              all in (Low, Lower) < Pivot1
                !    Pivot1 <= all in (K, Upper)  <= Pivot2
                !              all in [Upper, iEnd) > Pivot2
                ! Pointer K is the last index of ?-part
                Lower = Lower - 1
                Upper = Upper + 1
                K = Upper - 1
                DO WHILE (K > Lower)
                    AK = A(K)
                    IF (COMPARE_GLT(AK, Pivot1)) THEN
                        ! Move A(K) to the left side
                        DO WHILE (Lower < K)
                            Lower = Lower + 1
                            IF (COMPARE_GLE(Pivot1, A(Lower))) THEN
                                IF (COMPARE_GLT(Pivot2, A(Lower))) THEN
                                    Upper = Upper - 1
                                    A(K) = A(Upper)
                                    A(Upper) = A(Lower)
                                ELSE
                                    A(K) = A(Lower)
                                END IF
                                A(Lower) = AK
                                EXIT
                            END IF
                        END DO
                    ELSEIF (COMPARE_GLT(Pivot2, AK)) THEN
                        ! Move A(K) to the right side
                        Upper = Upper - 1
                        A(K) = A(Upper)
                        A(Upper) = AK
                    END IF
                    K = K - 1
                END DO

                ! Swap the pivots into their final positions.
                A(Low) = A(Lower)
                A(Lower) = Pivot1
                A(iEnd) = A(Upper)
                A(Upper) = Pivot2

                ! Sort non-left parts recursively ,
                ! excluding known pivots.
                CALL QuickSortDP(A, Lower + 1, Upper, CutOff)
                CALL QuickSortDP(A, Upper + 1, High, CutOff)

            ELSE    ! Use single pivot in case of many equal elements

                ! Use the third of the five sorted elements as the pivot.
                ! This value is inexpensive approximation of the median.
                Pivot = A(E3)

                ! The first element to be sorted is moved to the
                ! location formerly occupied by the pivot. After
                ! completion of partitioning the pivot is swapped
                ! back into its final position, and excluded from
                ! the next subsequent sorting.
                A(E3) = A(Lower)

                ! Traditional 3-way (Dutch National Flag) partitioning
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
                    AK = A(K)

                    IF (AK /= Pivot) THEN
                        A(K) = Pivot

                        IF (COMPARE_GLT(AK, Pivot)) THEN
                            ! Move A(K) to the left side
                            Lower = Lower + 1
                            DO WHILE (COMPARE_GLT(A(Lower), Pivot))
                                Lower = Lower + 1
                            END DO

                            IF (COMPARE_GLT(Pivot, A(Lower))) THEN
                                Upper = Upper - 1
                                A(Upper) = A(Lower)
                            END IF
                            A(Lower) = AK
                        ELSE
                            ! AK > Pivot - Move A(K) to the right side
                            Upper = Upper - 1
                            A(Upper) = AK
                        END IF
                    END IF
                    K = K - 1
                END DO

                ! Swap the pivot into its final position.
                A(Low) = A(Lower)
                A(Lower) = Pivot

                ! Sort the right part , excluding known pivot.
                ! All elements from the central part are equal
                ! and therefore already sorted.
                CALL QuickSortDP(A, Upper, High, CutOff)
            END IF
            High = Lower    ! Iterate along the left part

        END DO

        RETURN

    END SUBROUTINE QuickSortDP

    !**************************************************************************

END SUBROUTINE Quick_Sort_Java

!------------------------------------------------------------------------------
!                   RECURSIVE QUICKSORT WORKING ROUTINES
!------------------------------------------------------------------------------

RECURSIVE SUBROUTINE QuickSort_Recursive(A,LStart,REnd,CutOff,LeftMost,Partition_Array)

!** PURPOSE OF THIS SUBROUTINE
	! To sort the array in the range [LStart,REnd] in a desired order
    ! using recursive Quicksort algorithm.  If the number of elements in
    ! the range below the cutoff, then a pair-insertion sorting routine
    ! is used instead.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(:)             ! the array to be sorted
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
            CALL DualInsert_Guarded(A, LStart, REnd)
        ELSE
            CALL DualInsert_UnGuarded(A, LStart, REnd)
        END IF
        RETURN
    END IF

    ! perform partitioning
    CALL Partition_Array(A, LStart, REnd, LEnd, RStart)

    ! perform quick sort on the two sub-arrays
    CALL QuickSort_Recursive(A, LStart, LEnd, CutOff, LeftMost, Partition_Array)
    CALL QuickSort_Recursive(A, RStart, REnd, CutOff, FalseVal, Partition_Array)

    RETURN

END SUBROUTINE QuickSort_Recursive

!******************************************************************************

RECURSIVE SUBROUTINE QuickSort_TailRecur(A,Left,Right,CutOff,LeftMost,Partition_Array)

!** PURPOSE OF THIS SUBROUTINE
	! To sort the array in the range [Left,Right] in a desired order
    ! using recursive Quicksort algorithm with a tail recursion.  If the
    ! number of elements in the range below the cutoff, then a pair-insertion
    ! sorting routine is used instead.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(:)             ! the array to be sorted
    tIndex,    INTENT(IN)       :: Left             ! the start of the range
    tIndex,    INTENT(IN)       :: Right            ! the end of the range
    tIndex,    INTENT(IN)       :: CutOff           ! cutoff to pair insertion sort
    tLogical,  INTENT(IN)       :: LeftMost         ! flag indicating whether the specified range
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
                CALL DualInsert_Guarded(A, LStart, REnd)
            ELSE
                CALL DualInsert_UnGuarded(A, LStart, REnd)
            END IF
            EXIT
        END IF

        ! perform partitioning
        CALL Partition_Array(A, LStart, REnd, LEnd, RStart)

        ! recur on the smaller sub-array
        IF ((LEnd-LStart) < (REnd-RStart)) THEN
            CALL QuickSort_TailRecur(A, LStart, LEnd, CutOff, IsLeftMost, Partition_Array)
            LStart = RStart
            IsLeftMost = FalseVal
        ELSE
            CALL QuickSort_TailRecur(A, RStart, REnd, CutOff, FalseVal, Partition_Array)
            REnd = LEnd
            IsLeftMost = LeftMost
        END IF

    END DO

    RETURN

END SUBROUTINE QuickSort_TailRecur

!------------------------------------------------------------------------------
!                       MERGE SORT ROUTINES
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!                   MERGESORT DRIVER ROUTINES
!------------------------------------------------------------------------------

MODULE SUBROUTINE Merge_Sort_TopDown(A)

!^ **PURPOSE OF THIS SUBROUTINE**: <br>
!  To sort an array in a desired order using the *top-down merge sort*
!  algorithm [1].  The algorithm employed is actually a hybrid one where
!  the insertion sort algorithm is utilized for small (sub)array(s). <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] <a href="https://en.wikipedia.org/wiki/Merge_sort">Merge sort. </a> <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(:)   !! array to be sorted

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: Insertion_CutOff_Low  = 35   ! cutoff to pair insertion sort
    tInteger, PARAMETER :: Insertion_CutOff_High = 40   ! cutoff to pair insertion sort

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: CutOff       ! cutoff to pair insertion sort
    tIndex      :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(A, KIND=kIndex)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Insertion_CutOff_Low) THEN
        CALL Insert_Guarded(A, 1_kIndex, NA)
        RETURN
    END IF

    ! set cutoff depending on the size of the input array
    IF (NA < 2000_kIndex) THEN
        CutOff = Insertion_CutOff_Low
    ELSE
        CutOff = Insertion_CutOff_High
    END IF

    BLOCK
        ! block variable
#ifdef TYPE_IS_IN_Comparable_CLASS
        tVarAlloc   :: Buffer(:)
#else
        tVarLocal   :: Buffer(NA)
#endif
#ifdef TYPE_IS_IN_Comparable_CLASS
        ! allocate a buffer to use as scratch memory.
        ALLOCATE(Buffer(NA), MOLD=A)
#endif
        ! copy data from A
        Buffer = A
        ! perform merge sort
        CALL TopDown_Split_Merge(Buffer, 0_kIndex, NA, A, CutOff)
    END BLOCK

    RETURN

CONTAINS

    RECURSIVE SUBROUTINE TopDown_Split_Merge(B, Left, Right, A, CutOff)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To split A into 2 sub-arrays and sort both sub-arrays into B, and
        ! then merge both sub-arrays from B to A.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(0:)    ! array to be sorted
        tIndex,    INTENT(IN)       :: Left     ! the start of the range (inclusive)
        tIndex,    INTENT(IN)       :: Right    ! the end of the range (exclusive)
        tArgument, INTENT(INOUT)    :: B(0:)    ! buffer (auxiliary array)
        tIndex,    INTENT(IN)       :: CutOff   ! cutoff to pair insertion sort

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: NE, Mid

    !** FLOW:

        NE = Right - Left
        IF (NE > CutOff) THEN

            ! split the array longer than 1 item into halves.
            Mid = Left + (Right-Left)/2

            ! recursively sort both sub-arrays from array A into array B
            CALL TopDown_Split_Merge(A, Left, Mid,   B, CutOff)
            CALL TopDown_Split_Merge(A, Mid,  Right, B, CutOff)

            IF (COMPARE_GLE(B(Mid-1), B(Mid))) THEN
                ! array B has already been sorted so just copy it to array A
                A(Left:Right-1) = B(Left:Right-1)
            ELSE
                ! merge the resulting sub-arrays from array B into array A
                CALL Merging_Wiki(B, Left, Mid, Right, A)
            END IF

        ELSE
            CALL Insert_Guarded(A, Left+1, Right)
        END IF

        RETURN

    END SUBROUTINE TopDown_Split_Merge

    !**************************************************************************

END SUBROUTINE Merge_Sort_TopDown

!******************************************************************************

MODULE SUBROUTINE Merge_Sort_BottomUp(A)

!^ **PURPOSE OF THIS SUBROUTINE**: <br>
!  To sort an array in a desired order using the *bottom-up merge sort*
!  algorithm [1].  The algorithm employed is actually a hybrid one where
!  the insertion sort algorithm is utilized for small (sub)array(s). <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] <a href="https://en.wikipedia.org/wiki/Merge_sort">Merge sort. </a> <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(:)     !! array to be sorted

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: Insertion_CutOff_Low  = 20   ! cutoff to pair insertion sort
    tInteger, PARAMETER :: Insertion_CutOff_High = 50   ! cutoff to pair insertion sort

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: CutOff       ! cutoff to pair insertion sort
    tIndex      :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(A, KIND=kIndex)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Insertion_CutOff_Low) THEN
        CALL Insert_Guarded(A, 1_kIndex, NA)
        RETURN
    END IF

    ! set cutoff depending on the size of the input array
    IF (NA < 2000_kIndex) THEN
        CutOff = Insertion_CutOff_Low
    ELSE
        CutOff = Insertion_CutOff_High
    END IF

    BLOCK
        ! block variable
#ifdef TYPE_IS_IN_Comparable_CLASS
        tVarAlloc   :: Buffer(:)
#else
        tVarLocal   :: Buffer(NA)
#endif
#ifdef TYPE_IS_IN_Comparable_CLASS
        ! allocate a buffer to use as scratch memory.
        ALLOCATE(Buffer(NA), MOLD=A)
#endif
        CALL BottomUp_Merge(A, Buffer, NA, CutOff)
    END BLOCK

    RETURN

CONTAINS

    SUBROUTINE BottomUp_Merge(A, B, NE, Runs)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To split A into 2 sub-arrays and sort both sub-arrays into B, and
        ! then merge both sub-arrays from B to A.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(0:)    ! array to be sorted
        tArgument, INTENT(INOUT)    :: B(0:)    ! buffer (auxiliary array)
        tIndex,    INTENT(IN)       :: NE       ! size of the array
        tIndex,    INTENT(IN)       :: Runs     ! number of elements of sub-arrays to be sorted
                                                ! by pair insertion sort

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Left, Mid, Right
        tIndex      :: Width
        tLogical    :: Switch

    !** FLOW:

        ! sort individual sub-arrays of size Runs
        Left  = 1
        Right = MIN(Runs, NE)
        DO
            CALL Insert_Guarded(A, Left, Right)
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
                    IF (COMPARE_GLE(B(Mid-1), B(Mid))) THEN
                        ! Merely copy the two sub-arrays: B(Left:Mid-1) and B(Mid:Right-1) into A()
                        A(Left:Right-1) = B(Left:Right-1)
                    ELSE
                        ! Merge two sub-arrays: B(Left:Mid-1) and B(Mid:Right-1) into A()
                        CALL Merging_Wiki(B, Left, Mid, Right, A)
                    END IF
                ELSE
                    IF (COMPARE_GLE(A(Mid-1), A(Mid))) THEN
                        ! Merely copy the two sub-arrays: A(Left:Mid-1) and A(Mid:Right-1) into B()
                        B(Left:Right-1) = A(Left:Right-1)
                    ELSE
                        ! Merge two sub-arrays: A(Left:Mid-1) and A(Mid:Right-1) into B()
                        CALL Merging_Wiki(A, Left, Mid, Right, B)
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
        IF (Switch) A = B

        RETURN

    END SUBROUTINE BottomUp_Merge

    !**************************************************************************

END SUBROUTINE Merge_Sort_BottomUp

!******************************************************************************

MODULE SUBROUTINE Merge_Sort_QuadSplit(A)

!^ **PURPOSE OF THIS SUBROUTINE**: <br>
!  To sort an array in a desired order using the *top-down merge sort*
!  algorithm [1] where the given array is split into four sub-arrays
!  instead of two sub-arrays.  The algorithm employed is actually a hybrid
!  one where the insertion sort algorithm is utilized for small (sub)array(s). <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] <a href="https://en.wikipedia.org/wiki/Merge_sort">Merge sort. </a> <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(:)     !! array to be sorted

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: Insertion_CutOff_Low  = 35   ! cutoff to pair insertion sort
    tInteger, PARAMETER :: Insertion_CutOff_High = 40   ! cutoff to pair insertion sort

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: CutOff       ! cutoff to pair insertion sort
    tIndex      :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(A, KIND=kIndex)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Insertion_CutOff_Low) THEN
        CALL Insert_Guarded(A, 1_kIndex, NA)
        RETURN
    END IF

    ! set cutoff depending on the size of the input array
    IF (NA < 2000_kIndex) THEN
        CutOff = Insertion_CutOff_Low
    ELSE
        CutOff = Insertion_CutOff_High
    END IF

    BLOCK
        ! block variable
#ifdef TYPE_IS_IN_Comparable_CLASS
        tVarAlloc   :: Buffer(:)
#else
        tVarLocal   :: Buffer(NA)
#endif
#ifdef TYPE_IS_IN_Comparable_CLASS
        ! allocate a buffer to use as scratch memory.
        ALLOCATE(Buffer(NA), MOLD=A)
#endif
        CALL Quad_Split_Merge(A, 0_kIndex, NA, Buffer, CutOff)
    END BLOCK


    RETURN

CONTAINS

    RECURSIVE SUBROUTINE Quad_Split_Merge(A, Left, Right, B, CutOff)

    !** PURPOSE OF THIS SUBROUTINE
	    ! To split A into 4 sub-arrays and sort them into B, and
        ! then merge the four sub-arrays from B to A.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(0:)    ! array to be sorted
        tIndex,    INTENT(IN)       :: Left     ! the start of the range
        tIndex,    INTENT(IN)       :: Right    ! the end of the range
        tArgument, INTENT(INOUT)    :: B(0:)    ! buffer (auxiliary array)
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
            CALL Quad_Split_Merge(A, Left,     LeftMid,  B, CutOff)
            CALL Quad_Split_Merge(A, LeftMid,  Mid,      B, CutOff)
            CALL Quad_Split_Merge(A, Mid,      RightMid, B, CutOff)
            CALL Quad_Split_Merge(A, RightMid, Right,    B, CutOff)

            IF (COMPARE_GLE(A(LeftMid-1), A(LeftMid))) THEN
                ! merely copy the resulting sub-arrays into array B
                B(Left:Mid-1) = A(Left:Mid-1)
            ELSE
                ! merge the resulting sub-arrays into array B
                CALL Merging_Wiki(A, Left, LeftMid, Mid, B)
            END IF
            IF (COMPARE_GLE(A(RightMid-1), A(RightMid))) THEN
                ! merely copy the resulting sub-arrays into array B
                B(Mid:Right-1) = A(Mid:Right-1)
            ELSE
                ! merge the resulting sub-arrays into array B
                CALL Merging_Wiki(A, Mid, RightMid, Right, B)
            END IF

            IF (COMPARE_GLE(B(Mid-1), B(Mid))) THEN
                ! array B has already been sorted so just copy it to array A
                A(Left:Right-1) = B(Left:Right-1)
            ELSE
                ! merge the sub-arrays of array B into array A
                CALL Merging_Wiki(B, Left, Mid, Right, A)
            END IF
        ELSE
            CALL Insert_Guarded(A, Left+1, Right)
        END IF

        RETURN

    END SUBROUTINE Quad_Split_Merge

    !**************************************************************************

END SUBROUTINE Merge_Sort_QuadSplit

!******************************************************************************

MODULE SUBROUTINE Merge_Sort_HalfCopy(A)

!^ **PURPOSE OF THIS SUBROUTINE**: <br>
!  To sort an array in a desired order using a fast *mergesort* algorithm
!  based on *half-copying merge* algorithm by C. Juszczak [1].  The algorithm
!  employed is actually a hybrid one where the insertion sort algorithm is
!  utilized for small (sub)array(s). <br>
!    <br>
!  **REFERENCES**: <br>
!   [1] <a href="http://kicia.ift.uni.wroc.pl/algorytmy/mergesortpaper.pdf">
!       Juszczak, C. 2007.  Fast mergesort implementation based on half-copying
!       merge algorithm. </a> <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(:)     !! array to be sorted

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: Insertion_CutOff_Low  = 25   ! cutoff to pair insertion sort
    tInteger, PARAMETER :: Insertion_CutOff_High = 55   ! cutoff to pair insertion sort

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: N2           ! size of the buffer array
    tIndex      :: CutOff       ! cutoff to pair insertion sort
    tIndex      :: NA           ! size of the arrays

!** FLOW:

    ! get array size
    NA = SIZE(A, KIND=kIndex)

    ! simply return if size is one or less
    IF (NA <= 1_kIndex) RETURN

    ! check whether to perform pair insertion sort instead
    IF (NA <= Insertion_CutOff_Low) THEN
        CALL Insert_Guarded(A, 1_kIndex, NA)
        RETURN
    END IF

    ! set cutoff depending on the size of the input array
    IF (NA < 2000_kIndex) THEN
        CutOff = Insertion_CutOff_Low
    ELSE
        CutOff = Insertion_CutOff_High
    END IF

    ! get buffer size
    N2 = NA - NA/2_kIndex

    BLOCK
#ifdef TYPE_IS_IN_Comparable_CLASS
        tVarAlloc   :: Buffer(:)
#else
        tVarLocal   :: Buffer(N2)
#endif
#ifdef TYPE_IS_IN_Comparable_CLASS
        ! allocate a buffer to use as scratch memory.
        ALLOCATE(Buffer(N2), MOLD=A)
#endif
        CALL Main_Split_Merging(A, 0_kIndex, NA, Buffer, CutOff)
    END BLOCK

    RETURN

CONTAINS

    SUBROUTINE Main_Split_Merging(A, P, K, B, CutOff)

    !** PURPOSE OF THIS SUBROUTINE
        ! For small arrays, the pair-insertion sort is used as it is the
        ! fastest sorting algorithm for small N. This also ensures that
        ! the merge is never called with empty regions [P1,K1) or [P2,K2).
        ! The routine calls the 'HalfCopy_Split_Merge' routine. It sorts the
        ! input range [P,K) 'in place' but requires a buffer to temporarily
        ! store N/2 array elements.

    !** REFERENCES
        ! Adapted from routine 'MergeSort' in the reference.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(0:)    ! working array
        tIndex,    INTENT(IN)       :: P        ! the start (inclusive) of the range of array to be sorted
        tIndex,    INTENT(IN)       :: K        ! the end (exclusive) of the range of array to be sorted
        tArgument, INTENT(INOUT)    :: B(0:)    ! working array
        tIndex,    INTENT(IN)       :: CutOff   ! cutoff to pair insertion sort

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: NE, S, T, KMS

    !** FLOW:

        NE = K - P
        IF (NE > CutOff) THEN
            ! split the array into halves.
            S = P + NE/2
            T = K-(S-P)
            ! CopyMerge(A2, B)
            CALL HalfCopy_Split_Merge(A, S, K, B, 0, CutOff)
            ! CopyMerge(A1, A2)
            CALL HalfCopy_Split_Merge(A, P, S, A, T, CutOff)

            KMS = K - S
            IF (COMPARE_GLE(B(KMS-1), A(T))) THEN
                ! just copy array B to the first half of array A
                A(T-KMS:T-1) = B(0:KMS-1)
            ELSE
                ! merge the resulting sub-arrays
                ! MergeR(B, A2, A)
                CALL MergingR_HalfCopy(B, 0, KMS, A, T, K)
            END IF
        ELSE
            CALL Insert_Guarded(A, P+1, K)
        END IF

        RETURN

    END SUBROUTINE Main_Split_Merging

    !**********************************************************************************

    RECURSIVE SUBROUTINE HalfCopy_Split_Merge(A, P, K, B, T, CutOff)

    !** PURPOSE OF THIS SUBROUTINE
        ! Based on the half-copying algorithm, a very efficient copying version
        ! of Mergesort may be written sorting the elements form the range [P,K)
        ! and placing the result in the range [T,T+(K-P)). The right half of the
        ! input is sorted (by using recursively the same algorithm) into the right
        ! half of the output range. Then, the left half of the input is range
        ! sorted (also recursively) into the right half of the input range. Then,
        ! it is merged with our half copying merge with the numbers already present
        ! in the output.

    !** REFERENCES
        ! Adapted from routine 'Copying_MergeSort' in the reference.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tArgument, INTENT(INOUT)    :: A(0:)    ! working array
        tIndex,    INTENT(IN)       :: P        ! the start (inclusive) of the range of array to be sorted
        tIndex,    INTENT(IN)       :: K        ! the end (exclusive) of the range of array to be sorted
        tArgument, INTENT(INOUT)    :: B(0:)    ! working array
        tIndex,    INTENT(IN)       :: T        ! the start (inclusive) of the range of array to be sorted
        tIndex,    INTENT(IN)       :: CutOff   ! cutoff to pair insertion sort

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: NE, S, S1, T1

    !** FLOW:

        NE = K - P
        IF (NE > CutOff) THEN
            ! split the array into halves.
            S = P + (K-P)/2
            ! CopyMerge(A2, B2)
            CALL HalfCopy_Split_Merge(A, S, K, B, T+(S-P), CutOff)
            ! CopyMerge(A1, B2)
            CALL HalfCopy_Split_Merge(A, P, S, A, S,       CutOff)

            S1 = S + (S-P)
            T1 = T+(S-P)
            IF (COMPARE_GLE(A(S1-1), B(T1))) THEN
                ! just copy array A to the first half of array B
                B(T1-(S1-S):T1-1) = A(S:S1-1)
            ELSE
                ! merge the resulting sub-arrays
                ! Merge(A2, B2, B)
                CALL Merging_HalfCopy(A, S, S1, B, T1, T+(K-P))
            END IF
        ELSE
            ! copy data from A to B
            B(T:T+NE-1) = A(P:K-1)

            ! perform pair-insertion sort on B
            CALL Insert_Guarded(B, T+1, T+NE)
        END IF

        RETURN

    END SUBROUTINE HalfCopy_Split_Merge

    !**********************************************************************************

END SUBROUTINE Merge_Sort_HalfCopy

!------------------------------------------------------------------------------
!                   MERGING ROUTINES
!------------------------------------------------------------------------------

SUBROUTINE Merging_HalfCopy(A, S1, K1, B, S2, K2)

!** PURPOSE OF THIS SUBROUTINE
    ! To merge two sub-arrays in the [P1,K1) and [P2,K2) sorted ranges.
    ! The pointer K2 coincides with the end of the target range [P,K)
    ! meaning that no element in [P2,K2) needs to be moved if all
    ! elements from [S1,K1) are smaller than B(P2). It is assumed that
    ! both ranges are non empty. When the range [P1,K1) becomes exhausted
    ! the merging is done because all remaining elements from [P2,K2) are
    ! already in place.

!** REFERENCES
    ! Adapted from routine 'Merge' in the following reference:
    ! Juszczak, C. 2007.  "Fast Mergesort Implementation Based on Half-Copying Merge Algorithm"

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: A(0:)    ! working array
    tIndex,    INTENT(IN)       :: S1       ! the start of the first sub-array (inclusive)
    tIndex,    INTENT(IN)       :: K1       ! the end of the first sub-array (exclusive)
    tArgument, INTENT(INOUT)    :: B(0:)    ! working array
    tIndex,    INTENT(IN)       :: S2       ! the start of the second sub-array (inclusive)
    tIndex,    INTENT(IN)       :: K2       ! the end of the second sub-array (exclusive)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: P1, P2, P

!** FLOW:

    ! initialize
    P1 = S1
    P2 = S2

    ! calculate the beginning of the output
    P = P2 - (K1-P1)

    DO
        IF (COMPARE_GLE(A(P1), B(P2))) THEN
            B(P) = A(P1)
            P = P + 1
            P1 = P1 + 1
            IF (P1 == K1) RETURN
        ELSE
            B(P) = B(P2)
            P = P + 1
            P2 = P2 + 1
            IF (P2 == K2) EXIT
        END IF
    END DO
    DO
        B(P) = A(P1)
        P = P + 1
        P1 = P1 + 1
        IF (P1 == K1) EXIT
    END DO

    RETURN

END SUBROUTINE Merging_HalfCopy

!******************************************************************************

SUBROUTINE MergingR_HalfCopy(B, S1, K1, A, S2, K2)

!** PURPOSE OF THIS SUBROUTINE
    ! See purpose of the routine in 'Merging_HalfCopy' subroutine.
    ! To preserve the stability of MergeSort algorithm, this
    ! routine is a slightly changed version of Merging_HalfCopy routine
    ! It assumes that the range [P2,K2) was to the left of [K1,P1)
    ! in the input and takes this into account when the compared
    ! elements turn out to be equal (those from [P2,K2) are first
    ! copied to the output).

!** REFERENCES
    ! Adapted from routine 'MergeR' in the following reference:
    ! Juszczak, C. 2007.  "Fast Mergesort Implementation Based on Half-Copying Merge Algorithm"

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(INOUT)    :: B(0:)    ! working array
    tIndex,    INTENT(IN)       :: S1       ! the start of the first sub-array (inclusive)
    tIndex,    INTENT(IN)       :: K1       ! the end of the first sub-array (exclusive)
    tArgument, INTENT(INOUT)    :: A(0:)    ! working array
    tIndex,    INTENT(IN)       :: S2       ! the start of the second sub-array (inclusive)
    tIndex,    INTENT(IN)       :: K2       ! the end of the second sub-array (exclusive)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: P1, P2, P

!** FLOW:

    ! initialize
    P1 = S1
    P2 = S2

    ! calculate the beginning of the output
    P = P2 - (K1-P1)
    DO
        IF (COMPARE_GLT(B(P1), A(P2))) THEN
            A(P) = B(P1)
            P = P + 1
            P1 = P1 + 1
            IF (P1 == K1) RETURN
        ELSE
            A(P) = A(P2)
            P = P + 1
            P2 = P2 + 1
            IF (P2 == K2) EXIT
        END IF
    END DO
    DO
        A(P) = B(P1)
        P = P + 1
        P1 = P1 + 1
        IF (P1 == K1) EXIT
    END DO

    RETURN

END SUBROUTINE MergingR_HalfCopy

!******************************************************************************

SUBROUTINE Merging_Wiki(A, Left, Mid, Right, B)

!** PURPOSE OF THIS SUBROUTINE
    ! To merge array where
    !   left source half is  A(Left:Mid-1),
    !   right source half is A(Mid:Right-1), and
    !   result is            B(Left:Right-1).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tArgument, INTENT(IN)       :: A(0:)
    tIndex,    INTENT(IN)       :: Left
    tIndex,    INTENT(IN)       :: Mid
    tIndex,    INTENT(IN)       :: Right
    tArgument, INTENT(INOUT)    :: B(0:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, J, K

!** FLOW:

    ! initialize
    I = Left
    J = Mid

    ! While there are elements in the left or right runs
    K = Left
    DO WHILE (K < Right)
        ! If left run head exists and is <= existing right run head.
        IF ((I < Mid).AND.((J >= Right).OR.(COMPARE_GLE(A(I), A(J))))) THEN
            B(K) = A(I)
            I = I + 1
        ELSE
            B(K) = A(J)
            J = J + 1
        END IF
        K = K + 1
    END DO

    RETURN

END SUBROUTINE Merging_Wiki

!******************************************************************************

#undef MaximumDepth
#undef tArgument
#undef tVarAlloc
#undef tVarScalar
#undef tVarSpec
#undef COMPARE_GLT
#undef COMPARE_GLE
#undef Is_Array_Sorted
#undef Wise_Sort_Unstable
#undef Wise_Sort_Stable
#undef Intro_Sort
#undef Java_Sort
#undef PDQ_Sort
#undef Tim_Sort
#undef Rust_Sort
#undef Quick_Sort_Hoare
#undef Quick_Sort_Lomuto
#undef Quick_Sort_Mo3
#undef Quick_Sort_3Way
#undef Quick_Sort_Vowels
#undef Quick_Sort_Stable
#undef Quick_Sort_Iterative
#undef Quick_Sort_Java
#undef Merge_Sort_TopDown
#undef Merge_Sort_BottomUp
#undef Merge_Sort_QuadSplit
#undef Merge_Sort_HalfCopy

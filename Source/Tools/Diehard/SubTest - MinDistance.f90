
SUBMODULE (ModTest_Diehard) SubTest_MinDistance

!** PURPOSE OF THIS MODULE:
    ! contains routines to perform "Minimum Distance" test.

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tCharLen(80), PARAMETER :: TestDescription(13) = [                              &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", &
        "     ::               THE MINIMUM DISTANCE TEST                       ::", &
        "     :: It does this 100 times::   choose n=8000 random points in a   ::", &
        "     :: square of side 10000.  Find d, the minimum distance between   ::", &
        "     :: the (n^2-n)/2 pairs of points.  If the points are truly inde- ::", &
        "     :: pendent uniform, then d^2, the square of the minimum distance ::", &
        "     :: should be (very close to) exponentially distributed with mean ::", &
        "     :: .995 .  Thus 1-exp(-d^2/.995) should be uniform on [0,1) and  ::", &
        "     :: a KSTEST on the resulting 100 values serves as a test of uni- ::", &
        "     :: formity for random points in the square. Test numbers=0 mod 5 ::", &
        "     :: are printed but the KSTEST is based on the full set of 100    ::", &
        "     :: random choices of 8000 points in the 10000x10000 square.      ::", &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::"]

!** DERIVED TYPE DEFINITIONS
    ! na
    
!** INTERFACE DEFINITIONS
    ! na
    
!** GENERIC DECLARATIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na
    
    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE SUBROUTINE FT10_MinDistance_FromFile(InpFileName, OutUnit, InpRec)

    ! To perform the "Minimum Distance" test based on the specified input file

    ! arguments
    tCharLen(25),   INTENT(IN)      :: InpFileName
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(I32Data), INTENT(INOUT)   :: InpRec
    
    ! local variables
    tSingle         :: X(8000), G(100), Y(8000)
    tInteger        :: I, IJ, J, NS
    tSingle         :: D, DMin, P, Sum, U, V
    
    ! execution

    ! write test description
    WRITE (*, 5000)       TestDescription(1:13)
    WRITE (OutUnit, 5000) TestDescription(1:13)

    !  Minimum distance^2 between n  random points(x(i), y(i)).
    !  Mean is about .64 for 4000 points in a square of side 1000.
    !  and .995 for 8000 points in a square of side 10000.
    !  Since distance^2 is approximately exponential with mean .04,
    !  1.-exp(-d^2/.04) should be uniform on [0,1).  Thus a KS test.

    NS = 100
    InpRec%RecNum = 1
    WRITE (*, 5100)       InpFileName
    WRITE (OutUnit, 5100) InpFileName

    Sum = 0.0
    DO  IJ = 1, NS
        DMin = 10000000.0
        DO  I = 1, 8000
            X(I) = 5000.0 + InpRec%GetSample() * 0.2328306E-5
            Y(I) = 5000.0 + InpRec%GetSample() * 0.2328306E-5
        END DO

        ! There was a crazy call to DSORT here in the F77 version.
        ! The double precision numbers in array QQ were obtained by
        ! equivalencing QQ to two consecutive reals from array XY!
        CALL SqSort(X, 8000, Y)

        DO  I = 1, 7999
            U = X(I)
            V = Y(I)
            DO  J = I+1, 8000

                D = (U-X(J)) ** 2 + (V-Y(J)) ** 2
                DMin = MIN(D, DMin)

            END DO
        END DO

        D = DMin
        Sum = Sum + D

        G(IJ) = 1.0 - EXP(-DMin/0.995)
        IF (MOD(IJ, 5) == 0) THEN
            WRITE (OutUnit, 5200) IJ, D, Sum / IJ, G(IJ)
            WRITE (*, 5200)       IJ, D, Sum / IJ, G(IJ)
        END IF
    END DO
    WRITE (OutUnit, 5300) InpFileName
    WRITE (*, 5300)       InpFileName
    WRITE (OutUnit, 5400)
    WRITE (*, 5400)
    CALL KS_Test(G, NS, P)
    WRITE (OutUnit, 5500) P
    WRITE (*, 5500)       P
    WRITE (OutUnit, 5600)
    WRITE (*, 5600)

    RETURN

5000 FORMAT (A78)
5100 FORMAT ('               This is the MINIMUM DISTANCE test'/   &
             '              for ranDom integers in the file ', A15/   &
             T5, ' Sample no.    d^2     avg     equiv uni            ')
5200 FORMAT (I12, F10.4, F9.4, F12.6)
5300 FORMAT ('     MINIMUM DISTANCE TEST for ', A20)
5400 FORMAT (T11, 'Result of KS test on 20 transformed mindist^2''S:')
5500 FORMAT (T11, '                        p-value=', F8.6)
5600 FORMAT (/  '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/)
     
END SUBROUTINE FT10_MinDistance_FromFile

!******************************************************************************

MODULE SUBROUTINE FT10_MinDistance_FromRNG(OutUnit, RNG)

    ! To perform the "Minimum Distance" test based on the specified generator

    ! arguments
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG
    
    ! local variables
    tCharAlloc      :: RngName
    tSingle         :: X(8000), G(100), Y(8000)
    tInteger        :: I, IJ, J, NS
    tSingle         :: D, DMin, P, Sum, U, V
    
    ! execution

    RngName = RNG%GetName()

    ! write test description
    WRITE (*, 5000)       TestDescription(1:13)
    WRITE (OutUnit, 5000) TestDescription(1:13)

    !  Minimum distance^2 between n  random points(x(i), y(i)).
    !  Mean is about .64 for 4000 points in a square of side 1000.
    !  and .995 for 8000 points in a square of side 10000.
    !  Since distance^2 is approximately exponential with mean .04,
    !  1.-exp(-d^2/.04) should be uniform on [0,1).  Thus a KS test.

    NS = 100
    CALL RNG%ReInit()
    WRITE (*, 5100)       RngName
    WRITE (OutUnit, 5100) RngName

    Sum = 0.0
    DO  IJ = 1, NS
        DMin = 10000000.0
        DO  I = 1, 8000
            X(I) = 5000.0 + GetSample(RNG) * 0.2328306E-5
            Y(I) = 5000.0 + GetSample(RNG) * 0.2328306E-5
        END DO

        ! There was a crazy call to DSORT here in the F77 version.
        ! The double precision numbers in array QQ were obtained by
        ! equivalencing QQ to two consecutive reals from array XY!
        CALL SqSort(X, 8000, Y)

        DO  I = 1, 7999
            U = X(I)
            V = Y(I)
            DO  J = I+1, 8000

                D = (U-X(J)) ** 2 + (V-Y(J)) ** 2
                DMin = MIN(D, DMin)

            END DO
        END DO

        D = DMin
        Sum = Sum + D

        G(IJ) = 1.0 - EXP(-DMin/0.995)
        IF (MOD(IJ, 5) == 0) THEN
            WRITE (OutUnit, 5200) IJ, D, Sum / IJ, G(IJ)
            WRITE (*, 5200)       IJ, D, Sum / IJ, G(IJ)
        END IF
    END DO
    WRITE (OutUnit, 5300) RngName
    WRITE (*, 5300)       RngName
    WRITE (OutUnit, 5400)
    WRITE (*, 5400)
    CALL KS_Test(G, NS, P)
    WRITE (OutUnit, 5500) P
    WRITE (*, 5500)       P
    WRITE (OutUnit, 5600)
    WRITE (*, 5600)

    RETURN

5000 FORMAT (A78)
5100 FORMAT ('               This is the MINIMUM DISTANCE test'/   &
             '              for ranDom integers in the file ', A15/   &
             T5, ' Sample no.    d^2     avg     equiv uni            ')
5200 FORMAT (I12, F10.4, F9.4, F12.6)
5300 FORMAT ('     MINIMUM DISTANCE TEST for ', A20)
5400 FORMAT (T11, 'Result of KS test on 20 transformed mindist^2''S:')
5500 FORMAT (T11, '                        p-value=', F8.6)
5600 FORMAT (/  '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/)
     
END SUBROUTINE FT10_MinDistance_FromRNG

!******************************************************************************

SUBROUTINE SqSort_Quick(A, N, T)
 
    ! To sort array A and its associative array T

    ! arguments
    tSingle,  INTENT(INOUT)     :: A(:)
    tInteger, INTENT(IN)        :: N
    tSingle,  INTENT(INOUT)     :: T(:)
    
    ! general parameters for quick sort    
    tInteger, PARAMETER :: Quick_Insert_CutOff_Low  = 40    ! cutoff to pair insertion
    tInteger, PARAMETER :: Quick_Insert_CutOff_High = 70    ! cutoff to pair insertion
    
    ! local variables
    tIndex              :: iStart, iEnd
    tIndex              :: CutOff
    tIndex              :: NA

    ! execution
    
    ! initialize
    NA = N
    iStart = 1
    iEnd = NA
    IF (NA < 2000) THEN
        CutOff = Quick_Insert_CutOff_Low
    ELSE
        CutOff = Quick_Insert_CutOff_High
    END IF
    
    ! perform quick sort
    CALL QuickSort(A, T, iStart, iEnd, CutOff)
    
    RETURN
    
CONTAINS

#define COMPARE_GLT(A, B)           (A < B)
#define COMPARE_GLE(A, B)           (A <= B)

    SUBROUTINE QuickSort(A, T, Left, Right, CutOff)

        IMPLICIT NONE            ! Enforce explicit typing of all variables in this routine
   
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSingle,  INTENT(INOUT) :: A(:)
        tSingle,  INTENT(INOUT) :: T(:)
        tIndex,   INTENT(IN)    :: Left
        tIndex,   INTENT(IN)    :: Right
        tIndex,   INTENT(IN)    :: CutOff

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSingle         :: Pivot, Temp, TPivot
        tIndex          :: I, J, K, L, R
        tIndex          :: Ptr
        tIndex          :: Stack(2, STORAGE_SIZE(NA))

    !** FLOW:

        ! initialize
        Ptr = 1
        L = Left
        R = Right
        
        ! perform quick sort
        DO
            IF (R-L < CutOff) THEN
                ! perform insertion sort when subarray small enough
                IF (L == Left) THEN
                    ! left-most subarray
                    CALL DualInsert_Guarded(A, T, L, R)
                ELSE
                    ! not left-most subarray
                    CALL DualInsert_UnGuarded(A, T, L, R)
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
            EXCHANGE(T, K, L+1)
            IF (COMPARE_GLT(A(R), A(L))) THEN
                EXCHANGE(A, L, R)
                EXCHANGE(T, L, R)
            END IF
            IF (COMPARE_GLT(A(R), A(L+1))) THEN
                EXCHANGE(A, L+1, R)
                EXCHANGE(T, L+1, R)
            END IF
            IF (COMPARE_GLT(A(L+1), A(L))) THEN
                EXCHANGE(A, L, L+1)
                EXCHANGE(T, L, L+1)
            END IF
            ! initialize pointers for partitioning
            I = L+1
            J = R
            ! partitioning element
            Pivot = A(L+1)
            TPivot = T(L+1)
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
                EXCHANGE(T, I, J)
            END DO
            ! insert partitioning element
            A(L+1) = A(J)
            A(J)   = Pivot
            T(L+1) = T(J)
            T(J)   = TPivot
            ! push pointers to larger subarray on stack and process smaller subarray immediately.
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
    
    END SUBROUTINE QuickSort

    !******************************************************************

    SUBROUTINE DualInsert_Guarded(A, T, Lo, Hi)

        !DIR$ ATTRIBUTES INLINE :: DualInsert_Guarded

    !** PURPOSE OF THIS SUBROUTINE
        ! To sort array using pair-insertion sort algorithm

    !** REFERENCES
        ! Adapted from a piece of code from Java's JDK7

        IMPLICIT NONE            ! Enforce explicit typing of all variables in this routine
   
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSingle,  INTENT(INOUT) :: A(:)     ! array to be sorted
        tSingle,  INTENT(INOUT) :: T(:)     ! array to be sorted
        tIndex,   INTENT(IN)    :: Lo       ! starting index (inclusive)
        tIndex,   INTENT(IN)    :: Hi       ! ending index (inclusive)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSingle     :: Pivot1, Pivot2, Last
        tSingle     :: PivotT1, PivotT2, TLast
        tIndex      :: LPtr, RPtr, Indx

    !** FLOW:

        ! initialize
        LPtr = Lo
        RPtr = Hi
        
        ! Skip the longest (ascending/descending) sequence
        DO
            IF (LPtr >= RPtr) RETURN
            LPtr = LPtr + 1
            IF (COMPARE_GLT(A(LPtr), A(LPtr-1))) EXIT
        END DO
    
        ! perform optimized dual-insertion sort
        Indx = LPtr
        LPtr = LPtr + 1
        DO WHILE (LPtr <= RPtr)

            ! set pivots used for insertions where Pivot1 > Pivot2
            IF (COMPARE_GLT(A(Indx), A(LPtr))) THEN
                Pivot1 = A(LPtr)
                Pivot2 = A(Indx)
                PivotT1 = T(LPtr)
                PivotT2 = T(Indx)
            ELSE
                Pivot1 = A(Indx)
                Pivot2 = A(LPtr)
                PivotT1 = T(Indx)
                PivotT2 = T(LPtr)
            END IF
        
            ! K is the index used to find an insertion point
            Indx = Indx - 1
        
            ! find an insertion point for Pivot1 and shift existing
            ! content by 2
            DO WHILE ((Indx >= Lo).AND.(COMPARE_GLT(Pivot1, A(Indx))))
                A(Indx+2) = A(Indx)
                T(Indx+2) = T(Indx)
                Indx = Indx - 1
            END DO
        
            ! store Pivot1 at its insertion place
            A(Indx+2) = Pivot1
            T(Indx+2) = PivotT1

            ! A(K+1) is an available space now so  find an insertion point
            ! for Pivot2 and shift existing content by 1
            DO WHILE ((Indx >= Lo).AND.(COMPARE_GLT(Pivot2, A(Indx))))
                A(Indx+1) = A(Indx)
                T(Indx+1) = T(Indx)
                Indx = Indx - 1
            END DO

            ! store Pivot2 at its insertion place
            A(Indx+1) = Pivot2
            T(Indx+1) = PivotT2

            ! update indices
            Indx = LPtr + 1
            LPtr = Indx + 1
        END DO

        ! perform single insertion for the last element
        Last = A(RPtr)
        TLast = T(RPtr)
        RPtr = RPtr - 1
        DO WHILE ((RPtr >= Lo).AND.(COMPARE_GLT(Last, A(RPtr))))
            A(RPtr+1) = A(RPtr)
            T(RPtr+1) = T(RPtr)
            RPtr = RPtr - 1
        END DO
        A(RPtr+1) = Last
        T(RPtr+1) = TLast
                
        RETURN

    END SUBROUTINE DualInsert_Guarded

    !*********************************************************************

    SUBROUTINE DualInsert_UnGuarded(A, T, Lo, Hi)

        !DIR$ ATTRIBUTES INLINE :: DualInsert_UnGuarded

    !** PURPOSE OF THIS SUBROUTINE
        ! To sort array using pair-insertion sort algorithm without checking
        ! whether the array index is out of bound or not

    !** REFERENCES
        ! Adapted from a piece of code from Java's JDK7

        IMPLICIT NONE            ! Enforce explicit typing of all variables in this routine
   
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSingle,  INTENT(INOUT) :: A(:)     ! array to be sorted
        tSingle,  INTENT(INOUT) :: T(:)     ! array to be sorted
        tIndex,   INTENT(IN)    :: Lo       ! starting index (inclusive)
        tIndex,   INTENT(IN)    :: Hi       ! ending index (inclusive)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSingle     :: Pivot1, Pivot2, Last
        tSingle     :: PivotT1, PivotT2, TLast
        tIndex      :: LPtr, RPtr, Indx

    !** FLOW:

        ! initialize
        LPtr = Lo
        RPtr = Hi
        
        ! Skip the longest (ascending/descending) sequence
        DO
            IF (LPtr >= RPtr) RETURN
            LPtr = LPtr + 1
            IF (COMPARE_GLT(A(LPtr), A(LPtr-1))) EXIT
        END DO
    
        ! perform optimized dual-insertion sort
        Indx = LPtr
        LPtr = LPtr + 1
        DO WHILE (LPtr <= RPtr)

            ! set pivots used for insertions where Pivot1 > Pivot2
            IF (COMPARE_GLT(A(Indx), A(LPtr))) THEN
                Pivot1 = A(LPtr)
                Pivot2 = A(Indx)
                PivotT1 = T(LPtr)
                PivotT2 = T(Indx)
            ELSE
                Pivot1 = A(Indx)
                Pivot2 = A(LPtr)
                PivotT1 = T(Indx)
                PivotT2 = T(LPtr)
            END IF
        
            ! K is the index used to find an insertion point
            Indx = Indx - 1
        
            ! find an insertion point for Pivot1 and shift existing
            ! content by 2
            DO WHILE (COMPARE_GLT(Pivot1, A(Indx)))
                A(Indx+2) = A(Indx)
                T(Indx+2) = T(Indx)
                Indx = Indx - 1
            END DO
        
            ! store Pivot1 at its insertion place
            A(Indx+2) = Pivot1
            T(Indx+2) = PivotT1

            ! A(K+1) is an available space now so  find an insertion point
            ! for Pivot2 and shift existing content by 1
            DO WHILE (COMPARE_GLT(Pivot2, A(Indx)))
                A(Indx+1) = A(Indx)
                T(Indx+1) = T(Indx)
                Indx = Indx - 1
            END DO

            ! store Pivot2 at its insertion place
            A(Indx+1) = Pivot2
            T(Indx+1) = PivotT2

            ! update indices
            Indx = LPtr + 1
            LPtr = Indx + 1
        END DO

        ! perform single insertion for the last element
        Last = A(RPtr)
        TLast = T(RPtr)
        RPtr = RPtr - 1
        DO WHILE (COMPARE_GLT(Last, A(RPtr)))
            A(RPtr+1) = A(RPtr)
            T(RPtr+1) = T(RPtr)
            RPtr = RPtr - 1
        END DO
        A(RPtr+1) = Last
        T(RPtr+1) = TLast
                
        RETURN

    END SUBROUTINE DualInsert_UnGuarded

    !*********************************************************************

#undef COMPARE_GLT
#undef COMPARE_GLE

END SUBROUTINE SqSort_Quick

!******************************************************************************

SUBROUTINE SqSort(A, N, T)
 
    ! To sort array A and its associative array T

    ! arguments
    tSingle,  INTENT(INOUT)     :: A(:)
    tInteger, INTENT(IN)        :: N
    tSingle,  INTENT(INOUT)     :: T(:)
    
    ! local variables
    tIndex      :: Indx(N), I
    tSingle     :: UnsortedArray(N)

    ! execution
    
    ! get indices for sorting
    CALL WiseRank(A(1:N), Indx(1:N))
    
    ! sort A
    UnsortedArray = A
    DO I = 1, N
        A(I) = UnsortedArray(Indx(I))
    END DO

    ! sort T
    UnsortedArray = T
    DO I = 1, N
        T(I) = UnsortedArray(Indx(I))
    END DO
    
    RETURN

END SUBROUTINE SqSort

!******************************************************************************

END SUBMODULE SubTest_MinDistance

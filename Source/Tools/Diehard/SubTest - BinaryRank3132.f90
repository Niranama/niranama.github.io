
SUBMODULE (ModTest_Diehard) SubTest_BinaryRank3132

!** PURPOSE OF THIS MODULE:
    ! contains routines to perform 'Binary Rank' test for 31x31 and 32x32 matrices.

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tCharLen(80), PARAMETER :: TestDescription(18) = [                              &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", &
        "     :: This is the BINARY RANK TEST for 31x31 matrices. The leftmost ::", &
        "     :: 31 bits of 31 random integers from the test sequence are used ::", &
        "     :: to form a 31x31 binary matrix over the field {0,1}. The rank  ::", &
        "     :: is determined. That rank can be from 0 to 31, but ranks< 28   ::", &
        "     :: are rare, and their counts are pooled with those for rank 28. ::", &
        "     :: Ranks are found for 40,000 such random matrices and a chisqua-::", &
        "     :: re test is performed on counts for ranks 31,30,29 and <=28.   ::", &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", &
        "     :: This is the BINARY RANK TEST for 32x32 matrices. A random 32x ::", &
        "     :: 32 binary matrix is formed, each row a 32-bit random integer. ::", &
        "     :: The rank is determined. That rank can be from 0 to 32, ranks  ::", &
        "     :: less than 29 are rare, and their counts are pooled with those ::", &
        "     :: for rank 29.  Ranks are found for 40,000 such random matrices ::", &
        "     :: and a chisquare test is performed on counts for ranks  32,31, ::", &
        "     :: 30 and <=29.                                                  ::", &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::"]
    tSingle,      PARAMETER :: P(0:3) =                         &
        [0.2887880952, 0.5775761902, 0.1283502644, 0.0052854502]

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

MODULE SUBROUTINE FT03_BinaryRank_3132_FromFile(InpFileName, OutUnit, InpRec)

    ! To perform the 'BinaryRank_3132' Test based on the specified input file

    ! arguments
    tCharLen(25),   INTENT(IN)      :: InpFileName
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(I32Data), INTENT(INOUT)   :: InpRec
    
    ! local variables
    tInteger        :: I, IJ, IR, K, M, N, NTries, NTry
    tInteger        :: Row(32), Table(0:3)
    tSingle         :: D, E, S
    
    ! execution

    ! write test description
    WRITE (*, 5000)       TestDescription(1:18)
    WRITE (OutUnit, 5000) TestDescription(1:18)

    ! see original file \f\bprint.for that displays each step in the
    !  Rank reduction.
    !  finds Rank of 31x31 and 32x32 matrices.
    ! For the 31x31, uses 31 leftmost bits of a 32-bit integer
    ! to form a Row of the binary matrix.
    ! For the 32x32, uses 32 full integer words for each of 32 Rows
    !      function mRank(r,m,n)
    !   for nxn matrices, to at least 6 places,
    !  the probability of Rank n-2,n-1,n are all vIRtually the same.
    !    r          p
    !  <=29    .0052854502
    !    30    .1283502644
    !    31    .5775761902
    !    32    .2887880952
    ! **** Finds binary Rank of M Rows, N trailing bits each ****
    
    DO  M = 31, 32
        InpRec%RecNum = 1
        IF (M == 31) WRITE (*, 5000)       TestDescription(1:9)
        IF (M == 31) WRITE (OutUnit, 5000) TestDescription(1:9)
        IF (M == 32) WRITE (*, 5000)       TestDescription(10:18)
        IF (M == 32) WRITE (OutUnit, 5000) TestDescription(10:18)
        N = M
        WRITE (*, 5100)       InpFileName
        WRITE (OutUnit, 5100) InpFileName
        WRITE (*, 5200)       M, N
        WRITE (OutUnit, 5200) M, N
        WRITE (*, 5300)       M
        WRITE (OutUnit, 5300) M
        DO  I = 0, 3
            Table(I) = 0
        END DO
        NTries = 40000
        DO  IJ = 1, NTries
            DO  I = 1, M
                Row(I) = SHIFTR(InpRec%GetSample(), 32 - M)
            END DO
            NTry = NTry + 1
            CALL Rank(Row, M, N, IR)
            K = MIN(N - IR, 3)
            Table(K) = Table(K) + 1
        END DO
        S = 0
        WRITE (*, 5400)
        WRITE (OutUnit, 5400)
        DO  I = 3, 0, -1
            E = P(I) * NTries
            D = (Table(I)-E) ** 2 / E
            S = S + D
            WRITE (*, 5500)       N - I, Table(I), E, D, S
            WRITE (OutUnit, 5500) N - I, Table(I), E, D, S
        END DO
        WRITE (*, 5600)       S, ChiSquare(S, 3)
        WRITE (OutUnit, 5600) S, ChiSquare(S, 3)
    END DO

    WRITE (*, 5700)
    WRITE (OutUnit, 5700)
    RETURN

5000 FORMAT (A78)
5100 FORMAT ('    Binary Rank test for ', A20)
5200 FORMAT (T8, '  Rank test for ', I2, 'x', I2, ' binary matrices:')
5300 FORMAT (T8, ' Rows from leftmost ', I2, ' bits of each 32-bit integer')
5400 FORMAT ('      Rank   observed  expected (o-e)^2/e  sum')
5500 FORMAT (2I10, F10.1, F10.6, F9.3)
5600 FORMAT ('  ChiSquare= ', F6.3, ' for 3 d. of f.; p-value= ', F8.6/   &
             '--------------------------------------------------------------')
5700 FORMAT (/ '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/)

END SUBROUTINE FT03_BinaryRank_3132_FromFile

!******************************************************************************

MODULE SUBROUTINE FT03_BinaryRank_3132_FromRNG(OutUnit, RNG)

    ! To perform the 'BinaryRank_3132' Test based on the specified generator

    ! arguments
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG
    
    ! local variables
    tInteger        :: I, IJ, IR, K, M, N, NTries, NTry
    tInteger        :: Row(32), Table(0:3)
    tSingle         :: D, E, S
    tCharAlloc      :: RngName
    
    ! execution
    
    RngName = RNG%GetName()

    ! write test description
    WRITE (*, 5000)       TestDescription(1:18)
    WRITE (OutUnit, 5000) TestDescription(1:18)

    ! see original file \f\bprint.for that displays each step in the
    !  Rank reduction.
    !  finds Rank of 31x31 and 32x32 matrices.
    ! For the 31x31, uses 31 leftmost bits of a 32-bit integer
    ! to form a Row of the binary matrix.
    ! For the 32x32, uses 32 full integer words for each of 32 Rows
    !      function mRank(r,m,n)
    !   for nxn matrices, to at least 6 places,
    !  the probability of Rank n-2,n-1,n are all vIRtually the same.
    !    r          p
    !  <=29    .0052854502
    !    30    .1283502644
    !    31    .5775761902
    !    32    .2887880952
    ! **** Finds binary Rank of M Rows, N trailing bits each ****
    
    DO  M = 31, 32
        CALL RNG%ReInit()
        IF (M == 31) WRITE (*, 5000)       TestDescription(1:9)
        IF (M == 31) WRITE (OutUnit, 5000) TestDescription(1:9)
        IF (M == 32) WRITE (*, 5000)       TestDescription(10:18)
        IF (M == 32) WRITE (OutUnit, 5000) TestDescription(10:18)
        N = M
        WRITE (*, 5100)       RngName
        WRITE (OutUnit, 5100) RngName
        WRITE (*, 5200)       M, N
        WRITE (OutUnit, 5200) M, N
        WRITE (*, 5300)       M
        WRITE (OutUnit, 5300) M
        DO  I = 0, 3
            Table(I) = 0
        END DO
        NTries = 40000
        DO  IJ = 1, NTries
            DO  I = 1, M
                Row(I) = SHIFTR(GetSample(RNG), 32 - M)
            END DO
            NTry = NTry + 1
            CALL Rank(Row, M, N, IR)
            K = MIN(N - IR, 3)
            Table(K) = Table(K) + 1
        END DO
        S = 0
        WRITE (*, 5400)
        WRITE (OutUnit, 5400)
        DO  I = 3, 0, -1
            E = P(I) * NTries
            D = (Table(I)-E) ** 2 / E
            S = S + D
            WRITE (*, 5500)       N - I, Table(I), E, D, S
            WRITE (OutUnit, 5500) N - I, Table(I), E, D, S
        END DO
        WRITE (*, 5600)       S, ChiSquare(S, 3)
        WRITE (OutUnit, 5600) S, ChiSquare(S, 3)
    END DO

    WRITE (*, 5700)
    WRITE (OutUnit, 5700)
    RETURN

5000 FORMAT (A78)
5100 FORMAT ('    Binary Rank test for ', A20)
5200 FORMAT (T8, '  Rank test for ', I2, 'x', I2, ' binary matrices:')
5300 FORMAT (T8, ' Rows from leftmost ', I2, ' bits of each 32-bit integer')
5400 FORMAT ('      Rank   observed  expected (o-e)^2/e  sum')
5500 FORMAT (2I10, F10.1, F10.6, F9.3)
5600 FORMAT ('  ChiSquare= ', F6.3, ' for 3 d. of f.; p-value= ', F8.6/   &
             '--------------------------------------------------------------')
5700 FORMAT (/ '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/)

END SUBROUTINE FT03_BinaryRank_3132_FromRNG

!******************************************************************************

SUBROUTINE Rank(R, M, N, ResVal)

    ! routine used by 'BINARY RANK' test for 31x31 and 32x32 matrices

    ! arguments
    tInteger, INTENT(INOUT) :: R(32)
    tInteger, INTENT(IN)    :: M
    tInteger, INTENT(IN)    :: N
    tInteger, INTENT(OUT)   :: ResVal
    
    ! parameters
    tInteger, PARAMETER  :: MasK(32) = [                                        &
        1, 2, 4, 8, 16, 32, 64, 128, 256, 512,                                  &
        1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072, 262144, 524288,    &
        1048576, 2097152, 4194304, 8388608, 16777216, 33554432,                 &
        67108864, 134217728, 268435456, 536870912, SHIFTL(1, 30), SHIFTL(1, 31)]

    ! local variables
    tInteger  :: I, II, J, K, X

    ! execution
    
    ResVal = 0
    J = N
    I = 1
    ! find row that starts with a 1 in current column (33-j)
    OuterLoop: DO
        DO  II = I, M
            IF (IAND(R(II),MasK(J)) == MasK(J)) THEN
                X = R(II)
                R(II) = R(I)
                R(I) = X
                DO  K = I + 1, M
                    IF (IAND(R(K), MasK(J)) == MasK(J)) R(K) = IEOR(R(K),X)
                END DO
                ResVal = ResVal + 1
                IF (I == M .OR. J == 1) EXIT OuterLoop
                J = J - 1
                I = I + 1
                CYCLE OuterLoop
            END IF
        END DO
        J = J - 1
        IF (J == 0) EXIT OuterLoop
    END DO OuterLoop
    
    RETURN
    
END SUBROUTINE Rank

!******************************************************************************
END SUBMODULE SubTest_BinaryRank3132

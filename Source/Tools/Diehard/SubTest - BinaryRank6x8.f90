
SUBMODULE (ModTest_Diehard) SubTest_BinaryRank6x8

!** PURPOSE OF THIS MODULE:
    ! contains routines to perform 'Binary Rank' test for 6x8 matrices.

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tCharLen(80), PARAMETER :: TestDescription(10) = [                              &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", &
        "     :: This is the BINARY RANK TEST for 6x8 matrices.  From each of  ::", &
        "     :: six random 32-bit integers from the generator under test, a   ::", &
        "     :: specified byte is chosen, and the resulting six bytes form a  ::", &
        "     :: 6x8 binary matrix whose rank is determined.  That rank can be ::", &
        "     :: from 0 to 6, but ranks 0,1,2,3 are rare; their counts are     ::", &
        "     :: pooled with those for rank 4. Ranks are found for 100,000     ::", &
        "     :: random matrices, and a chi-square test is performed on        ::", &
        "     :: counts for ranks 6,5 and <=4.                                 ::", &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::"]
    tSingle,      PARAMETER :: P(2:6) =                             &
        [0.149858E-06, 0.808926E-04, 0.936197E-02, 0.217439, 0.773118]
    tCharLen(6),  PARAMETER :: RK(4:6) = [' r<=4 ', ' r =5 ', ' r =6 ']

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

MODULE SUBROUTINE FT04_BinaryRank_6x8_FromFile(InpFileName, OutUnit, InpRec)

    ! To perform the 'BinaryRank_6x8' Test based on the specified input file

    ! arguments
    tCharLen(25),   INTENT(IN)      :: InpFileName
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(I32Data), INTENT(INOUT)   :: InpRec
    
    ! local variables
    tSingle         :: PP(0:25)
    tInteger        :: R(31), K(2:6)
    tInteger        :: I, IJ, IR, KK, KR, L, MR
    tSingle         :: E, Pks, S, T

    ! execution

    ! write test description
    WRITE (*, 5000)       TestDescription(1:10)
    WRITE (OutUnit, 5000) TestDescription(1:10)

    !******* Test Ranks of 100,000 6x8 binary matrices **************
    !******* Each row a byte from a RNG, overlapping rows *************

    !*** Rank 2 to 6 with prob p(2),...,p(6); 2,3,4 pooled.

    WRITE (*, 5100)       InpFileName
    WRITE (OutUnit, 5100) InpFileName
    DO  IJ = 25, 1, -1
        InpRec%RecNum = 1
        WRITE (*, 5200)       InpFileName
        WRITE (OutUnit, 5200) InpFileName
        KR = IJ - 1
        DO  KK = 2, 6
            K(KK) = 0
        END DO
        WRITE (*, 5300)       25 - KR, 32 - KR
        WRITE (OutUnit, 5300) 25 - KR, 32 - KR
        WRITE (*, 5400)
        WRITE (OutUnit, 5400)
        DO  L = 1, 100000
            DO  I = 1, 6
                R(I) = IAND(SHIFTR(InpRec%GetSample(), KR), 255)
            END DO
            CALL RankB(R, 6, 8, IR)
            MR = MAX(4, IR)
            K(MR) = K(MR) + 1
        END DO
        S = 0
        DO  L = 4, 6
            IF (L > 4) THEN
                E = 100000 * P(L)
            ELSE
                E = 100000 * (P(2)+P(3)+P(4))
            END IF
            T = (K(L)-E) ** 2 / E
            S = S + T
            WRITE (OutUnit, 5500) RK(L), K(L), E, (K(L)-E) ** 2 / E, S
            WRITE (*, 5500)       RK(L), K(L), E, (K(L)-E) ** 2 / E, S
        END DO
        PP(KR) = 1. - EXP(-S/2)
        WRITE (*, 5600)       PP(KR)
        WRITE (OutUnit, 5600) PP(KR)
    END DO

    WRITE (*, 5700)
    WRITE (OutUnit, 5700)
    WRITE (*, 5800)       (PP(I), I = 24, 0, -1)
    WRITE (OutUnit, 5800) (PP(I), I = 24, 0, -1)
    CALL ASort(PP(1:25))
    WRITE (*, 5900)       InpFileName
    WRITE (OutUnit, 5900) InpFileName
    CALL KS_Test(PP, 25, Pks)
    WRITE (*, 6000)       Pks
    WRITE (OutUnit, 6000) Pks
    WRITE (*, 6100)
    WRITE (OutUnit, 6100)
    
    RETURN

5000 FORMAT (A78)
5100 FORMAT ('         Binary Rank Test for ', A20)
5200 FORMAT ('        Rank of a 6x8 binary matrix, '/   &
    '     rows formed from eight bits of the RNG ', A15)
5300 FORMAT ('     b-Rank test for bits ', i2, ' to ', I2)
5400 FORMAT (T16, '      observed   expected     (o-e)^2/e      sum')
5500 FORMAT (T7, A9, I12, F12.1, F12.3, F12.3)
5600 FORMAT (T13, '            p=1-exp(-SUM/2)=', F7.5)
5700 FORMAT ( '   TEST SUMMARY, 25 tests on 100,000 random 6x8 matrices'/   &
    ' These should be 25 uniform [0,1] random variables:')
5800 FORMAT (5F12.6)
5900 FORMAT ('   bRank test summary for ', A20/   &
    '       The KS test for those 25 supposed UNI''S yields')
6000 FORMAT ('                    KS p-value=', F8.6)
6100 FORMAT (/ '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/)

END SUBROUTINE FT04_BinaryRank_6x8_FromFile

!******************************************************************************

MODULE SUBROUTINE FT04_BinaryRank_6x8_FromRNG(OutUnit, RNG)

    ! To perform the 'BinaryRank_6x8' Test based on the specified generator

    ! arguments
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG
    
    ! local variables
    tSingle         :: PP(0:25)
    tInteger        :: R(31), K(2:6)
    tInteger        :: I, IJ, IR, KK, KR, L, MR
    tSingle         :: E, Pks, S, T
    tCharAlloc      :: RngName
    
    ! execution
    
    RngName = RNG%GetName()

    ! write test description
    WRITE (*, 5000)       TestDescription(1:10)
    WRITE (OutUnit, 5000) TestDescription(1:10)

    !******* Test Ranks of 100,000 6x8 binary matrices **************
    !******* Each row a byte from a RNG, overlapping rows *************

    !*** Rank 2 to 6 with prob p(2),...,p(6); 2,3,4 pooled.

    WRITE (*, 5100)       RngName
    WRITE (OutUnit, 5100) RngName
    DO  IJ = 25, 1, -1
        CALL RNG%ReInit()
        WRITE (*, 5200)       RngName
        WRITE (OutUnit, 5200) RngName
        KR = IJ - 1
        DO  KK = 2, 6
            K(KK) = 0
        END DO
        WRITE (*, 5300)       25 - KR, 32 - KR
        WRITE (OutUnit, 5300) 25 - KR, 32 - KR
        WRITE (*, 5400)
        WRITE (OutUnit, 5400)
        DO  L = 1, 100000
            DO  I = 1, 6
                R(I) = IAND(SHIFTR(GetSample(RNG), KR), 255)
            END DO
            CALL RankB(R, 6, 8, IR)
            MR = MAX(4, IR)
            K(MR) = K(MR) + 1
        END DO
        S = 0
        DO  L = 4, 6
            IF (L > 4) THEN
                E = 100000 * P(L)
            ELSE
                E = 100000 * (P(2)+P(3)+P(4))
            END IF
            T = (K(L)-E) ** 2 / E
            S = S + T
            WRITE (OutUnit, 5500) RK(L), K(L), E, (K(L)-E) ** 2 / E, S
            WRITE (*, 5500)       RK(L), K(L), E, (K(L)-E) ** 2 / E, S
        END DO
        PP(KR) = 1. - EXP(-S/2)
        WRITE (*, 5600)       PP(KR)
        WRITE (OutUnit, 5600) PP(KR)
    END DO

    WRITE (*, 5700)
    WRITE (OutUnit, 5700)
    WRITE (*, 5800)       (PP(I), I = 24, 0, -1)
    WRITE (OutUnit, 5800) (PP(I), I = 24, 0, -1)
    CALL ASort(PP(1:25))
    WRITE (*, 5900)       RngName
    WRITE (OutUnit, 5900) RngName
    CALL KS_Test(PP, 25, Pks)
    WRITE (*, 6000)       Pks
    WRITE (OutUnit, 6000) Pks
    WRITE (*, 6100)
    WRITE (OutUnit, 6100)
    
    RETURN

5000 FORMAT (A78)
5100 FORMAT ('         Binary Rank Test for ', A20)
5200 FORMAT ('        Rank of a 6x8 binary matrix, '/   &
    '     rows formed from eight bits of the RNG ', A15)
5300 FORMAT ('     b-Rank test for bits ', i2, ' to ', I2)
5400 FORMAT (T16, '      observed   expected     (o-e)^2/e      sum')
5500 FORMAT (T7, A9, I12, F12.1, F12.3, F12.3)
5600 FORMAT (T13, '            p=1-exp(-SUM/2)=', F7.5)
5700 FORMAT ( '   TEST SUMMARY, 25 tests on 100,000 random 6x8 matrices'/   &
    ' These should be 25 uniform [0,1] random variables:')
5800 FORMAT (5F12.6)
5900 FORMAT ('   bRank test summary for ', A20/   &
    '       The KS test for those 25 supposed UNI''S yields')
6000 FORMAT ('                    KS p-value=', F8.6)
6100 FORMAT (/ '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/)

END SUBROUTINE FT04_BinaryRank_6x8_FromRNG

!******************************************************************************

SUBROUTINE RankB(R, M, N, ResVal)

    ! routine used by 'BINARY RANK' test for 6x8 matrices

    ! arguments
    tInteger, INTENT(INOUT) :: R(31)
    tInteger, INTENT(IN)    :: M
    tInteger, INTENT(IN)    :: N
    tInteger, INTENT(OUT)   :: ResVal
    
    ! parameters
    tInteger, PARAMETER  :: MasK(31) = [                                &
        1, 2, 4, 8, 16, 32, 64, 128, 256, 512,                          &
        1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072, 262144,    &
        524288, 1048576, 2097152, 4194304, 8388608, 16777216,           &
        33554432, 67108864, 134217728, 268435456, 536870912, 1073741824]
        
    ! local variables
    tInteger  :: I, II, J, K, X

    ! execution
    
    ResVal = 0
    J = N
    I = 1
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
    
END SUBROUTINE RankB

!******************************************************************************

END SUBMODULE SubTest_BinaryRank6x8

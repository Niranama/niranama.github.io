
SUBMODULE (ModTest_Diehard) SubTest_Birthday

!** PURPOSE OF THIS MODULE:
    ! contains routines to perform 'Birthday Spacing' test.

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tCharLen(80), PARAMETER :: TestDescription(18) = [                               &
         "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", &
         "     ::            This is the BIRTHDAY SPACINGS TEST                 ::", &
         "     :: Choose m birthdays in a year of n days.  List the spacings    ::", &
         "     :: between the birthdays.  If j is the number of values that     ::", &
         "     :: occur more than once in that list, then j is asymptotically   ::", &
         "     :: Poisson distributed with mean m^3/(4n).  Experience shows n   ::", &
         "     :: must be quite large, say n>=2^18, for comparing the results   ::", &
         "     :: to the Poisson distribution with that mean.  This test uses   ::", &
         "     :: n=2^24 and m=2^9,  so that the underlying distribution for j  ::", &
         "     :: is taken to be Poisson with lambda=2^27/(2^26)=2.  A sample   ::", &
         "     :: of 500 j's is taken, and a chi-square goodness of fit test    ::", &
         "     :: provides a p value.  The first test uses bits 1-24 (counting  ::", &
         "     :: from the left) from integers in the specified file.           ::", &
         "     ::   Then the file is closed and reopened. Next, bits 2-25 are   ::", &
         "     :: used to provide birthdays, then 3-26 and so on to bits 9-32.  ::", &
         "     :: Each set of bits provides a p-value, and the nine p-values    ::", &
         "     :: provide a sample for a KSTEST.                                ::", &
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

MODULE SUBROUTINE FT01_BirthdaySpacing_FromFile(InpFileName, OutUnit, InpRec)

    ! To perform the 'Birthday Spacing' Test based on the specified input file

    ! arguments
    tCharLen(25),   INTENT(IN)      :: InpFileName
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(I32Data), INTENT(INOUT)   :: InpRec
    
    ! local variables
    tInteger        :: B(4096), C(4096), MSpace(1000)
    tSingle         :: Alam, Pks(64), PP, S
    tInteger        :: I, J, JB, KR, L, LK, M, Mask, NBits, NSample
    
    ! execution

    ! write test description
    WRITE (*, 5000)      TestDescription(1:18)
    WRITE (OutUnit,5000) TestDescription(1:18)

    NBits = 24
    M = 512
    NSample = 500
    Alam = REAL(M) ** 3 / 2.0_kDouble ** (NBits+2)
    WRITE (*, 5200)      M, NBits, Alam
    WRITE (OutUnit,5200) M, NBits, Alam
    WRITE (OutUnit,5100) InpFileName
    WRITE (*, 5100)      InpFileName
    !      is=lw-NBits
    Mask = 2 ** (NBits-1) + 2 ** (NBits-1) - 1
    DO  KR = 32 - NBits, 0, -1
        S = 0.0
        InpRec%RecNum = 1
        DO  J = 1, NSample
            DO  I = 1, M
                JB = InpRec%GetSample()
                B(I) = IAND(SHIFTR(JB, KR), Mask)
            END DO
            CALL ISort(B(1:M))
            C(1) = B(1)
            DO  I = 2, M
                C(I) = B(I) - B(I-1)
            END DO
            CALL ISort(C(1:M))
            L = 0
            DO  I = 2, M
                LK = 0
                IF (C(I) == C(I-1)) THEN
                    LK = LK + 1
                    L = L + 1
                END IF
            END DO
            S = S + L
            MSpace(J) = L
        END DO
        WRITE (OutUnit,5400) NSample
        WRITE (*, 5400)      NSample
        WRITE (OutUnit,5300) InpFileName, 33 - NBits - KR, 32 - KR, S / NSample
        WRITE (*, 5300)      InpFileName, 33 - NBits - KR, 32 - KR, S / NSample
        CALL ChiSquareTest(Alam, MSpace, NSample, PP, OutUnit)
        Pks(9-KR) = PP
    END DO

    WRITE (*, 5500)      Pks(1:9)
    WRITE (OutUnit,5500) Pks(1:9)
    CALL KS_Test(Pks, 9, PP)
    WRITE (*, 5600)      PP
    WRITE (OutUnit,5600) PP
    WRITE (*, 5700)
    WRITE (OutUnit,5700)
    RETURN

5000 FORMAT (A78)
5100 FORMAT ('           Results for ', A20)
5200 FORMAT (' BIRTHDAY SPACINGS TEST, M=', I4, ' N=2**', I2, ' LAMBDA=', F8.4)
5300 FORMAT (T11, A16, ' using bits ', I2, ' to ', I2, F8.3, F10.6)
5400 FORMAT (T18, '  For a sample of size', I4, ':     mean   ')
5500 FORMAT ('   The 9 p-values were'/ F15.6, 4F10.6/ F15.6, 4F10.6)
5600 FORMAT ('  A KSTEST for the 9 p-values yields ',F8.6)
5700 FORMAT (/ '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/)

END SUBROUTINE FT01_BirthdaySpacing_FromFile

!******************************************************************************

MODULE SUBROUTINE FT01_BirthdaySpacing_FromRNG(OutUnit, RNG)

    ! To perform the 'Birthday Spacing' Test based on the specified generator

    ! arguments
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG
    
    ! local variables
    tInteger        :: B(4096), C(4096), MSpace(1000)
    tSingle         :: Alam, Pks(64), PP, S
    tInteger        :: I, J, JB, KR, L, LK, M, Mask, NBits, NSample
    tCharAlloc      :: RngName
    
    ! execution
    
    RngName = RNG%GetName()

    ! write test description
    WRITE (*, 5000)      TestDescription(1:18)
    WRITE (OutUnit,5000) TestDescription(1:18)

    NBits = 24
    M = 512
    NSample = 500
    Alam = REAL(M) ** 3 / 2.0_kDouble ** (NBits+2)
    WRITE (*, 5200)      M, NBits, Alam
    WRITE (OutUnit,5200) M, NBits, Alam
    WRITE (OutUnit,5100) RngName
    WRITE (*, 5100)      RngName
    !      is=lw-NBits
    Mask = 2 ** (NBits-1) + 2 ** (NBits-1) - 1
    DO  KR = 32 - NBits, 0, -1
        S = 0.0
        CALL RNG%ReInit()
        DO  J = 1, NSample
            DO  I = 1, M
                JB = GetSample(RNG)
                B(I) = IAND(SHIFTR(JB, KR), Mask)
            END DO
            CALL ISort(B(1:M))
            C(1) = B(1)
            DO  I = 2, M
                C(I) = B(I) - B(I-1)
            END DO
            CALL ISort(C(1:M))
            L = 0
            DO  I = 2, M
                LK = 0
                IF (C(I) == C(I-1)) THEN
                    LK = LK + 1
                    L = L + 1
                END IF
            END DO
            S = S + L
            MSpace(J) = L
        END DO
        WRITE (OutUnit,5400) NSample
        WRITE (*, 5400)      NSample
        WRITE (OutUnit,5300) RngName, 33 - NBits - KR, 32 - KR, S / NSample
        WRITE (*, 5300)      RngName, 33 - NBits - KR, 32 - KR, S / NSample
        CALL ChiSquareTest(Alam, MSpace, NSample, PP, OutUnit)
        Pks(9-KR) = PP
    END DO

    WRITE (*, 5500)      Pks(1:9)
    WRITE (OutUnit,5500) Pks(1:9)
    CALL KS_Test(Pks, 9, PP)
    WRITE (*, 5600)      PP
    WRITE (OutUnit,5600) PP
    WRITE (*, 5700)
    WRITE (OutUnit,5700)
    RETURN

5000 FORMAT (A78)
5100 FORMAT ('           Results for ', A20)
5200 FORMAT (' BIRTHDAY SPACINGS TEST, M=', I4, ' N=2**', I2, ' LAMBDA=', F8.4)
5300 FORMAT (T11, A16, ' using bits ', I2, ' to ', I2, F8.3, F10.6)
5400 FORMAT (T18, '  For a sample of size', I4, ':     mean   ')
5500 FORMAT ('   The 9 p-values were'/ F15.6, 4F10.6/ F15.6, 4F10.6)
5600 FORMAT ('  A KSTEST for the 9 p-values yields ',F8.6)
5700 FORMAT (/ '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/)

END SUBROUTINE FT01_BirthdaySpacing_FromRNG

!******************************************************************************

SUBROUTINE ChiSquareTest(Lambda, MSpace, NSample, PP, OutUnit)

    ! Routine used by 'BIRTHDAY SPACINGS' test
    ! To perform a chi square test on N samples from a discrete distribution.
    ! Setup for poisson.  Change PP's for others.  Requires array MSpace(NSample)
    ! that gives number of duplicate spacings in each of nsample years.

    ! arguments
    tSingle,  INTENT(IN)        :: Lambda
    tInteger, INTENT(INOUT)     :: MSpace(1000)
    tInteger, INTENT(IN)        :: NSample
    tSingle,  INTENT(OUT)       :: PP
    tInteger, INTENT(IN)        :: OutUnit      ! output file unit number

    ! local variables
    tInteger    :: K(0:500)
    tSingle     :: Ex(0:500), Obs(0:500), Ps(0:500)
    tInteger    :: I, J, L, Lb, LT, M, NJ
    tSingle     :: P, S

    ! execution

    NJ = INT(Lambda + 4 * SQRT(Lambda))
    Ex(0:NJ) = 0.0
    K(0:NJ) = 0
    Ps(0:NJ) = 0.0
    P = EXP(-Lambda)
    Ps(0) = P * NSample
    K(0) = 0
    J = 0
    S = P * NSample
    IF (S > 5.0) THEN
        J = 1
        Ex(0) = S
        S = 0.0
    END IF

    DO  I = 1, NJ
        P = Lambda * P / I
        Ps(I) = Ps(I-1) + P * NSample
        S = S + P * NSample
        K(I) = J
        IF (Ps(I) > NSample-5) THEN
            Ex(J) = S + NSample - Ps(I)
            DO  L = I + 1, NSample
                K(L) = J
            END DO
            EXIT
        END IF
        IF (S >= 5.0) THEN
            Ex(J) = S
            J = J + 1
            S = 0.0
        END IF
    END DO

    Obs(0:100) = 0.0
    DO  I = 1, NSample
        L = K(MSpace(I))
        Obs(L) = Obs(L) + 1
    END DO

    S = 0.0
    DO  M = 0, J
        S = S + (Obs(M)-Ex(M)) ** 2 / Ex(M)
    END DO
    Lb = 0
    M = K(0)

    WRITE (OutUnit, *) ' duplicate ', '      number       number '
    WRITE (OutUnit, *) ' spacings  ', '     observed     expected'
    WRITE (*, *)       ' duplicate ', '      number       number '
    WRITE (*, *)       ' spacings  ', '     observed     expected'
    DO  I = 1, 100
        IF (K(I) /= M) THEN
            LT = I - 1
            IF (Lb /= LT) WRITE (*, 5000)       Lb, LT, Obs(M), Ex(M)
            IF (Lb /= LT) WRITE (OutUnit, 5000) Lb, LT, Obs(M), Ex(M)
            IF (Lb == LT) WRITE (*, 5100)       Lb, Obs(M), Ex(M)
            IF (Lb == LT) WRITE (OutUnit, 5100) Lb, Obs(M), Ex(M)
            M = K(I)
            Lb = I
            IF (M == J) EXIT
        END IF
    END DO

    WRITE (*, 5200)       Lb, Obs(M), Ex(M)
    WRITE (OutUnit, 5200) Lb, Obs(M), Ex(M)
    PP = ChiSquare(S,J)
    WRITE (*, 5300)       J, S, PP
    WRITE (OutUnit, 5300) J, S, PP
    WRITE (OutUnit, *) ' :::::::::::::::::::::::::::::::::::::::::'
    WRITE (*, *)       ' :::::::::::::::::::::::::::::::::::::::::'

    RETURN

5000    FORMAT (' ', I2, ' to ', I2, F13.0, F13.3)
5100    FORMAT (T4, I6, F13.0, F13.3)
5200    FORMAT (' ', I2, ' to INF', F12.0, F13.3)
5300    FORMAT (' Chisquare with ', I2, ' d.o.f. = ', F8.2, ' p-value = ', F8.6)
         
END SUBROUTINE ChiSquareTest

!******************************************************************************
END SUBMODULE SubTest_Birthday

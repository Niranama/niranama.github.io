
SUBMODULE (ModTest_Diehard) SubTest_OPSO_OQSO_DNA

!** PURPOSE OF THIS MODULE:
    ! contains routines to perform 'BitStream' test.

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tCharLen(80), PARAMETER :: TestDescription(36) = [                              &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", &
        "     ::             The tests OPSO, OQSO and DNA                      ::", &
        "     ::         OPSO means Overlapping-Pairs-Sparse-Occupancy         ::", &
        "     :: The OPSO test considers 2-letter words from an alphabet of    ::", &
        "     :: 1024 letters.  Each letter is determined by a specified ten   ::", &
        "     :: bits from a 32-bit integer in the sequence to be tested. OPSO ::", &
        "     :: generates  2^21 (overlapping) 2-letter words  (from 2^21+1    ::", &
        "     :: 'keystrokes')  and counts the number of missing words---that  ::", &
        "     :: is 2-letter words which do not appear in the entire sequence. ::", &
        "     :: That count should be very close to normally distributed with  ::", &
        "     :: mean 141,909, sigma 290. Thus (missingwrds-141909)/290 should ::", &
        "     :: be a standard normal variable. The OPSO test takes 32 bits at ::", &
        "     :: a time from the test file and uses a designated set of ten    ::", &
        "     :: consecutive bits. It then restarts the file for the next de-  ::", &
        "     :: signated 10 bits, and so on.                                  ::", &
        "     ::                                                               ::", &
        "     ::     OQSO means Overlapping-Quadruples-Sparse-Occupancy        ::", &
        "     ::   The test OQSO is similar, except that it considers 4-letter ::", &
        "     :: words from an alphabet of 32 letters, each letter determined  ::", &
        "     :: by a designated string of 5 consecutive bits from the test    ::", &
        "     :: file, elements of which are assumed 32-bit random integers.   ::", &
        "     :: The mean number of missing words in a sequence of 2^21 four-  ::", &
        "     :: letter words,  (2^21+3 'keystrokes'), is again 141909, with   ::", &
        "     :: sigma = 295.  The mean is based on theory; sigma comes from   ::", &
        "     :: extensive simulation.                                         ::", &
        "     ::                                                               ::", &
        "     ::    The DNA test considers an alphabet of 4 letters::  C,G,A,T,::", &
        "     :: determined by two designated bits in the sequence of random   ::", &
        "     :: integers being tested.  It considers 10-letter words, so that ::", &
        "     :: as in OPSO and OQSO, there are 2^20 possible words, and the   ::", &
        "     :: mean number of missing words from a string of 2^21  (over-    ::", &
        "     :: lapping)  10-letter  words (2^21+9 'keystrokes') is 141909.   ::", &
        "     :: The standard deviation sigma=339 was determined as for OQSO   ::", &
        "     :: by simulation.  (Sigma for OPSO, 290, is the true value (to   ::", &
        "     :: three places), not determined by simulation.                  ::", &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::"]
    tSingle,      PARAMETER :: Sigmas(3)  = [ 290.0, 295.0, 339.0 ]
    tCharLen(4),  PARAMETER :: CurTest(3) = [ 'OPSO', 'OQSO', ' DNA' ]

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS
    ! na

!** GENERIC DECLARATIONS:
    INTERFACE IKBit
        MODULE PROCEDURE IKBit_FromRNG, IKBit_FromFile
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE SUBROUTINE FT06_OPSO_OQSO_DNA_FromFile(InpFileName, OutUnit, InpRec)

    ! To perform the 'OPSO, OQSO and DNA' Test based on the specified input file

    ! arguments
    tCharLen(25),   INTENT(IN)      :: InpFileName
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(I32Data), INTENT(INOUT)   :: InpRec

    ! local variables
    tInteger        :: W(0:32767), MBit(0:31)
    tInteger        :: I, IC, Indx, J, JK
    tInteger        :: K, KIJ, KK, Count, KPow, KRK, KR, L, LK, MK, MKK
    tInteger        :: NT, NTries
    tSingle         :: True, X

    ! execution

    ! write test description
    WRITE (*, 5000)       TestDescription(1:36)
    WRITE (OutUnit, 5000) TestDescription(1:36)

    !***** NUMBER OF MISSING WORDS IN A STRING OF 2**21 k-LETTER WORDS,****
    !***** EACH LETTER 20/k BITS. THERE ARE 2**20 POSSIBLE WORDS**************
    !***** EACH OF THE 32 BITS IN THE 2**15 W-TABLE IDENTIFIES A WORD*******
    !******** mean should be 141,909 with sigma=290

    DO  JK = 1, 3
        K = 2 * JK
        IF (JK == 3) K = 10
        Indx = (73-(K-9)**2) / 24
        WRITE (*, *)       CurTest(Indx), ' test for ', InpFileName
        WRITE (*, *)       ' Output: No. missing words (mw), equiv normal variate (z), p-value (p)'
        WRITE (*, *)       '   mw     z     p'
        WRITE (OutUnit, *) CurTest(Indx), ' test for ', InpFileName
        WRITE (OutUnit, *) ' Output: No. missing words (mw), equiv normal variate (z), p-value (p)'
        WRITE (OutUnit, *) '   mw     z     p'
        NTries = 1
        KPow = 21
        DO  KRK = 33 - 20 / K, 1, -1
            InpRec%RecNum = 1
            KR = 33 - 20 / K - KRK
            MK = 2 ** (20/K) - 1
            MKK = 2 ** (20-20/K) - 1
            LK = 20 / K
            DO  KIJ = 1, NTries
                KPow = 21
                ! **** SET MASK BITS ***************
                DO  I = 0, 31
                    MBit(I) = SHIFTL(1, I)
                END DO
                ! *********** INITIALIZE *****************
                True = 2 ** 20 * EXP(-2.**(KPow-20))
                ! ***** MAIN LOOP *********
                DO  NT = 1, NTries
                    ! ******** SET W-TABLE TO ZEROS *******
                    W(0:32767) = 0

                    !**** GET INITIAL WORD
                    J = IKBit(InpRec, KR, MK)
                    DO  I = 1, K - 1
                        J = 2 ** (20/K) * J + IKBit(InpRec, KR, MK)
                    END DO

                    !****  GENERATE 2**KPow OVERLAPPING WORDS ********
                    DO  IC = 1, 2 ** KPow
                        ! *** GET NEW J *****
                        J = SHIFTL(IAND(J, MKK), LK) + IKBit(InpRec, KR, MK)
                        ! *** GET BIT Indx FROM LAST 5 BITS OF J  ***
                        L = IAND(J,31)
                        ! *** GET TABLE Indx FROM LEADING 15 BITS OF J ***
                        KK = SHIFTR(J, 5)
                        ! *** SET BIT L IN W(KK) ***
                        W(KK) = IOR(W(KK), MBit(L))
                    END DO
                    ! ********** COUNT NUMBER OF EMPTY CELLS *******
                    Count = 0
                    DO  KK = 0, 32767
                        DO  L = 0, 31
                            IF (IAND(W(KK),MBit(L)) == 0) Count = Count + 1
                        END DO
                    END DO
                    ! **** END OF MAIN LOOP ****

                    X = (Count-True) / Sigmas(JK)
                    WRITE (OutUnit, 5100) CurTest(Indx), InpFileName, 33 - 20 / K - KR, &
                                          32 - KR, Count, X, Phi(X)
                    WRITE (*, 5100)       CurTest(Indx), InpFileName, 33 - 20 / K - KR, &
                                          32 - KR, Count, X, Phi(X)
                END DO
                InpRec%RecNum = 1
            END DO
        END DO
    END DO
    WRITE (*, 5200)
    WRITE (OutUnit, 5200)

    RETURN

5000 FORMAT (A78)
5100 FORMAT (A8,' for ', A15,' using bits ', I2,' to ', I2, I14, F7.3, F7.4)
5200 FORMAT (/ '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/ )

END SUBROUTINE FT06_OPSO_OQSO_DNA_FromFile

!******************************************************************************

MODULE SUBROUTINE FT06_OPSO_OQSO_DNA_FromRNG(OutUnit, RNG)

    ! To perform the 'OPSO, OQSO and DNA' Test based on the specified generator

    ! arguments
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG

    ! local variables
    tInteger        :: W(0:32767), MBit(0:31)
    tInteger        :: I, IC, Indx, J, JK
    tInteger        :: K, KIJ, KK, Count, KPow, KRK, KR, L, LK, MK, MKK
    tInteger        :: NT, NTries
    tSingle         :: True, X
    tCharAlloc      :: RngName

    ! execution

    RngName = RNG%GetName()

    ! write test description
    WRITE (*, 5000)       TestDescription(1:36)
    WRITE (OutUnit, 5000) TestDescription(1:36)

    !***** NUMBER OF MISSING WORDS IN A STRING OF 2**21 k-LETTER WORDS,****
    !***** EACH LETTER 20/k BITS. THERE ARE 2**20 POSSIBLE WORDS**************
    !***** EACH OF THE 32 BITS IN THE 2**15 W-TABLE IDENTIFIES A WORD*******
    !******** mean should be 141,909 with sigma=290

    DO  JK = 1, 3
        K = 2 * JK
        IF (JK == 3) K = 10
        Indx = (73-(K-9)**2) / 24
        WRITE (*, *)       CurTest(Indx), ' test for ', RngName
        WRITE (*, *)       ' Output: No. missing words (mw), equiv normal variate (z), p-value (p)'
        WRITE (*, *)       '   mw     z     p'
        WRITE (OutUnit, *) CurTest(Indx), ' test for ', RngName
        WRITE (OutUnit, *) ' Output: No. missing words (mw), equiv normal variate (z), p-value (p)'
        WRITE (OutUnit, *) '   mw     z     p'
        NTries = 1
        KPow = 21
        DO  KRK = 33 - 20 / K, 1, -1
            CALL RNG%ReInit()
            KR = 33 - 20 / K - KRK
            MK = 2 ** (20/K) - 1
            MKK = 2 ** (20-20/K) - 1
            LK = 20 / K
            DO  KIJ = 1, NTries
                KPow = 21
                ! **** SET MASK BITS ***************
                DO  I = 0, 31
                    MBit(I) = SHIFTL(1, I)
                END DO
                ! *********** INITIALIZE *****************
                True = 2 ** 20 * EXP(-2.**(KPow-20))
                ! ***** MAIN LOOP *********
                DO  NT = 1, NTries
                    ! ******** SET W-TABLE TO ZEROS *******
                    W(0:32767) = 0

                    !**** GET INITIAL WORD
                    J = IKBit(RNG, KR, MK)
                    DO  I = 1, K - 1
                        J = 2 ** (20/K) * J + IKBit(RNG, KR, MK)
                    END DO

                    !****  GENERATE 2**KPow OVERLAPPING WORDS ********
                    DO  IC = 1, 2 ** KPow
                        ! *** GET NEW J *****
                        J = SHIFTL(IAND(J, MKK), LK) + IKBit(RNG, KR, MK)
                        ! *** GET BIT Indx FROM LAST 5 BITS OF J  ***
                        L = IAND(J,31)
                        ! *** GET TABLE Indx FROM LEADING 15 BITS OF J ***
                        KK = SHIFTR(J, 5)
                        ! *** SET BIT L IN W(KK) ***
                        W(KK) = IOR(W(KK), MBit(L))
                    END DO
                    ! ********** COUNT NUMBER OF EMPTY CELLS *******
                    Count = 0
                    DO  KK = 0, 32767
                        DO  L = 0, 31
                            IF (IAND(W(KK),MBit(L)) == 0) Count = Count + 1
                        END DO
                    END DO
                    ! **** END OF MAIN LOOP ****

                    X = (Count-True) / Sigmas(JK)
                    WRITE (OutUnit, 5100) CurTest(Indx), RngName, 33 - 20 / K - KR, &
                                          32 - KR, Count, X, Phi(X)
                    WRITE (*, 5100)       CurTest(Indx), RngName, 33 - 20 / K - KR, &
                                          32 - KR, Count, X, Phi(X)
                END DO
                CALL RNG%ReInit()
            END DO
        END DO
    END DO
    WRITE (*, 5200)
    WRITE (OutUnit, 5200)

    RETURN

5000 FORMAT (A78)
5100 FORMAT (A8,' for ', A15,' using bits ', I2,' to ', I2, I14, F7.3, F7.4)
5200 FORMAT (/ '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/ )

END SUBROUTINE FT06_OPSO_OQSO_DNA_FromRNG

!******************************************************************************

FUNCTION IKBit_FromFile(InpRec, KR, MK) RESULT(ResVal)

    ! function used by 'OPSO, OQSO and DNA' tests

    ! arguments
    TYPE(I32Data), INTENT(INOUT)    :: InpRec
    tInteger,      INTENT(IN)       :: KR, MK
    tInteger                        :: ResVal

    ! execution

    !** ONE-LINE FUNCTION TO GENERATE 5-BIT LETTER IN CONVENIENT POSITION
    ResVal = IAND(SHIFTR(InpRec%GetSample(), KR), MK)

    RETURN

END FUNCTION IKBit_FromFile

!******************************************************************************

FUNCTION IKBit_FromRNG(RNG, KR, MK) RESULT(ResVal)

    ! function used by 'OPSO, OQSO and DNA' tests

    ! arguments
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG
    tInteger,           INTENT(IN)      :: KR, MK
    tInteger                            :: ResVal

    ! execution

    !** ONE-LINE FUNCTION TO GENERATE 5-BIT LETTER IN CONVENIENT POSITION
    ResVal = IAND(SHIFTR(GetSample(RNG), KR), MK)

    RETURN

END FUNCTION IKBit_FromRNG

!******************************************************************************

END SUBMODULE SubTest_OPSO_OQSO_DNA

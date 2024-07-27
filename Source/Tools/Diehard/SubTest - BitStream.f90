
SUBMODULE (ModTest_Diehard) SubTest_BitStream

!** PURPOSE OF THIS MODULE:
    ! contains routines to perform 'BitStream' test.

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tCharLen(80), PARAMETER :: TestDescription(16) = [                              &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", &
        "     ::                   THE BITSTREAM TEST                          ::", &
        "     :: The file under test is viewed as a stream of bits. Call them  ::", &
        "     :: b1,b2,... .  Consider an alphabet with two 'letters', 0 and 1 ::", &
        "     :: and think of the stream of bits as a succession of 20-letter  ::", &
        "     :: 'words', overlapping.  Thus the first word is b1b2...b20, the ::", &
        "     :: second is b2b3...b21, and so on.  The bitstream test counts   ::", &
        "     :: the number of missing 20-letter (20-bit) words in a string of ::", &
        "     :: 2^21 overlapping 20-letter words.  There are 2^20 possible 20 ::", &
        "     :: letter words.  For a truly random string of 2^21+19 bits, the ::", &
        "     :: number of missing words j should be (very close to) normally  ::", &
        "     :: distributed with mean 141,909 and sigma 428.  Thus            ::", &
        "     ::  (j-141909)/428 should be a standard normal variate (z score) ::", &
        "     :: that leads to a uniform [0,1) p value.  The test is repeated  ::", &
        "     :: twenty times.                                                 ::", &
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

MODULE SUBROUTINE FT05_BitStream_FromFile(InpFileName, OutUnit, InpRec)

    ! To perform the 'BitStream' Test based on the specified input file

    ! arguments
    tCharLen(25),   INTENT(IN)      :: InpFileName
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(I32Data), INTENT(INOUT)   :: InpRec
    
    ! local variables
    tInteger        :: W(0:32767), MBit(0:31)
    tSingle         :: Mu, Sigma, X
    tInteger        :: I, IB, IC, J, K, Count, KPow, L, NT, NTries, Num
    
    ! execution

    ! write test description
    WRITE (*, 5000)       TestDescription(1:16)
    WRITE (OutUnit, 5000) TestDescription(1:16)

    !     THE OVERLAPPING 20-tuples TEST  BITSTREAM, 20 BITS PER WORD, N words
    !     If n=2^22, should be 19205.3 missing 20-letter words, sigma 167.
    !     If n=2^21, should be 141909  missing 20-letter words, sigma 428.
    !     If n=2^20, should be 385750  missing 20-letter words, sigma 512

    InpRec%RecNum = 1

    KPow = 21
    NTries = 20
    Sigma = 428
    
    !**** SET MASK BITS***************
    DO  I = 0, 31
        MBit(I) = SHIFTL(1, I)
    END DO
    !***********INITIALIZE*****************
    WRITE (OutUnit, *) '      THE OVERLAPPING 20-tuples BITSTREAM TEST,'
    WRITE (OutUnit, *) '           20 BITS PER WORD, 2^21 words.'
    WRITE (*, *)       '      THE OVERLAPPING 20-tuples BITSTREAM TEST,'
    WRITE (*, *)       '           20 BITS PER WORD, 2^21 words.'
    WRITE (OutUnit, *) '   This test samples the bitstream 20 times.'
    WRITE (*, *)       '   This test samples the bitstream 20 times.'
    Mu = 2 ** 20 * EXP(-2.0**(KPow-20))
    WRITE (OutUnit, 5100) Mu, Sigma
    WRITE (*, 5100)       Mu, Sigma

    !*****MAIN LOOP*********
    !**** GET INITIAL WORD
    J = InpRec%GetSample()
    J = IAND(J, 2**20-1)
    WRITE (OutUnit, 5200) InpFileName
    WRITE (*, 5200)       InpFileName

    DO  NT = 1, NTries
        !     ********SET W-TABLE TO ZEROS*******
        W(0:32767) = 0
        !**** GENERATE 2**kpow OVERLAPPING WORDS**********
        DO  IC = 1, 2 ** (KPow-5)
            Num = InpRec%GetSample()
            DO  IB = 1, 32
                !     *** GET NEW J *****
                J = SHIFTL(IAND(J, 2**19-1), 1) + IAND(Num, 1)
                Num = SHIFTR(Num, 1)
                !     *** GET BIT INDEX FROM LAST 5 BITS OF J  ***
                L = IAND(J, 31)
                !     *** GET TABLE INDEX FROM LEADING 15 BITS OF J***
                K = SHIFTR(J, 5)
                !     *** SET BIT L IN W(K) ***
                W(K) = IOR(W(K), MBit(L))
            END DO
        END DO
        !     ********** COUNT NUMBER OF EMPTY CELLS *******
        Count = 0
        DO  K = 0, 32767
            DO  L = 0, 31
                IF (IAND(W(K), MBit(L)) == 0) Count = Count + 1
            END DO
        END DO
        !     ****END OF MAIN LOOP****
        X = (Count - Mu) / Sigma
        WRITE (OutUnit, 5300) NT, Count, X, Phi(X)
        WRITE (*, 5300)       NT, Count, X, Phi(X)
    END DO

    WRITE (*, 5400)
    WRITE (OutUnit, 5400)

    RETURN

5000 FORMAT (A78)
5100 FORMAT ('  No. missing words should average', F9.0, ' with sigma= ',  &
             F4.0/ '--------------------------------------------------')
5200 FORMAT (' BITSTREAM test results for', A15)
5300 FORMAT (' tst no ', I2, ': ', I7, ' missing words, ', F7.2,  &
             ' sigmas from mean, p-value=', F7.5)
5400 FORMAT (/'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/ )

END SUBROUTINE FT05_BitStream_FromFile

!******************************************************************************

MODULE SUBROUTINE FT05_BitStream_FromRNG(OutUnit, RNG)

    ! To perform the 'BitStream' Test based on the specified generator

    ! arguments
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG
    
    ! local variables
    tInteger        :: W(0:32767), MBit(0:31)
    tSingle         :: Mu, Sigma, X
    tInteger        :: I, IB, IC, J, K, Count, KPow, L, NT, NTries, Num
    tCharAlloc      :: RngName
    
    ! execution
    
    RngName = RNG%GetName()

    ! write test description
    WRITE (*, 5000)       TestDescription(1:16)
    WRITE (OutUnit, 5000) TestDescription(1:16)

    !     THE OVERLAPPING 20-tuples TEST  BITSTREAM, 20 BITS PER WORD, N words
    !     If n=2^22, should be 19205.3 missing 20-letter words, sigma 167.
    !     If n=2^21, should be 141909  missing 20-letter words, sigma 428.
    !     If n=2^20, should be 385750  missing 20-letter words, sigma 512

    CALL RNG%ReInit()

    KPow = 21
    NTries = 20
    Sigma = 428
    
    !**** SET MASK BITS***************
    DO  I = 0, 31
        MBit(I) = SHIFTL(1, I)
    END DO
    !***********INITIALIZE*****************
    WRITE (OutUnit, *) '      THE OVERLAPPING 20-tuples BITSTREAM TEST,'
    WRITE (OutUnit, *) '           20 BITS PER WORD, 2^21 words.'
    WRITE (*, *)       '      THE OVERLAPPING 20-tuples BITSTREAM TEST,'
    WRITE (*, *)       '           20 BITS PER WORD, 2^21 words.'
    WRITE (OutUnit, *) '   This test samples the bitstream 20 times.'
    WRITE (*, *)       '   This test samples the bitstream 20 times.'
    Mu = 2 ** 20 * EXP(-2.0**(KPow-20))
    WRITE (OutUnit, 5100) Mu, Sigma
    WRITE (*, 5100)       Mu, Sigma

    !*****MAIN LOOP*********
    !**** GET INITIAL WORD
    J = GetSample(RNG)
    J = IAND(J, 2**20-1)
    WRITE (OutUnit, 5200) RngName
    WRITE (*, 5200)       RngName

    DO  NT = 1, NTries
        !     ********SET W-TABLE TO ZEROS*******
        W(0:32767) = 0
        !**** GENERATE 2**kpow OVERLAPPING WORDS**********
        DO  IC = 1, 2 ** (KPow-5)
            Num = GetSample(RNG)
            DO  IB = 1, 32
                !     *** GET NEW J *****
                J = SHIFTL(IAND(J, 2**19-1), 1) + IAND(Num, 1)
                Num = SHIFTR(Num, 1)
                !     *** GET BIT INDEX FROM LAST 5 BITS OF J  ***
                L = IAND(J, 31)
                !     *** GET TABLE INDEX FROM LEADING 15 BITS OF J***
                K = SHIFTR(J, 5)
                !     *** SET BIT L IN W(K) ***
                W(K) = IOR(W(K), MBit(L))
            END DO
        END DO
        !     ********** COUNT NUMBER OF EMPTY CELLS *******
        Count = 0
        DO  K = 0, 32767
            DO  L = 0, 31
                IF (IAND(W(K), MBit(L)) == 0) Count = Count + 1
            END DO
        END DO
        !     ****END OF MAIN LOOP****
        X = (Count - Mu) / Sigma
        WRITE (OutUnit, 5300) NT, Count, X, Phi(X)
        WRITE (*, 5300)       NT, Count, X, Phi(X)
    END DO

    WRITE (*, 5400)
    WRITE (OutUnit, 5400)

    RETURN

5000 FORMAT (A78)
5100 FORMAT ('  No. missing words should average', F9.0, ' with sigma= ',  &
             F4.0/ '--------------------------------------------------')
5200 FORMAT (' BITSTREAM test results for', A15)
5300 FORMAT (' tst no ', I2, ': ', I7, ' missing words, ', F7.2,  &
             ' sigmas from mean, p-value=', F7.5)
5400 FORMAT (/'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/ )

END SUBROUTINE FT05_BitStream_FromRNG

!******************************************************************************

END SUBMODULE SubTest_BitStream

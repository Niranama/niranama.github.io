
SUBMODULE (ModTest_Diehard) SubTest_CountThe1s_Stream

!** PURPOSE OF THIS MODULE:
    ! contains routines to perform "Count-The-1's" test on a stream of bytes.

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tCharLen(80), PARAMETER :: TestDescription(18) = [                              &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", &
        "     ::     This is the COUNT-THE-1's TEST on a stream of bytes.      ::", &
        "     :: Consider the file under test as a stream of bytes (four per   ::", &
        "     :: 32 bit integer).  Each byte can contain from 0 to 8 1's,      ::", &
        "     :: with probabilities 1,8,28,56,70,56,28,8,1 over 256.  Now let  ::", &
        "     :: the stream of bytes provide a string of overlapping  5-letter ::", &
        "     :: words, each 'letter' taking values A,B,C,D,E. The letters are ::", &
        "     :: determined by the number of 1's in a byte::  0,1,or 2 yield A,::", &
        "     :: 3 yields B, 4 yields C, 5 yields D and 6,7 or 8 yield E. Thus ::", &
        "     :: we have a monkey at a typewriter hitting five keys with vari- ::", &
        "     :: ous probabilities (37,56,70,56,37 over 256).  There are 5^5   ::", &
        "     :: possible 5-letter words, and from a string of 256,000 (over-  ::", &
        "     :: lapping) 5-letter words, counts are made on the frequencies   ::", &
        "     :: for each word.   The quadratic form in the weak inverse of    ::", &
        "     :: the covariance matrix of the cell counts provides a chisquare ::", &
        "     :: test::  Q5-Q4, the difference of the naive Pearson sums of    ::", &
        "     :: (OBS-EXP)^2/EXP on counts for 5- and 4-letter cell counts.    ::", &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::"]
    tInteger,     PARAMETER :: P(0:4) = [ 37, 56, 70, 56, 37 ]

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

MODULE SUBROUTINE FT07_CountThe1s_Stream_FromFile(InpFileName, OutUnit, InpRec)

    ! To perform the "Count-The-1's" test based on the specified input file

    ! arguments
    tCharLen(25),  INTENT(IN)       :: InpFileName
    tInteger,      INTENT(IN)       :: OutUnit
    CLASS(I8Data), INTENT(INOUT)    :: InpRec
    
    ! local variables
    tInteger        :: W, T(0:3124), S(0:624), KBits(0:255)
    tSingle         :: ChiSqr, E, Q4, Q5, Z
    tInteger        :: I, I1, I2, II, J, JJ, JK, KS, N
    
    ! execution

    ! write test description
    WRITE (*, 5000)       TestDescription(1:18)
    WRITE (OutUnit, 5000) TestDescription(1:18)

    !     OBC: overlapping-bit-count from stream of bytes

    !******Create KBits table: KBits(j)=truncated no of bits in j, -128<=j<=127
    !****Filename reads one byte at a time, as integer*1, so -128 to 127*****
    DO  JJ = 0, 255
        J = JJ
        KS = 0
        DO  I = 1, 8
            KS = KS + IAND(J, 1)
            J = SHIFTR(J, 1)
        END DO
        IF (KS < 2) KS = 2
        IF (KS > 6) KS = 6
        KBits(JJ) = KS - 2
    END DO

    N = 100
    InpRec%RecNum = 1
    WRITE (OutUnit, 5100) InpFileName
    WRITE (*, 5100)       InpFileName
    DO  JK = 1, 2
        DO  I = 0, 5 ** 4 - 1
            S(I) = 0
        END DO
        DO  I = 0, 5 ** 5 - 1
            T(I) = 0
        END DO

        !***** generate initial word with 5 random keystrokes:
        W = 5 ** 4 * KBits(InpRec%GetSample()) + 5 ** 3 * KBits(InpRec%GetSample()) +  &
            5 ** 2 * KBits(InpRec%GetSample()) + 5 * KBits(InpRec%GetSample())      +  &
                     KBits(InpRec%GetSample())
        DO  I2 = 1, N
            DO  I1 = 1, 25600
                !******Erase leftmost letter of W:
                W = MOD(W, 5**4)
                !******Boost count for that 4-letter word:
                S(W) = S(W) + 1
                !******Shift W left, add new letter, boost 5-letter word count:
                W = 5 * W + KBits(InpRec%GetSample())
                T(W) = T(W) + 1
            END DO
        END DO

        !****  Find Q4: sum(obs-exp)^2/exp for 4-letter words
        Q4 = 0
        DO  II = 0, 5 ** 4 - 1
            I = II
            E = 25600 * N
            DO  J = 0, 3
                E = E * P(MOD(I, 5)) / 256.0
                I = I / 5
            END DO
            Q4 = Q4 + (S(II)-E) ** 2 / E
        END DO
        !****  Find Q5: sum(obs-exp)^2/exp for 5-letter words
        Q5 = 0
        DO  II = 0, 5 ** 5 - 1
            I = II
            E = 25600 * N
            DO  J = 0, 4
                E = E * P(MOD(I, 5)) / 256.0
                I = I / 5
            END DO
            Q5 = Q5 + (T(II) - E) ** 2 / E
        END DO
        ChiSqr = Q5 - Q4
        Z = (ChiSqr-2500.0) / SQRT(5000.0)
        IF (JK == 1) THEN
            WRITE (*, 5200)       25600 * N
            WRITE (OutUnit, 5200) 25600 * N
            WRITE (*, *)       ' Results for COUNT-THE-1''S IN SUCCESSIVE BYTES:'
            WRITE (OutUnit, *) ' Results for COUNT-THE-1''S IN SUCCESSIVE BYTES:'
        END IF
        WRITE (OutUnit, 5300) InpFileName, ChiSqr, Z, Phi(Z)
        WRITE (*, 5300)       InpFileName, ChiSqr, Z, Phi(Z)
    END DO
    WRITE (*, 5400)
    WRITE (OutUnit, 5400)
    
    RETURN

5000 FORMAT (A78)
5100 FORMAT ('   Test results for ', A15)
5200 FORMAT (' Chi-square with 5^5-5^4=2500 d.of f. for sample size:', I7/  &
     T32, ' ChiSquare  equiv normal  p-value')
5300 FORMAT (' byte stream for ', A20, F9.2, F11.3, F13.6)
5400 FORMAT (/ '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/ )
     
END SUBROUTINE FT07_CountThe1s_Stream_FromFile

!******************************************************************************

MODULE SUBROUTINE FT07_CountThe1s_Stream_FromRNG(OutUnit, RNG)

    ! To perform the "Count-The-1's" test based on the specified generator

    ! arguments
    tInteger,               INTENT(IN)      :: OutUnit
    CLASS(BaseRNG), TARGET, INTENT(INOUT)   :: RNG
    
    ! local variables
    TYPE(I8B_Data)  :: InpRec
    tCharAlloc      :: RngName
    tInteger        :: W, T(0:3124), S(0:624), KBits(0:255)
    tSingle         :: ChiSqr, E, Q4, Q5, Z
    tInteger        :: I, I1, I2, II, J, JJ, JK, KS, N
    
    ! execution

    RngName = RNG%GetName()
    InpRec%RNG => RNG

    ! write test description
    WRITE (*, 5000)       TestDescription(1:18)
    WRITE (OutUnit, 5000) TestDescription(1:18)

    !     OBC: overlapping-bit-count from stream of bytes

    !******Create KBits table: KBits(j)=truncated no of bits in j, -128<=j<=127
    !****Filename reads one byte at a time, as integer*1, so -128 to 127*****
    DO  JJ = 0, 255
        J = JJ
        KS = 0
        DO  I = 1, 8
            KS = KS + IAND(J, 1)
            J = SHIFTR(J, 1)
        END DO
        IF (KS < 2) KS = 2
        IF (KS > 6) KS = 6
        KBits(JJ) = KS - 2
    END DO

    N = 100
    CALL RNG%ReInit()
    WRITE (OutUnit, 5100) RngName
    WRITE (*, 5100)       RngName
    DO  JK = 1, 2
        DO  I = 0, 5 ** 4 - 1
            S(I) = 0
        END DO
        DO  I = 0, 5 ** 5 - 1
            T(I) = 0
        END DO

        !***** generate initial word with 5 random keystrokes:
        W = 5 ** 4 * KBits(InpRec%GetSample()) + 5 ** 3 * KBits(InpRec%GetSample()) +  &
            5 ** 2 * KBits(InpRec%GetSample()) + 5 * KBits(InpRec%GetSample())      +  &
                     KBits(InpRec%GetSample())
        DO  I2 = 1, N
            DO  I1 = 1, 25600
                !******Erase leftmost letter of W:
                W = MOD(W, 5**4)
                !******Boost count for that 4-letter word:
                S(W) = S(W) + 1
                !******Shift W left, add new letter, boost 5-letter word count:
                W = 5 * W + KBits(InpRec%GetSample())
                T(W) = T(W) + 1
            END DO
        END DO

        !****  Find Q4: sum(obs-exp)^2/exp for 4-letter words
        Q4 = 0
        DO  II = 0, 5 ** 4 - 1
            I = II
            E = 25600 * N
            DO  J = 0, 3
                E = E * P(MOD(I, 5)) / 256.0
                I = I / 5
            END DO
            Q4 = Q4 + (S(II)-E) ** 2 / E
        END DO
        !****  Find Q5: sum(obs-exp)^2/exp for 5-letter words
        Q5 = 0
        DO  II = 0, 5 ** 5 - 1
            I = II
            E = 25600 * N
            DO  J = 0, 4
                E = E * P(MOD(I, 5)) / 256.0
                I = I / 5
            END DO
            Q5 = Q5 + (T(II) - E) ** 2 / E
        END DO
        ChiSqr = Q5 - Q4
        Z = (ChiSqr-2500.0) / SQRT(5000.0)
        IF (JK == 1) THEN
            WRITE (*, 5200)       25600 * N
            WRITE (OutUnit, 5200) 25600 * N
            WRITE (*, *)       ' Results for COUNT-THE-1''S IN SUCCESSIVE BYTES:'
            WRITE (OutUnit, *) ' Results for COUNT-THE-1''S IN SUCCESSIVE BYTES:'
        END IF
        WRITE (OutUnit, 5300) RngName, ChiSqr, Z, Phi(Z)
        WRITE (*, 5300)       RngName, ChiSqr, Z, Phi(Z)
    END DO
    WRITE (*, 5400)
    WRITE (OutUnit, 5400)
    
    NULLIFY(InpRec%RNG)
    
    RETURN

5000 FORMAT (A78)
5100 FORMAT ('   Test results for ', A15)
5200 FORMAT (' Chi-square with 5^5-5^4=2500 d.of f. for sample size:', I7/  &
     T32, ' ChiSquare  equiv normal  p-value')
5300 FORMAT (' byte stream for ', A20, F9.2, F11.3, F13.6)
5400 FORMAT (/ '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/ )

END SUBROUTINE FT07_CountThe1s_Stream_FromRNG

!******************************************************************************

END SUBMODULE SubTest_CountThe1s_Stream

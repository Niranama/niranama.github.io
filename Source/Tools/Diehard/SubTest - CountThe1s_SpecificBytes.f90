
SUBMODULE (ModTest_Diehard) SubTest_CountThe1s_SpecificBytes

!** PURPOSE OF THIS MODULE:
    ! contains routines to perform "Count-The-1's" test for specific bytes.

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tCharLen(80), PARAMETER :: TestDescription(20) = [                              &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", &
        "     ::     This is the COUNT-THE-1's TEST for specific bytes.        ::", &
        "     :: Consider the file under test as a stream of 32-bit integers.  ::", &
        "     :: From each integer, a specific byte is chosen , say the left-  ::", &
        "     :: most::  bits 1 to 8. Each byte can contain from 0 to 8 1's,   ::", &
        "     :: with probabilitie 1,8,28,56,70,56,28,8,1 over 256.  Now let   ::", &
        "     :: the specified bytes from successive integers provide a string ::", &
        "     :: of (overlapping) 5-letter words, each 'letter' taking values  ::", &
        "     :: A,B,C,D,E. The letters are determined  by the number of 1's,  ::", &
        "     :: in that byte::  0,1,or 2 ---> A, 3 ---> B, 4 ---> C, 5 ---> D,::", &
        "     :: and  6,7 or 8 ---> E.  Thus we have a monkey at a typewriter  ::", &
        "     :: hitting five keys with with various probabilities::  37,56,70,::", &
        "     :: 56,37 over 256. There are 5^5 possible 5-letter words, and    ::", &
        "     :: from a string of 256,000 (overlapping) 5-letter words, counts ::", &
        "     :: are made on the frequencies for each word. The quadratic form ::", &
        "     :: in the weak inverse of the covariance matrix of the cell      ::", &
        "     :: counts provides a chisquare test::  Q5-Q4, the difference of  ::", &
        "     :: the naive Pearson  sums of (OBS-EXP)^2/EXP on counts for 5-   ::", &
        "     :: and 4-letter cell counts.                                     ::", &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::"]
    tInteger,     PARAMETER :: P(0:4)   = [ 37, 56, 70, 56, 37 ]
    tInteger,     PARAMETER :: K(0:255) = [  &
        0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 2, 0, 0, 0,  &
        1, 0, 1, 1, 2, 0, 1, 1, 2, 1, 2, 2, 3, 0, 0, 0, 1, 0, 1, 1, 2,  &
        0, 1, 1, 2, 1, 2, 2, 3, 0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2,  &
        3, 3, 4, 0, 0, 0, 1, 0, 1, 1, 2, 0, 1, 1, 2, 1, 2, 2, 3, 0, 1,  &
        1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4, 0, 1, 1, 2, 1, 2, 2,  &
        3, 1, 2, 2, 3, 2, 3, 3, 4, 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4,  &
        3, 4, 4, 4, 0, 0, 0, 1, 0, 1, 1, 2, 0, 1, 1, 2, 1, 2, 2, 3, 0,  &
        1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4, 0, 1, 1, 2, 1, 2,  &
        2, 3, 1, 2, 2, 3, 2, 3, 3, 4, 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3,  &
        4, 3, 4, 4, 4, 0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,  &
        1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 4, 1, 2, 2, 3, 2,  &
        3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 4, 2, 3, 3, 4, 3, 4, 4, 4, 3, 4,  &
        4, 4, 4, 4, 4, 4 ]

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

MODULE SUBROUTINE FT08_CountThe1s_SpecificBytes_FromFile(InpFileName, OutUnit, InpRec)

    ! To perform the "Count-The-1's" test based on the specified input file

    ! arguments
    tCharLen(25),   INTENT(IN)      :: InpFileName
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(I32Data), INTENT(INOUT)   :: InpRec
    
    ! local variables
    tInteger        :: W, T(0:3124), S(0:624)
    tInteger        :: I, I1, I2, II, J, JK, l, M, N
    tSingle         :: ChiSqr, E, Q4, Q5, Z
    
    ! execution

    ! write test description
    WRITE (*, 5000)       TestDescription(1:20)
    WRITE (OutUnit, 5000) TestDescription(1:20)

    !     OBC: overlapping-bit-count in specified bytes
    N = 10

    WRITE (*, 5100)       InpFileName
    WRITE (OutUnit, 5100) InpFileName
    DO  JK = 1, 25
        InpRec%RecNum = 1
        S(0:624) = 0
        T(0:3124) = 0
        I = K(IAND(SHIFTR(InpRec%GetSample(), 25-JK), 255))
        J = K(IAND(SHIFTR(InpRec%GetSample(), 25-JK), 255))
        l = K(IAND(SHIFTR(InpRec%GetSample(), 25-JK), 255))
        M = K(IAND(SHIFTR(InpRec%GetSample(), 25-JK), 255))
        !***** generate initial word with 5 random keystrokes:
        W = 625 * K(IAND(SHIFTR(InpRec%GetSample(), 25-JK), 255)) +  &
            125 * I + 25 * J + 5 * l + M
        DO  I1 = 1, 25600
            DO  I2 = 1, N
                !******Erase leftmost letter of w:
                W = MOD(W, 5**4)
                !******Boost count for that 4-letter word:
                S(W) = S(W) + 1
                !******Shift w left, add new letter, boost 5-letter word count:
                I = K(IAND(SHIFTR(InpRec%GetSample(), 25-JK), 255))
                W = 5 * W + I
                T(W) = T(W) + 1
            END DO
        END DO

        !****  Find Q4: sum(obs-exp)**2/exp for 4-letter words
        Q4 = 0
        DO  II = 0, 5 ** 4 - 1
            I = II
            E = 25600 * N
            DO  J = 1, 4
                E = E * P(MOD(I, 5)) * 2.0 ** (-8)
                I = I / 5
            END DO
            Q4 = Q4 + (S(II)-E) ** 2 / E
        END DO
        !****  Find Q5: sum(obs-exp)**2/exp for 5-letter words
        Q5 = 0
        DO  II = 0, 5 ** 5 - 1
            I = II
            E = 25600 * N
            DO  J = 1, 5
                E = E * P(MOD(I, 5)) * 2.0 ** (-8)
                I = I / 5
            END DO
            Q5 = Q5 + (T(II)-E) ** 2 / E
        END DO
        ChiSqr = Q5 - Q4
        Z = (ChiSqr-2500.0) / SQRT(5000.0)
        IF (JK == 1) THEN
            WRITE (*, 5200)       25600 * N
            WRITE (OutUnit, 5200) 25600 * N
            WRITE (*, *)       ' Results for COUNT-THE 1''S IN SPECIFIED BYTES:'
            WRITE (OutUnit, *) ' Results for COUNT-THE-1''S IN SPECIFIED BYTES:'
        END IF
        WRITE (*, 5300)       JK, JK + 7, ChiSqr, Z, Phi(Z)
        WRITE (OutUnit, 5300) JK, JK + 7, ChiSqr, Z, Phi(Z)
    END DO
    WRITE (*, 5400)
    WRITE (OutUnit, 5400)
    RETURN

5000 FORMAT (A78)
5100 FORMAT ('   Test results for ', A15)
5200 FORMAT (' Chi-square with 5^5-5^4=2500 d.of f. for sample size:', I7/ &
             T10, '             ChiSquare  equiv normal  p value')
5300 FORMAT ('           bits ', I2, ' to ', I2, F9.2, F11.3, F13.6)
5400 FORMAT (/ '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/ )
     
END SUBROUTINE FT08_CountThe1s_SpecificBytes_FromFile

!******************************************************************************

MODULE SUBROUTINE FT08_CountThe1s_SpecificBytes_FromRNG(OutUnit, RNG)

    ! To perform the "Count-The-1's" test based on the specified generator

    ! arguments
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG
    
    ! local variables
    tCharAlloc      :: RngName
    tInteger        :: W, T(0:3124), S(0:624)
    tInteger        :: I, I1, I2, II, J, JK, l, M, N
    tSingle         :: ChiSqr, E, Q4, Q5, Z
    
    ! execution

    RngName = RNG%GetName()

    ! write test description
    WRITE (*, 5000)       TestDescription(1:20)
    WRITE (OutUnit, 5000) TestDescription(1:20)

    !     OBC: overlapping-bit-count in specified bytes
    N = 10

    WRITE (*, 5100)       RngName
    WRITE (OutUnit, 5100) RngName
    DO  JK = 1, 25
        CALL RNG%ReInit()
        S(0:624) = 0
        T(0:3124) = 0
        I = K(IAND(SHIFTR(GetSample(RNG), 25-JK), 255))
        J = K(IAND(SHIFTR(GetSample(RNG), 25-JK), 255))
        l = K(IAND(SHIFTR(GetSample(RNG), 25-JK), 255))
        M = K(IAND(SHIFTR(GetSample(RNG), 25-JK), 255))
        !***** generate initial word with 5 random keystrokes:
        W = 625 * K(IAND(SHIFTR(GetSample(RNG), 25-JK), 255)) +  &
            125 * I + 25 * J + 5 * l + M
        DO  I1 = 1, 25600
            DO  I2 = 1, N
                !******Erase leftmost letter of w:
                W = MOD(W, 5**4)
                !******Boost count for that 4-letter word:
                S(W) = S(W) + 1
                !******Shift w left, add new letter, boost 5-letter word count:
                I = K(IAND(SHIFTR(GetSample(RNG), 25-JK), 255))
                W = 5 * W + I
                T(W) = T(W) + 1
            END DO
        END DO

        !****  Find Q4: sum(obs-exp)**2/exp for 4-letter words
        Q4 = 0
        DO  II = 0, 5 ** 4 - 1
            I = II
            E = 25600 * N
            DO  J = 1, 4
                E = E * P(MOD(I, 5)) * 2.0 ** (-8)
                I = I / 5
            END DO
            Q4 = Q4 + (S(II)-E) ** 2 / E
        END DO
        !****  Find Q5: sum(obs-exp)**2/exp for 5-letter words
        Q5 = 0
        DO  II = 0, 5 ** 5 - 1
            I = II
            E = 25600 * N
            DO  J = 1, 5
                E = E * P(MOD(I, 5)) * 2.0 ** (-8)
                I = I / 5
            END DO
            Q5 = Q5 + (T(II)-E) ** 2 / E
        END DO
        ChiSqr = Q5 - Q4
        Z = (ChiSqr-2500.0) / SQRT(5000.0)
        IF (JK == 1) THEN
            WRITE (*, 5200)       25600 * N
            WRITE (OutUnit, 5200) 25600 * N
            WRITE (*, *)       ' Results for COUNT-THE 1''S IN SPECIFIED BYTES:'
            WRITE (OutUnit, *) ' Results for COUNT-THE-1''S IN SPECIFIED BYTES:'
        END IF
        WRITE (*, 5300)       JK, JK + 7, ChiSqr, Z, Phi(Z)
        WRITE (OutUnit, 5300) JK, JK + 7, ChiSqr, Z, Phi(Z)
    END DO
    WRITE (*, 5400)
    WRITE (OutUnit, 5400)
    RETURN

5000 FORMAT (A78)
5100 FORMAT ('   Test results for ', A15)
5200 FORMAT (' Chi-square with 5^5-5^4=2500 d.of f. for sample size:', I7/ &
             T10, '             ChiSquare  equiv normal  p value')
5300 FORMAT ('           bits ', I2, ' to ', I2, F9.2, F11.3, F13.6)
5400 FORMAT (/ '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/ )
    
END SUBROUTINE FT08_CountThe1s_SpecificBytes_FromRNG

!******************************************************************************

END SUBMODULE SubTest_CountThe1s_SpecificBytes

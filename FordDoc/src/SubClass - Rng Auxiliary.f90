
SUBMODULE (Class_BaseRNG) SubClass_Rng_Auxiliary
       
!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains auxiliary/helper routines to support random number generation.

    IMPLICIT NONE

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE FUNCTION Get_Random_Seed64() RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a random 64-bit integer value that may be useful for
    !  initializing a source of seed value(s) for instances of random
    !  number generators initialized without specifying any seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong       :: Output   !! output used as a seed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: T(8)
    tLong       :: Time1, Time2

! FLOW
    
    ! get times
    CALL SYSTEM_CLOCK(COUNT=Time1)
    CALL DATE_AND_TIME(VALUES=T)
    Time2 = T(7) + 60*(T(6) + 60*(T(5) + 24*(T(3) - 1 + 31*(T(2) - 1 + 12*T(1))))) + T(8)
    
    ! get output seed
    Output = IEOR(Mix_Stafford_13(Time1), Mix_Stafford_13(Time2))

    RETURN

END FUNCTION Get_Random_Seed64

!******************************************************************************

MODULE FUNCTION Get_Random_Seed32() RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a random 32-bit integer value that may be useful for
    !  initializing a source of seed value(s) for instances of random
    !  number generators initialized without specifying any seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger    :: Output   !! output used as a seed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: T(8)
    tLong       :: Time1, Time2

! FLOW
    
    ! get times
    CALL SYSTEM_CLOCK(COUNT=Time1)
    CALL DATE_AND_TIME(VALUES=T)
    Time2 = T(7) + 60*(T(6) + 60*(T(5) + 24*(T(3) - 1 + 31*(T(2) - 1 + 12*T(1))))) + T(8)
    
    ! get output seed
    Output = ToInt32(SHIFTR(IEOR(Mix_Stafford_13(Time1), Mix_Stafford_13(Time2)), 32))

    RETURN

END FUNCTION Get_Random_Seed32

!******************************************************************************

MODULE FUNCTION Mix_Murmur_64(Input) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute the 64-bit mixing function of the MurmurHash3 hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: Input
    tLong               :: Output

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: C1 = ToLong(Z'FF51AFD7ED558CCD')
    tLong, PARAMETER    :: C2 = ToLong(Z'C4CEB9FE1A85EC53')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Output = IEOR(Input,  SHIFTR(Input,  33)) * C1
    Output = IEOR(Output, SHIFTR(Output, 33)) * C2
    Output = IEOR(Output, SHIFTR(Output, 33))
    
    RETURN

END FUNCTION Mix_Murmur_64

!******************************************************************************

MODULE FUNCTION Mix_Stafford_13(Input) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute Stafford variant 13 of the 64-bit mixing function of
    ! the MurmurHash3 hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: Input
    tLong               :: Output

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: C1 = ToLong(Z'BF58476D1CE4E5B9')
    tLong, PARAMETER    :: C2 = ToLong(Z'94D049BB133111EB')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Output = IEOR(Input,  SHIFTR(Input,  30)) * C1
    Output = IEOR(Output, SHIFTR(Output, 27)) * C2
    Output = IEOR(Output, SHIFTR(Output, 31))
    
    RETURN

END FUNCTION Mix_Stafford_13

!******************************************************************************

MODULE FUNCTION Mix_Lea_64(Input) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute Doug Lea's 64-bit mixing function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: Input
    tLong               :: Output

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: C = ToLong(Z'DABA0B6EB09322E3')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Output = IEOR(Input,  SHIFTR(Input,  32)) * C
    Output = IEOR(Output, SHIFTR(Output, 32)) * C
    Output = IEOR(Output, SHIFTR(Output, 32))
    
    RETURN

END FUNCTION Mix_Lea_64

!******************************************************************************

MODULE FUNCTION Mix_Murmur_32(Input) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute the 32-bit mixing function of the MurmurHash3 hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: Input
    tInteger                :: Output

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: C1 = ToInteger(Z'85EBCA6B')
    tInteger, PARAMETER :: C2 = ToInteger(Z'C2B2AE35')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Output = IEOR(Input,  SHIFTR(Input,  16)) * C1
    Output = IEOR(Output, SHIFTR(Output, 13)) * C2
    Output = IEOR(Output, SHIFTR(Output, 16))
    
    RETURN

END FUNCTION Mix_Murmur_32

!******************************************************************************

MODULE FUNCTION Mix_Lea_32(Input) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute Doug Lea's 32-bit mixing function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: Input
    tInteger                :: Output

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: C = ToInteger(Z'D36D884B')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Output = IEOR(Input,  SHIFTR(Input,  16)) * C
    Output = IEOR(Output, SHIFTR(Output, 16)) * C
    Output = IEOR(Output, SHIFTR(Output, 16))
    
    RETURN

END FUNCTION Mix_Lea_32

!******************************************************************************

MODULE FUNCTION ScrambleWell(Seed, Add) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !! To transform the initial state of a generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,    INTENT(IN)    :: Seed     ! seed element
    tInteger, INTENT(IN)    :: Add      ! offset
    tLong                   :: Output   ! the transformed seed element

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: Mult = 1812433253_kLong

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! code inspired from "AbstractWell" class
    Output = Mult * IEOR(Seed, SHIFTA(Seed, 30)) + Add
    
    RETURN

END FUNCTION ScrambleWell

!******************************************************************************

MODULE SUBROUTINE Fill_State32(Seed, State)

!** PURPOSE OF THIS SUBROUTINE:
    !! To fill state based on the given seed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: Seed(0:)
    tInteger, INTENT(OUT)   :: State(0:)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, PARAMETER :: Mask = ToLong(Z'00000000FFFFFFFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: StateSize, SeedSize, MinSize
    tInteger    :: I

! FLOW
    
    StateSize = SIZE(State)
    SeedSize  = SIZE(Seed)
    MinSize   = MIN(StateSize, SeedSize)
    State(0:MinSize-1) = Seed(0:MinSize-1)
    IF (SeedSize < StateSize) THEN
        DO I = SeedSize, StateSize-1
            State(I) = ToInteger(IAND(ScrambleWell(ToLong(State(I - SeedSize)), I), Mask))
        END DO
    END IF

    RETURN

END SUBROUTINE Fill_State32

!******************************************************************************

MODULE SUBROUTINE Fill_State64(Seed, State)

!** PURPOSE OF THIS SUBROUTINE:
    !! To fill state based on the given seed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: Seed(0:)
    tLong, INTENT(OUT)  :: State(0:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: StateSize, SeedSize, MinSize
    tInteger    :: I

! FLOW
    
    StateSize = SIZE(State)
    SeedSize  = SIZE(Seed)
    MinSize   = MIN(StateSize, SeedSize)
    State(0:MinSize-1) = Seed(0:MinSize-1)
    IF (SeedSize < StateSize) THEN
        DO I = SeedSize, StateSize-1
            State(I) = ScrambleWell(State(I - SeedSize), I)
        END DO
    END IF

    RETURN

END SUBROUTINE Fill_State64

!******************************************************************************

MODULE SUBROUTINE Extend_Seed32(SeedIn, SeedOut)

!** PURPOSE OF THIS SUBROUTINE:
    !! To extend the seed if the length of SeedIn is less than that of SeedOut.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: SeedIn(0:)
    tInteger, INTENT(OUT)   :: SeedOut(0:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: InSize, OutSize, MinSize
    tInteger    :: I, X

! FLOW
    
    InSize  = SIZE(SeedIn)
    OutSize = SIZE(SeedOut)
    MinSize = MIN(InSize, OutSize)
    SeedOut(0:MinSize-1) = SeedIn(0:MinSize-1)
    IF (InSize < OutSize) THEN
        X = SeedOut(0)
        DO I = InSize, OutSize-1
            X = X + GOLDEN_RATIO_32
            SeedOut(I) = Mix_Murmur_32(X)
        END DO
    END IF

    RETURN

END SUBROUTINE Extend_Seed32

!******************************************************************************

MODULE SUBROUTINE Extend_Seed64(SeedIn, SeedOut)

!** PURPOSE OF THIS SUBROUTINE:
    !! To extend the seed if the length of SeedIn is less than that of SeedOut.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)    :: SeedIn(0:)
    tLong, INTENT(OUT)   :: SeedOut(0:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: InSize, OutSize, MinSize
    tInteger    :: I
    tLong       :: X

! FLOW
    
    InSize  = SIZE(SeedIn)
    OutSize = SIZE(SeedOut)
    MinSize = MIN(InSize, OutSize)
    SeedOut(0:MinSize-1) = SeedIn(0:MinSize-1)
    IF (InSize < OutSize) THEN
        X = SeedOut(0)
        DO I = InSize, OutSize-1
            X = X + GOLDEN_RATIO_64
            SeedOut(I) = Mix_Stafford_13(X)
        END DO
    END IF

    RETURN

END SUBROUTINE Extend_Seed64

!******************************************************************************

END SUBMODULE SubClass_Rng_Auxiliary
    
!******************************************************************************

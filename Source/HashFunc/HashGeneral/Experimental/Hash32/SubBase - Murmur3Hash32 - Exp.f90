
SUBMODULE (ModBase_ExperimentalHash32) SubBase_Murmur3Hash32_Exp

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an experimental implementation for the Murmur3 hash algorithm
!   for 32-bit-integer output by Austin Appleby. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE FUNCTION Murmur3_Hash32_Exp(Input, InpSize, Algo, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the Murmur3 hash algorithm by Austin Appleby.
    !  This is an interface routine that calls the working routine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), DIMENSION(..), INTENT(IN)  :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
    tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-7)
    tInteger, OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
    tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tInteger                            :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER      :: InpPtr(:)
    tInteger            :: Seed

!** FLOW

    ! initialize
    SET_OPTION(Seed, 0_kInteger, StartHash)
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)

    ! perform hashing
    SELECT CASE (Algo)
    CASE (1)
        HashCode = Murmur3_Hash32(InpPtr, Seed, Pack_I32_A1)
    CASE (2)
        HashCode = Murmur3_Hash32(InpPtr, Seed, Pack_I32_A2)
    CASE (3)
        HashCode = Murmur3_Hash32(InpPtr, Seed, Pack_I32_A3)
    CASE (4)
        HashCode = Murmur3_Hash32(InpPtr, Seed, Pack_I32_A4)
    CASE (5)
        HashCode = Murmur3_Hash32(InpPtr, Seed, Pack_I32_A5)
    CASE (6)
        HashCode = Murmur3_Hash32(InpPtr, Seed, Pack_I32_A6)
    CASE (7)
        HashCode = Murmur3_Hash32(InpPtr, Seed, Pack_I32_A7)
    END SELECT

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION Murmur3_Hash32_Exp

!******************************************************************************

FUNCTION Murmur3_Hash32(Input, Seed, PackFull) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the Murmur3 hash algorithm by Austin Appleby.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)    :: Input(0:)    !! input bytes
    tInteger, INTENT(IN)    :: Seed         !! seed
    PROCEDURE(Pack_I32)     :: PackFull     !! procedure to convert a byte array to 32-bit integer
    tInteger                :: HashCode     !! hash code

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER     :: C1 = ToInteger(Z'CC9E2D51')
    tInteger, PARAMETER     :: C2 = ToInteger(Z'1B873593')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: H1, K1
    tIndex      :: Length, Remaining, Offset

!** FLOW

    ! initialize
    H1 = Seed
    Length = SIZE(Input)
    Remaining = Length
    Offset = 0
        
    ! perform hashing
    DO WHILE (Remaining >= 4)
        ! get input
        K1 = PackFull(Input, Offset)
        ! mixing input with constants
        K1 = K1*C1
        K1 = RotateLeft(K1, 15)
        K1 = K1*C2
        H1 = IEOR(H1, K1)
        H1 = RotateLeft(H1, 13) 
        H1 = H1*5 + ToInteger(Z'E6546B64')
        ! update indices
        Remaining = Remaining - 4
        Offset = Offset + 4
    END DO
        
    ! handle the tail
    IF (Remaining > 0) THEN
        K1 = PackPartial(Input, Offset, Remaining, PackFull)
        K1 = K1*C1
        K1 = RotateLeft(K1, 15)
        K1 = K1*C2
        H1 = IEOR(H1, K1)
    END IF
        
    ! finalization
    H1 = IEOR(H1, Length)
    CALL FinalMix(H1)
    HashCode = H1

    RETURN

END FUNCTION Murmur3_Hash32

!******************************************************************************

SUBROUTINE FinalMix(H)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform final mixing of working variables.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(INOUT) :: H

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    H = IEOR(H, SHIFTR(H, 16))
    H = H*ToInteger(Z'85EBCA6B')
    H = IEOR(H, SHIFTR(H, 13))
    H = H*ToInteger(Z'C2B2AE35')
    H = IEOR(H, SHIFTR(H, 16))

    RETURN

END SUBROUTINE FinalMix

!******************************************************************************

END SUBMODULE SubBase_Murmur3Hash32_Exp

!******************************************************************************

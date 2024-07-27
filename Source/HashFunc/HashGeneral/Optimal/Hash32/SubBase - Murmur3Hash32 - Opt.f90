
SUBMODULE (ModBase_OptimalHash32) SubBase_Murmur3Hash32_Opt

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an optimal implementation for the Murmur3 hash algorithm
!   for 32-bit-integer output by Austin Appleby. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define FinalMixing(H) \
    H = IEOR(H, SHIFTR(H, 16)); \
    H = H*ToInteger(Z'85EBCA6B'); \
    H = IEOR(H, SHIFTR(H, 13)); \
    H = H*ToInteger(Z'C2B2AE35'); \
    H = IEOR(H, SHIFTR(H, 16));
#define MaskInteger(X)          IAND(ToInteger(X), Z'000000FF')
#define UnsignedShort(Val, Off) IOR(MaskInteger(Val(Off)), SHIFTL(MaskInteger(Val(Off+1)), 8))
#define PackFull(Buf, Off)      IOR(UnsignedShort(Buf, Off), SHIFTL(UnsignedShort(Buf, Off+2), 16))

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

MODULE FUNCTION Murmur3_Hash32_Opt(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the Murmur3 hash algorithm by Austin Appleby.
    !  This is an interface routine that calls the working routine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), DIMENSION(..), INTENT(IN)  :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
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
    HashCode = Murmur3_Hash32(InpPtr, Seed)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION Murmur3_Hash32_Opt

!******************************************************************************

FUNCTION Murmur3_Hash32(Input, Seed) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the Murmur3 hash algorithm by Austin Appleby.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)    :: Input(0:)    !! input bytes
    tInteger, INTENT(IN)    :: Seed         !! seed
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
        K1 = Pack_Partial(Input, Offset, Remaining)
        K1 = K1*C1
        K1 = RotateLeft(K1, 15)
        K1 = K1*C2
        H1 = IEOR(H1, K1)
    END IF
        
    ! finalization
    H1 = IEOR(H1, Length)
    FinalMixing(H1)
    HashCode = H1

    RETURN

CONTAINS

    FUNCTION Pack_Partial(Buf, Off, Length) RESULT(Res)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To pack seven or fewer bytes of the array 'Buf' at offset 'Off'
        ! into the 64-bit word 'Res', in little-endian convention
        ! (least significant byte first).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,  INTENT(IN)  :: Buf(0:)  ! buffer
        tIndex, INTENT(IN)  :: Off      ! offset
        tIndex, INTENT(IN)  :: Length   ! the number of bytes to pack (between 1 to 3)
        tInteger            :: Res      ! result

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tByte       :: Wrk(0:3)
        tIndex      :: I

    ! FLOW
        
        Wrk(0:Length-1) = Buf(Off:Off+Length-1)
        Wrk(Length:3)   = 0_kByte
        Res = PackFull(Wrk, 0)

        RETURN

    END FUNCTION Pack_Partial

    !**************************************************************************

END FUNCTION Murmur3_Hash32

!******************************************************************************

END SUBMODULE SubBase_Murmur3Hash32_Opt

!******************************************************************************

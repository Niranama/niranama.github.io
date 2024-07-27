
SUBMODULE (ModBase_ReferenceHash32) SubBase_SuperFastHash_Ref

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains a reference implementation for the SuperFast hash algorithm
!   by Paul Hsieh. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    USE Class_ByteConverter
    
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

MODULE FUNCTION SuperFastHash_I32(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the SuperFast hash algorithm by Paul Hsieh.
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
    TYPE(ByteConverter) :: ByteConv

!** FLOW

    ! initialize
    SET_OPTION(Seed, 0_kInteger, StartHash)
    CALL ByteConv%Initialize()
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)

    ! perform hashing
    HashCode = SuperFast_Hash32(InpPtr, Seed, ByteConv)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION SuperFastHash_I32

!******************************************************************************

FUNCTION SuperFast_Hash32(Input, Seed, BC) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the SuperFast hash algorithm by Paul Hsieh.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN) :: Input(0:)    !! input bytes
    tInteger,            INTENT(IN) :: Seed         !! seed
    TYPE(ByteConverter), INTENT(IN) :: BC           !! byte converter
    tInteger                        :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: Tmp
    tIndex      :: Length, Remaining, Offset

!** FLOW

    ! initialize
    HashCode = Seed
    Length = SIZE(Input)
    Remaining = Length
    Offset = 0

    ! main loop
    DO WHILE (Remaining >= 4)
        ! get input and mixing
        HashCode = HashCode + BC%Pack_U16(Input, Offset)
        Tmp = IEOR(SHIFTL(BC%Pack_U16(Input, Offset + 2), 11), HashCode)
        HashCode = IEOR(SHIFTL(HashCode, 16), Tmp)
        HashCode = HashCode + SHIFTA(HashCode, 11)
        ! update indices
        Remaining = Remaining - 4
        Offset = Offset + 4
    END DO

    ! handle end cases
    SELECT CASE (Remaining)
    CASE (3)
        HashCode = HashCode + BC%Pack_U16(Input, Offset)
        HashCode = IEOR(HashCode, SHIFTL(HashCode, 16))
        HashCode = IEOR(HashCode, SHIFTL(BC%Get_U8(Input, Offset + 2), 18))
        HashCode = HashCode + SHIFTA(HashCode, 11)
    CASE (2)
        HashCode = HashCode + BC%Pack_U16(Input, Offset)
        HashCode = IEOR(HashCode, SHIFTL(HashCode, 11))
        HashCode = HashCode + SHIFTA(HashCode, 17)
    CASE (1)
        HashCode = HashCode + BC%Get_U8(Input, Offset);
        HashCode = IEOR(HashCode, SHIFTL(HashCode, 10))
        HashCode = HashCode + SHIFTA(HashCode, 1)
    END SELECT

    ! force "avalanching" of final 127 bits
    HashCode = IEOR(HashCode, SHIFTL(HashCode, 3))
    HashCode = HashCode + SHIFTA(HashCode, 5)
    HashCode = IEOR(HashCode, SHIFTL(HashCode, 4))
    HashCode = HashCode + SHIFTA(HashCode, 17)
    HashCode = IEOR(HashCode, SHIFTL(HashCode, 25))
    HashCode = HashCode + SHIFTA(HashCode, 6)

    RETURN

END FUNCTION SuperFast_Hash32

!******************************************************************************

END SUBMODULE SubBase_SuperFastHash_Ref

!******************************************************************************

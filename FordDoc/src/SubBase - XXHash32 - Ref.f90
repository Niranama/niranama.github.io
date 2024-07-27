
SUBMODULE (ModBase_ReferenceHash32) SubBase_XXHash_Ref

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains a reference implementation for the XXHash hash algorithm
!   for 32-bit-integer output by Yann Collet. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    USE Class_ByteConverter
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tInteger, PARAMETER     :: XXH_PRIME32_1 = ToInteger(Z'9E3779B1')
    tInteger, PARAMETER     :: XXH_PRIME32_2 = ToInteger(Z'85EBCA77')
    tInteger, PARAMETER     :: XXH_PRIME32_3 = ToInteger(Z'C2B2AE3D')
    tInteger, PARAMETER     :: XXH_PRIME32_4 = ToInteger(Z'27D4EB2F')
    tInteger, PARAMETER     :: XXH_PRIME32_5 = ToInteger(Z'165667B1')

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE FUNCTION XXHash_I32(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the XXHash hash algorithm by Yann Collet.
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
    HashCode = XX_Hash32(InpPtr, Seed, ByteConv)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION XXHash_I32

!******************************************************************************

FUNCTION XX_Hash32(Input, Seed, BC) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the XXHash hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN) :: Input(0:)    !! input bytes
    tInteger,            INTENT(IN) :: Seed         !! seed
    TYPE(ByteConverter), INTENT(IN) :: BC           !! byte converter
    tInteger                        :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: V1, V2, V3, V4
    tIndex      :: Length, Remaining, Offset

!** FLOW

    ! initialize
    Length = SIZE(Input)
    Remaining = Length
    Offset = 0

    ! perform hashing
    IF (Remaining >= 16) THEN
        ! initialize
        V1 = Seed + XXH_PRIME32_1 + XXH_PRIME32_2
        V2 = Seed + XXH_PRIME32_2
        V3 = Seed
        V4 = Seed - XXH_PRIME32_1
        DO
            ! perform 4 rounds
            CALL XXH32_Round(V1, BC%Pack_I32(Input, Offset))
            CALL XXH32_Round(V2, BC%Pack_I32(Input, Offset+4))
            CALL XXH32_Round(V3, BC%Pack_I32(Input, Offset+8))
            CALL XXH32_Round(V4, BC%Pack_I32(Input, Offset+12))
            ! update indices
            Offset = Offset + 16
            Remaining = Remaining - 16
            IF (Remaining < 16) EXIT
        END DO

        HashCode = RotateLeft(V1, 1) + RotateLeft(V2, 7) + &
                   RotateLeft(V3, 12) + RotateLeft(V4, 18)
    ELSE
        HashCode = Seed + XXH_PRIME32_5
    END IF

    HashCode = HashCode + ToInteger(Length)

    ! XXH32_finalize
    DO WHILE (Remaining >= 4)
        HashCode = HashCode + BC%Pack_I32(Input, Offset)*XXH_PRIME32_3
        HashCode = RotateLeft(HashCode, 17)*XXH_PRIME32_4
        Offset = Offset + 4
        Remaining = Remaining - 4
    END DO

    DO WHILE (Remaining /= 0)
        HashCode = HashCode + BC%Get_U8(Input, Offset)*XXH_PRIME32_5
        HashCode = RotateLeft(HashCode, 11)*XXH_PRIME32_1
        Offset = Offset + 1
        Remaining = Remaining - 1
    END DO

    ! XXH32_avalanche
    HashCode = IEOR(HashCode, SHIFTR(HashCode, 15))
    HashCode = HashCode*XXH_PRIME32_2
    HashCode = IEOR(HashCode, SHIFTR(HashCode, 13))
    HashCode = HashCode*XXH_PRIME32_3
    HashCode = IEOR(HashCode, SHIFTR(HashCode, 16))

    RETURN

END FUNCTION XX_Hash32

!******************************************************************************

SUBROUTINE XXH32_Round(Acc, Inp)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform mixing

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(INOUT) :: Acc
    tInteger, INTENT(IN)    :: Inp

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Acc = Acc + Inp*XXH_PRIME32_2
    Acc = RotateLeft(Acc, 13)
    Acc = Acc*XXH_PRIME32_1

    RETURN

END SUBROUTINE XXH32_Round

!******************************************************************************

END SUBMODULE SubBase_XXHash_Ref

!******************************************************************************

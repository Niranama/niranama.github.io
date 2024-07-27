
SUBMODULE (ModBase_ReferenceHash64) SubBase_MetroHash_Ref

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains a reference implementation for the MetroHash64 hash algorithm
!   by J. Andrew Rogers. <br>

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

MODULE FUNCTION MetroHash_I64(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the MetroHash64 hash algorithm by J. Andrew Rogers.
    !  This is an interface routine that calls the working routine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), DIMENSION(..), INTENT(IN)  :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
    tLong,    OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
    tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tLong                               :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER      :: InpPtr(:)
    tLong               :: Seed
    TYPE(ByteConverter) :: ByteConv

!** FLOW

    ! initialize
    SET_OPTION(Seed, 0_kInteger, StartHash)
    CALL ByteConv%Initialize()
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)

    ! perform hashing
    HashCode = Metro_Hash64(InpPtr, Seed, ByteConv)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION MetroHash_I64

!******************************************************************************

FUNCTION Metro_Hash64(Input, Seed, BC) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the MetroHash64 hash algorithm by J. Andrew Rogers.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN) :: Input(0:)    !! input bytes
    tLong,               INTENT(IN) :: Seed         !! seed
    TYPE(ByteConverter), INTENT(IN) :: BC           !! byte converter
    tLong                           :: HashCode     !! hash code

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: K0 = ToLong(Z'00000000D6D018F5')
    tLong, PARAMETER    :: K1 = ToLong(Z'00000000A2AA033B')
    tLong, PARAMETER    :: K2 = ToLong(Z'0000000062992FC1')
    tLong, PARAMETER    :: K3 = ToLong(Z'0000000030BC5B29')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: V0, V1, V2, V3
    tIndex      :: Length, Remaining, Offset

!** FLOW

    ! initialize
    HashCode = (Seed + K2)*K0
    Length = SIZE(Input)
    Remaining = Length
    Offset = 0_kIndex

    ! perform hashing
    IF (Length >= 32_kIndex) THEN
        ! initialize
        V0 = HashCode
        V1 = HashCode
        V2 = HashCode
        V3 = HashCode
        DO
            ! get data and mix them
            V0 = V0 + BC%Pack_I64(Input, Offset)*K0
            V0 = RotateRight(V0, 29) + V2
            V1 = V1 + BC%Pack_I64(Input, Offset+8_kIndex)*K1
            V1 = RotateRight(V1, 29) + V3
            V2 = V2 + BC%Pack_I64(Input, Offset+16_kIndex)*K2
            V2 = RotateRight(V2, 29) + V0
            V3 = V3 + BC%Pack_I64(Input, Offset+24_kIndex)*K3
            V3 = RotateRight(V3, 29) + V1
            ! update indices
            Offset    = Offset + 32_kIndex
            Remaining = Remaining - 32_kIndex
            IF (Remaining < 32_kIndex) EXIT
        END DO

        V2 = IEOR(V2, RotateRight(((V0 + V3)*K0) + V1, 37)*K1)
        V3 = IEOR(V3, RotateRight(((V1 + V2)*K1) + V0, 37)*K0)
        V0 = IEOR(V0, RotateRight(((V0 + V2)*K0) + V3, 37)*K1)
        V1 = IEOR(V1, RotateRight(((V1 + V3)*K1) + V2, 37)*K0)

        HashCode = HashCode + IEOR(V0, V1)
    END IF

    IF (Remaining >= 16_kIndex) THEN
        V0 = HashCode + (BC%Pack_I64(Input, Offset)*K2)
        V0 = RotateRight(V0, 29)*K3
        V1 = HashCode + (BC%Pack_I64(Input, Offset+8_kIndex)*K2)
        V1 = RotateRight(V1, 29)*K3
        V0 = IEOR(V0, RotateRight(V0*K0, 21) + V1)
        V1 = IEOR(V1, RotateRight(V1*K3, 21) + V0)
        HashCode = HashCode + V1

        ! update indices
        Offset    = Offset + 16_kIndex
        Remaining = Remaining - 16_kIndex
    END IF

    IF (Remaining >= 8_kIndex) THEN
        HashCode = HashCode + BC%Pack_I64(Input, Offset)*K3
        HashCode = IEOR(HashCode, RotateRight(HashCode, 55)*K1)

        ! update indices
        Offset    = Offset + 8_kIndex
        Remaining = Remaining - 8_kIndex
    END IF

    IF (Remaining >= 4_kIndex) THEN
        HashCode = HashCode + BC%Pack_U32(Input, Offset)*K3
        HashCode = IEOR(HashCode, RotateRight(HashCode, 26)*K1)

        ! update indices
        Offset    = Offset + 4_kIndex
        Remaining = Remaining - 4_kIndex
    END IF

    IF (Remaining >= 2_kIndex) THEN
        HashCode = HashCode + BC%Pack_U16(Input, Offset)*K3
        HashCode = IEOR(HashCode, RotateRight(HashCode, 48)*K1)

        ! update indices
        Offset    = Offset + 2_kIndex
        Remaining = Remaining - 2_kIndex
    END IF

    IF (Remaining >= 1_kIndex) THEN
        HashCode = HashCode + BC%Get_U8(Input, Offset)*K3
        HashCode = IEOR(HashCode, RotateRight(HashCode, 37)*K1)
    END IF

    HashCode = IEOR(HashCode, RotateRight(HashCode, 28))
    HashCode = HashCode*K0
    HashCode = IEOR(HashCode, RotateRight(HashCode, 29))

    RETURN

END FUNCTION Metro_Hash64

!******************************************************************************

END SUBMODULE SubBase_MetroHash_Ref

!******************************************************************************

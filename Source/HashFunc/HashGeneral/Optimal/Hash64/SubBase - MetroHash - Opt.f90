
SUBMODULE (ModBase_OptimalHash64) SubBase_MetroHash_Opt

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an optimal implementation for the MetroHash64 hash algorithm
!   by J. Andrew Rogers. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define MaskI8(X)               IAND(ToLong(X), Z'00000000000000FF')
#define MaskI16(X)              IAND(ToLong(X), Z'000000000000FFFF')
#define MaskI32(X)              IAND(ToLong(X), Z'00000000FFFFFFFF')
#define Pack_U16(Buf,Index)     MaskI16(PackShort(Buf, Index))
#define Pack_U32(Buf,Index)     MaskI32(PackInteger(Buf, Index))

!** MODULE PARAMETERS:
    tLong, PARAMETER    :: K0 = ToLong(Z'00000000D6D018F5')
    tLong, PARAMETER    :: K1 = ToLong(Z'00000000A2AA033B')
    tLong, PARAMETER    :: K2 = ToLong(Z'0000000062992FC1')
    tLong, PARAMETER    :: K3 = ToLong(Z'0000000030BC5B29')

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE FUNCTION Metro_Hash64_Opt(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

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

!** FLOW

    ! initialize
    SET_OPTION(Seed, 0_kInteger, StartHash)
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)

    ! perform hashing
    HashCode = Metro_Hash64(InpPtr, Seed)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION Metro_Hash64_Opt

!******************************************************************************

FUNCTION Metro_Hash64(Input, Seed) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the MetroHash64 hash algorithm by J. Andrew Rogers.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)    :: Input(0:)    !! input bytes
    tLong,    INTENT(IN)    :: Seed         !! seed
    tLong                   :: HashCode     !! hash code

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
            V0 = V0 + PackLong(Input, Offset)*K0
            V0 = RotateRight(V0, 29) + V2
            V1 = V1 + PackLong(Input, Offset+8_kIndex)*K1
            V1 = RotateRight(V1, 29) + V3
            V2 = V2 + PackLong(Input, Offset+16_kIndex)*K2
            V2 = RotateRight(V2, 29) + V0
            V3 = V3 + PackLong(Input, Offset+24_kIndex)*K3
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
        V0 = HashCode + (PackLong(Input, Offset)*K2)
        V0 = RotateRight(V0, 29)*K3
        V1 = HashCode + (PackLong(Input, Offset+8_kIndex)*K2)
        V1 = RotateRight(V1, 29)*K3
        V0 = IEOR(V0, RotateRight(V0*K0, 21) + V1)
        V1 = IEOR(V1, RotateRight(V1*K3, 21) + V0)
        HashCode = HashCode + V1

        ! update indices
        Offset    = Offset + 16_kIndex
        Remaining = Remaining - 16_kIndex
    END IF

    IF (Remaining >= 8_kIndex) THEN
        HashCode = HashCode + PackLong(Input, Offset)*K3
        HashCode = IEOR(HashCode, RotateRight(HashCode, 55)*K1)

        ! update indices
        Offset    = Offset + 8_kIndex
        Remaining = Remaining - 8_kIndex
    END IF

    IF (Remaining >= 4_kIndex) THEN
        HashCode = HashCode + Pack_U32(Input, Offset)*K3
        HashCode = IEOR(HashCode, RotateRight(HashCode, 26)*K1)

        ! update indices
        Offset    = Offset + 4_kIndex
        Remaining = Remaining - 4_kIndex
    END IF

    IF (Remaining >= 2_kIndex) THEN
        HashCode = HashCode + Pack_U16(Input, Offset)*K3
        HashCode = IEOR(HashCode, RotateRight(HashCode, 48)*K1)

        ! update indices
        Offset    = Offset + 2_kIndex
        Remaining = Remaining - 2_kIndex
    END IF

    IF (Remaining >= 1_kIndex) THEN
        HashCode = HashCode + MaskI8(Input(Offset))*K3
        HashCode = IEOR(HashCode, RotateRight(HashCode, 37)*K1)
    END IF

    HashCode = IEOR(HashCode, RotateRight(HashCode, 28))
    HashCode = HashCode*K0
    HashCode = IEOR(HashCode, RotateRight(HashCode, 29))

    RETURN

END FUNCTION Metro_Hash64

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                             +
!                   OPTIMAL VERSIONS OF AUXILIARY ROUTINES                    +
!                                                                             +
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PURE FUNCTION PackLong(Buf, Off) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To pack the array 'Buf' at Offset 'Off' into the 64-bit word 'Res',
    ! in little-endian convention (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,  INTENT(IN)  :: Buf(0:)  ! buffer
    tIndex, INTENT(IN)  :: Off      ! Offset
    tLong               :: Res      ! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte       :: Input(0:7)
    tLong       :: Output
    EQUIVALENCE (Output, Input)

! FLOW
            
    ! implementation algorithm #7
    Input(0:7) = Buf(Off:Off+7)
    Res = Output

    RETURN

END FUNCTION PackLong

!**************************************************************************

PURE FUNCTION PackInteger(Buf, Off) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To pack the array 'Buf' at Offset 'Off' into the 32-bit word 'Res',
    ! in little-endian convention (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,  INTENT(IN)  :: Buf(0:)  ! buffer
    tIndex, INTENT(IN)  :: Off      ! Offset
    tInteger            :: Res      ! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

#define MaskInteger(X)  IAND(ToInteger(X), Z'000000FF')

    ! implementation algorithm #1
    Res = IOR(IOR(IOR(       MaskInteger(Buf(Off)),         &
                      SHIFTL(MaskInteger(Buf(Off+1)),  8)), &
                      SHIFTL(MaskInteger(Buf(Off+2)), 16)), &
                      SHIFTL(MaskInteger(Buf(Off+3)), 24))
        
#undef MaskInteger

    RETURN

END FUNCTION PackInteger

!******************************************************************************

PURE FUNCTION PackShort(Buf, Off) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To pack the array 'Buf' at offset 'Off' into the 16-bit word 'Res',
    ! in little-endian convention (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,  INTENT(IN)  :: Buf(0:)  ! buffer
    tIndex, INTENT(IN)  :: Off      ! offset
    tShort              :: Res      ! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! implementation algorithm #3 (comparable to #1)
#define UnsignedByte(Val, Off)  IAND(ToInteger(Val(Off)), Z'000000FF')
#define UnsignedShort(Val, Off) IOR(UnsignedByte(Val, Off), SHIFTL(UnsignedByte(Val, Off+1), 8))
    Res = ToShort(UnsignedShort(Buf, Off))
#undef UnsignedByte
#undef UnsignedShort

    RETURN

END FUNCTION PackShort

!******************************************************************************

END SUBMODULE SubBase_MetroHash_Opt

!******************************************************************************


SUBMODULE (ModBase_OptimalHash64 : SubBase_XX3Hash64_Opt) SubBase_XXHash64_Opt

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an optimal implementation for the XX hash algorithms
!   for 64-bit integer output by Yann Collet. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define MaskI8(X)       IAND(ToLong(X), Z'00000000000000FF')
#define MaskI32(X)      IAND(ToLong(X), Z'00000000FFFFFFFF')

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

MODULE FUNCTION XX_Hash64_Opt(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the XXHash64 hash algorithm by Yann Collet.
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
    HashCode = XX_Hash64(InpPtr, Seed)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION XX_Hash64_Opt

!******************************************************************************

FUNCTION XX_Hash64(Input, Seed) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the XXHash64 hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, INTENT(IN)   :: Input(0:)    !! input bytes
    tLong, INTENT(IN)   :: Seed         !! seed
    tLong               :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: V1, V2, V3, V4, K1
    tIndex      :: Length, Remaining, Offset

!** SUBROUTINE MACRO DEFINITIONS:
#define XXH64_Round(Acc, Inp) \
    Acc = Acc + Inp*XXH_PRIME64_2; \
    Acc = RotateLeft(Acc, 31); \
    Acc = Acc*XXH_PRIME64_1;
#define XXH64_MergeRound(Acc, Val) \
    Val = Val*XXH_PRIME64_2; \
    Val = RotateLeft(Val, 31); \
    Val = Val*XXH_PRIME64_1; \
    Acc = IEOR(Acc, Val); \
    Acc = Acc*XXH_PRIME64_1 + XXH_PRIME64_4;
#define GetU8(In, Off)      MaskI8(In(Off))
#define Pack_U32(In, Off)   MaskI32(PackInteger(In, Off))

!** FLOW

    ! initialize
    Length = SIZE(Input)
    Remaining = Length
    Offset = 0

    ! perform hashing
    IF (Remaining >= 32) THEN
        ! initialize
        V1 = Seed + XXH_PRIME64_1 + XXH_PRIME64_2
        V2 = Seed + XXH_PRIME64_2
        V3 = Seed
        V4 = Seed - XXH_PRIME64_1
        DO
            ! perform 4 rounds
            XXH64_Round(V1, PackLong(Input, Offset))
            XXH64_Round(V2, PackLong(Input, Offset+8))
            XXH64_Round(V3, PackLong(Input, Offset+16))
            XXH64_Round(V4, PackLong(Input, Offset+24))
            ! update indices
            Offset = Offset + 32
            Remaining = Remaining - 32
            IF (Remaining < 32) EXIT
        END DO

        HashCode = RotateLeft(V1, 1) + RotateLeft(V2, 7) + &
                   RotateLeft(V3, 12) + RotateLeft(V4, 18)

        XXH64_MergeRound(HashCode, V1)
        XXH64_MergeRound(HashCode, V2)
        XXH64_MergeRound(HashCode, V3)
        XXH64_MergeRound(HashCode, V4)
    ELSE
        HashCode = Seed + XXH_PRIME64_5
    END IF

    HashCode = HashCode + ToLong(Length)

    ! XXH64_finalize
    DO WHILE (Remaining >= 8)
        K1 = 0_kLong
        XXH64_Round(K1, PackLong(Input, Offset))
        HashCode = IEOR(HashCode, K1)
        HashCode = RotateLeft(HashCode, 27)*XXH_PRIME64_1 + XXH_PRIME64_4
        Offset = Offset + 8
        Remaining = Remaining - 8
    END DO

    IF (Remaining >= 4) THEN
        HashCode = IEOR(HashCode, Pack_U32(Input, Offset)*XXH_PRIME64_1)
        HashCode = RotateLeft(HashCode, 23)*XXH_PRIME64_2 + XXH_PRIME64_3
        Offset = Offset + 4
        Remaining = Remaining - 4
    END IF

    DO WHILE (Remaining /= 0)
        HashCode = IEOR(HashCode, GetU8(Input, Offset)*XXH_PRIME64_5)
        HashCode = RotateLeft(HashCode, 11)*XXH_PRIME64_1
        Offset = Offset + 1
        Remaining = Remaining - 1
    END DO

    ! XXH32_avalanche
    HashCode = IEOR(HashCode, SHIFTR(HashCode, 33))
    HashCode = HashCode*XXH_PRIME64_2
    HashCode = IEOR(HashCode, SHIFTR(HashCode, 29))
    HashCode = HashCode*XXH_PRIME64_3
    HashCode = IEOR(HashCode, SHIFTR(HashCode, 32))

    RETURN

#undef XXH64_Round
#undef XXH64_MergeRound
#undef GetU8
#undef Pack_U32

END FUNCTION XX_Hash64

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
    EQUIVALENCE (Input, Output)

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

END SUBMODULE SubBase_XXHash64_Opt

!******************************************************************************

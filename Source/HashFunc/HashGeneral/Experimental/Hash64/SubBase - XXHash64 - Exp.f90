
SUBMODULE (ModBase_ExperimentalHash64) SubBase_XXHash64_Exp

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an experimental implementation for the XX hash algorithms
!   for 64-bit integer output by Yann Collet. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    USE ModBase_ExperimentalHash32

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define MaskI8(X)       IAND(ToLong(X), Z'00000000000000FF')
#define MaskI32(X)      IAND(ToLong(X), Z'00000000FFFFFFFF')

!** MODULE PARAMETERS:
    tLong,  PARAMETER   :: MaxU32 = ToLong(Z'00000000FFFFFFFF')
    ! Primes for 64 bits
    tLong,  PARAMETER   :: XXH_PRIME64_1 = ToLong(Z'9E3779B185EBCA87')  ! < 0b1001111000110111011110011011000110000101111010111100101010000111 >
    tLong,  PARAMETER   :: XXH_PRIME64_2 = ToLong(Z'C2B2AE3D27D4EB4F')  ! < 0b1100001010110010101011100011110100100111110101001110101101001111 >
    tLong,  PARAMETER   :: XXH_PRIME64_3 = ToLong(Z'165667B19E3779F9')  ! < 0b0001011001010110011001111011000110011110001101110111100111111001 >
    tLong,  PARAMETER   :: XXH_PRIME64_4 = ToLong(Z'85EBCA77C2B2AE63')  ! < 0b1000010111101011110010100111011111000010101100101010111001100011 >
    tLong,  PARAMETER   :: XXH_PRIME64_5 = ToLong(Z'27D4EB2F165667C5')  ! < 0b0010011111010100111010110010111100010110010101100110011111000101 >

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE FUNCTION XX_Hash64_Exp(Input, InpSize, Algo, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the XXHash64 hash algorithm by Yann Collet.
    !  This is an interface routine that calls the working routine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), DIMENSION(..), INTENT(IN)  :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
    tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-9)
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
    SELECT CASE (Algo)
    CASE (1)
        HashCode = XX_Hash64(InpPtr, Seed, Pack_I64_A1, Pack_I32_A1)
    CASE (2)
        HashCode = XX_Hash64(InpPtr, Seed, Pack_I64_A2, Pack_I32_A2)
    CASE (3)
        HashCode = XX_Hash64(InpPtr, Seed, Pack_I64_A3, Pack_I32_A3)
    CASE (4)
        HashCode = XX_Hash64(InpPtr, Seed, Pack_I64_A4, Pack_I32_A4)
    CASE (5)
        HashCode = XX_Hash64(InpPtr, Seed, Pack_I64_A5, Pack_I32_A5)
    CASE (6)
        HashCode = XX_Hash64(InpPtr, Seed, Pack_I64_A6, Pack_I32_A6)
    CASE (7)
        HashCode = XX_Hash64(InpPtr, Seed, Pack_I64_A7, Pack_I32_A7)
    CASE (8)
        HashCode = XX_Hash64_08(InpPtr, Seed, Pack_I32_A6)
    CASE (9)
        HashCode = XX_Hash64_09(InpPtr, Seed, Pack_I32_A7)
    END SELECT

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION XX_Hash64_Exp

!******************************************************************************

FUNCTION XX_Hash64(Input, Seed, PackLong, PackInteger) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the XXHash64 hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, INTENT(IN)   :: Input(0:)    !! input bytes
    tLong, INTENT(IN)   :: Seed         !! seed
    PROCEDURE(Pack_I64) :: PackLong     !! procedure to convert a byte array to 64-bit integer
    PROCEDURE(Pack_I32) :: PackInteger  !! procedure to convert a byte array to 32-bit integer
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
        K1 = 0
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

!******************************************************************************

FUNCTION XX_Hash64_08(Input, Seed, PackInteger) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the XXHash hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, TARGET, INTENT(IN)   :: Input(0:)    !! input bytes
    tLong,         INTENT(IN)   :: Seed         !! seed
    PROCEDURE(Pack_I32)         :: PackInteger  !! procedure to convert a byte array to 32-bit integer
    tLong                       :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong, POINTER  :: I64Val(:) => NULL()
    tLong           :: V1, V2, V3, V4, K1
    tIndex          :: Length, Remaining, Offset, Index
    TYPE(C_PTR)     :: CPTR

!** FLOW

#define Pack_U32(Buf,Index)     MaskI32(PackInteger(Buf, Index))

    ! initialize
    Length = SIZE(Input)
    Remaining = Length
    Index = 1
    CPTR = C_LOC(Input(0))
    CALL C_F_POINTER(CPTR, I64Val, SHAPE=[Length/8])

    ! perform hashing
    IF (Remaining >= 32) THEN
        V1 = Seed + XXH_PRIME64_1 + XXH_PRIME64_2
        V2 = Seed + XXH_PRIME64_2
        V3 = Seed
        V4 = Seed - XXH_PRIME64_1
        DO
            V1 = V1 + I64Val(Index)*XXH_PRIME64_2
            V1 = RotateLeft(V1, 31)
            V1 = V1*XXH_PRIME64_1

            V2 = V2 + I64Val(Index+1)*XXH_PRIME64_2
            V2 = RotateLeft(V2, 31)
            V2 = V2*XXH_PRIME64_1

            V3 = V3 + I64Val(Index+2)*XXH_PRIME64_2
            V3 = RotateLeft(V3, 31)
            V3 = V3*XXH_PRIME64_1

            V4 = V4 + I64Val(Index+3)*XXH_PRIME64_2
            V4 = RotateLeft(V4, 31)
            V4 = V4*XXH_PRIME64_1

            Index = Index + 4
            Remaining = Remaining - 32
            IF (Remaining < 32) EXIT
        END DO

        HashCode = RotateLeft(V1, 1) + RotateLeft(V2, 7) + &
                   RotateLeft(V3, 12) + RotateLeft(V4, 18)

        V1 = V1*XXH_PRIME64_2
        V1 = RotateLeft(V1, 31)
        V1 = V1*XXH_PRIME64_1
        HashCode = IEOR(HashCode, V1)
        HashCode = HashCode*XXH_PRIME64_1 + XXH_PRIME64_4

        V2 = V2*XXH_PRIME64_2
        V2 = RotateLeft(V2, 31)
        V2 = V2*XXH_PRIME64_1
        HashCode = IEOR(HashCode, V2)
        HashCode = HashCode*XXH_PRIME64_1 + XXH_PRIME64_4

        V3 = V3*XXH_PRIME64_2
        V3 = RotateLeft(V3, 31)
        V3 = V3*XXH_PRIME64_1
        HashCode = IEOR(HashCode, V3)
        HashCode = HashCode*XXH_PRIME64_1 + XXH_PRIME64_4

        V4 = V4*XXH_PRIME64_2
        V4 = RotateLeft(V4, 31)
        V4 = V4*XXH_PRIME64_1
        HashCode = IEOR(HashCode, V4)
        HashCode = HashCode*XXH_PRIME64_1 + XXH_PRIME64_4
    ELSE
        HashCode = Seed + XXH_PRIME64_5
    END IF

    HashCode = HashCode + ToLong(Length)

    DO WHILE (Remaining >= 8)
        K1 = I64Val(Index)
        K1 = K1*XXH_PRIME64_2
        K1 = RotateLeft(K1, 31)
        K1 = K1*XXH_PRIME64_1
        HashCode = IEOR(HashCode, K1)
        HashCode = RotateLeft(HashCode, 27)*XXH_PRIME64_1 + XXH_PRIME64_4
        Index = Index + 1
        Remaining = Remaining - 8
    END DO

    ! compute offset
    Offset = (Index-1)*8

    IF (Remaining >= 4) THEN
        HashCode = IEOR(HashCode, Pack_U32(Input, Offset)*XXH_PRIME64_1)
        HashCode = RotateLeft(HashCode, 23)*XXH_PRIME64_2 + XXH_PRIME64_3
        Offset = Offset + 4
        Remaining = Remaining - 4
    END IF

    DO WHILE (Remaining /= 0)
        HashCode = IEOR(HashCode, MaskI8(Input(Offset))*XXH_PRIME64_5)
        HashCode = RotateLeft(HashCode, 11)*XXH_PRIME64_1
        Offset = Offset + 1
        Remaining = Remaining - 1
    END DO

    ! final mixing
    HashCode = IEOR(HashCode, SHIFTR(HashCode, 33))
    HashCode = HashCode*XXH_PRIME64_2
    HashCode = IEOR(HashCode, SHIFTR(HashCode, 29))
    HashCode = HashCode*XXH_PRIME64_3
    HashCode = IEOR(HashCode, SHIFTR(HashCode, 32))

    ! free pointers
    NULLIFY(I64Val)

    RETURN

#undef Pack_U32

END FUNCTION XX_Hash64_08

!******************************************************************************

FUNCTION XX_Hash64_09(Input, Seed, PackInteger) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the XXHash hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, INTENT(IN)   :: Input(0:)    !! input bytes
    tLong, INTENT(IN)   :: Seed         !! seed
    PROCEDURE(Pack_I32) :: PackInteger  !! procedure to convert a byte array to 32-bit integer
    tLong               :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong           :: V1, V2, V3, V4, K1
    tIndex          :: Length, Remaining, Offset
    tByte           :: Bytes(0:31)
    tLong           :: I64Val(1:4)
    tInteger        :: I32Val
    EQUIVALENCE(Bytes, I64Val)
    EQUIVALENCE(Bytes, I32Val)

!** FLOW

    ! initialize
    Length = SIZE(Input)
    Remaining = Length
    Offset = 0

    ! perform hashing
    IF (Remaining >= 32) THEN
        V1 = Seed + XXH_PRIME64_1 + XXH_PRIME64_2
        V2 = Seed + XXH_PRIME64_2
        V3 = Seed
        V4 = Seed - XXH_PRIME64_1
        DO
            Bytes(0:31) = Input(Offset:Offset+31)
            V1 = V1 + I64Val(1)*XXH_PRIME64_2
            V1 = RotateLeft(V1, 31)
            V1 = V1*XXH_PRIME64_1

            V2 = V2 + I64Val(2)*XXH_PRIME64_2
            V2 = RotateLeft(V2, 31)
            V2 = V2*XXH_PRIME64_1

            V3 = V3 + I64Val(3)*XXH_PRIME64_2
            V3 = RotateLeft(V3, 31)
            V3 = V3*XXH_PRIME64_1

            V4 = V4 + I64Val(4)*XXH_PRIME64_2
            V4 = RotateLeft(V4, 31)
            V4 = V4*XXH_PRIME64_1

            Offset = Offset + 32
            Remaining = Remaining - 32
            IF (Remaining < 32) EXIT
        END DO

        HashCode = RotateLeft(V1, 1) + RotateLeft(V2, 7) + &
                   RotateLeft(V3, 12) + RotateLeft(V4, 18)

        V1 = V1*XXH_PRIME64_2
        V1 = RotateLeft(V1, 31)
        V1 = V1*XXH_PRIME64_1
        HashCode = IEOR(HashCode, V1)
        HashCode = HashCode*XXH_PRIME64_1 + XXH_PRIME64_4

        V2 = V2*XXH_PRIME64_2
        V2 = RotateLeft(V2, 31)
        V2 = V2*XXH_PRIME64_1
        HashCode = IEOR(HashCode, V2)
        HashCode = HashCode*XXH_PRIME64_1 + XXH_PRIME64_4

        V3 = V3*XXH_PRIME64_2
        V3 = RotateLeft(V3, 31)
        V3 = V3*XXH_PRIME64_1
        HashCode = IEOR(HashCode, V3)
        HashCode = HashCode*XXH_PRIME64_1 + XXH_PRIME64_4

        V4 = V4*XXH_PRIME64_2
        V4 = RotateLeft(V4, 31)
        V4 = V4*XXH_PRIME64_1
        HashCode = IEOR(HashCode, V4)
        HashCode = HashCode*XXH_PRIME64_1 + XXH_PRIME64_4
    ELSE
        HashCode = Seed + XXH_PRIME64_5
    END IF

    HashCode = HashCode + ToLong(Length)

    DO WHILE (Remaining >= 8)
        Bytes(0:7) = Input(Offset:Offset+7)
        K1 = I64Val(1)
        K1 = K1*XXH_PRIME64_2
        K1 = RotateLeft(K1, 31)
        K1 = K1*XXH_PRIME64_1
        HashCode = IEOR(HashCode, K1)
        HashCode = RotateLeft(HashCode, 27)*XXH_PRIME64_1 + XXH_PRIME64_4
        Offset = Offset + 8
        Remaining = Remaining - 8
    END DO

    IF (Remaining >= 4) THEN
        Bytes(0:3) = Input(Offset:Offset+3)
        HashCode = IEOR(HashCode, MaskI32(I32Val)*XXH_PRIME64_1)
        HashCode = RotateLeft(HashCode, 23)*XXH_PRIME64_2 + XXH_PRIME64_3
        Offset = Offset + 4
        Remaining = Remaining - 4
    END IF

    DO WHILE (Remaining /= 0)
        HashCode = IEOR(HashCode, MaskI8(Input(Offset))*XXH_PRIME64_5)
        HashCode = RotateLeft(HashCode, 11)*XXH_PRIME64_1
        Offset = Offset + 1
        Remaining = Remaining - 1
    END DO

    ! final mixing
    HashCode = IEOR(HashCode, SHIFTR(HashCode, 33))
    HashCode = HashCode*XXH_PRIME64_2
    HashCode = IEOR(HashCode, SHIFTR(HashCode, 29))
    HashCode = HashCode*XXH_PRIME64_3
    HashCode = IEOR(HashCode, SHIFTR(HashCode, 32))

    RETURN

END FUNCTION XX_Hash64_09

!******************************************************************************

END SUBMODULE SubBase_XXHash64_Exp

!******************************************************************************

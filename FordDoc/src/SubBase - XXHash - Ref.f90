
SUBMODULE (ModBase_ReferenceHash64) SubBase_XXHash_Ref

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains a reference implementation for the XX and XX3 hash algorithms
!   for 64-bit and 128-bit integer output by Yann Collet. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    USE ModBase_SIntUtil,   ONLY: LongReverseBytes    => ReverseBytes, &
                                  IntegerReverseBytes => ReverseBytes
    USE ModBase_UIntUtil,   ONLY: UnsignedLongMultiplyHigh => UMul128_Upper64
    USE Class_ByteConverter

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tLong,  PARAMETER   :: MaxU32 = ToLong(Z'00000000FFFFFFFF')
    ! Pseudorandom secret taken directly from FARSH
    tByte,  PARAMETER   :: XXH3_kSecret(192) =                                      &
                       [ToByte(Z'B8'), ToByte(Z'FE'), ToByte(Z'6C'), ToByte(Z'39'), &
                        ToByte(Z'23'), ToByte(Z'A4'), ToByte(Z'4B'), ToByte(Z'BE'), &
                        ToByte(Z'7C'), ToByte(Z'01'), ToByte(Z'81'), ToByte(Z'2C'), &
                        ToByte(Z'F7'), ToByte(Z'21'), ToByte(Z'AD'), ToByte(Z'1C'), &
                        ToByte(Z'DE'), ToByte(Z'D4'), ToByte(Z'6D'), ToByte(Z'E9'), &
                        ToByte(Z'83'), ToByte(Z'90'), ToByte(Z'97'), ToByte(Z'DB'), &
                        ToByte(Z'72'), ToByte(Z'40'), ToByte(Z'A4'), ToByte(Z'A4'), &
                        ToByte(Z'B7'), ToByte(Z'B3'), ToByte(Z'67'), ToByte(Z'1F'), &
                        ToByte(Z'CB'), ToByte(Z'79'), ToByte(Z'E6'), ToByte(Z'4E'), &
                        ToByte(Z'CC'), ToByte(Z'C0'), ToByte(Z'E5'), ToByte(Z'78'), &
                        ToByte(Z'82'), ToByte(Z'5A'), ToByte(Z'D0'), ToByte(Z'7D'), &
                        ToByte(Z'CC'), ToByte(Z'FF'), ToByte(Z'72'), ToByte(Z'21'), &
                        ToByte(Z'B8'), ToByte(Z'08'), ToByte(Z'46'), ToByte(Z'74'), &
                        ToByte(Z'F7'), ToByte(Z'43'), ToByte(Z'24'), ToByte(Z'8E'), &
                        ToByte(Z'E0'), ToByte(Z'35'), ToByte(Z'90'), ToByte(Z'E6'), &
                        ToByte(Z'81'), ToByte(Z'3A'), ToByte(Z'26'), ToByte(Z'4C'), &
                        ToByte(Z'3C'), ToByte(Z'28'), ToByte(Z'52'), ToByte(Z'BB'), &
                        ToByte(Z'91'), ToByte(Z'C3'), ToByte(Z'00'), ToByte(Z'CB'), &
                        ToByte(Z'88'), ToByte(Z'D0'), ToByte(Z'65'), ToByte(Z'8B'), &
                        ToByte(Z'1B'), ToByte(Z'53'), ToByte(Z'2E'), ToByte(Z'A3'), &
                        ToByte(Z'71'), ToByte(Z'64'), ToByte(Z'48'), ToByte(Z'97'), &
                        ToByte(Z'A2'), ToByte(Z'0D'), ToByte(Z'F9'), ToByte(Z'4E'), &
                        ToByte(Z'38'), ToByte(Z'19'), ToByte(Z'EF'), ToByte(Z'46'), &
                        ToByte(Z'A9'), ToByte(Z'DE'), ToByte(Z'AC'), ToByte(Z'D8'), &
                        ToByte(Z'A8'), ToByte(Z'FA'), ToByte(Z'76'), ToByte(Z'3F'), &
                        ToByte(Z'E3'), ToByte(Z'9C'), ToByte(Z'34'), ToByte(Z'3F'), &
                        ToByte(Z'F9'), ToByte(Z'DC'), ToByte(Z'BB'), ToByte(Z'C7'), &
                        ToByte(Z'C7'), ToByte(Z'0B'), ToByte(Z'4F'), ToByte(Z'1D'), &
                        ToByte(Z'8A'), ToByte(Z'51'), ToByte(Z'E0'), ToByte(Z'4B'), &
                        ToByte(Z'CD'), ToByte(Z'B4'), ToByte(Z'59'), ToByte(Z'31'), &
                        ToByte(Z'C8'), ToByte(Z'9F'), ToByte(Z'7E'), ToByte(Z'C9'), &
                        ToByte(Z'D9'), ToByte(Z'78'), ToByte(Z'73'), ToByte(Z'64'), &
                        ToByte(Z'EA'), ToByte(Z'C5'), ToByte(Z'AC'), ToByte(Z'83'), &
                        ToByte(Z'34'), ToByte(Z'D3'), ToByte(Z'EB'), ToByte(Z'C3'), &
                        ToByte(Z'C5'), ToByte(Z'81'), ToByte(Z'A0'), ToByte(Z'FF'), &
                        ToByte(Z'FA'), ToByte(Z'13'), ToByte(Z'63'), ToByte(Z'EB'), &
                        ToByte(Z'17'), ToByte(Z'0D'), ToByte(Z'DD'), ToByte(Z'51'), &
                        ToByte(Z'B7'), ToByte(Z'F0'), ToByte(Z'DA'), ToByte(Z'49'), &
                        ToByte(Z'D3'), ToByte(Z'16'), ToByte(Z'55'), ToByte(Z'26'), &
                        ToByte(Z'29'), ToByte(Z'D4'), ToByte(Z'68'), ToByte(Z'9E'), &
                        ToByte(Z'2B'), ToByte(Z'16'), ToByte(Z'BE'), ToByte(Z'58'), &
                        ToByte(Z'7D'), ToByte(Z'47'), ToByte(Z'A1'), ToByte(Z'FC'), &
                        ToByte(Z'8F'), ToByte(Z'F8'), ToByte(Z'B8'), ToByte(Z'D1'), &
                        ToByte(Z'7A'), ToByte(Z'D0'), ToByte(Z'31'), ToByte(Z'CE'), &
                        ToByte(Z'45'), ToByte(Z'CB'), ToByte(Z'3A'), ToByte(Z'8F'), &
                        ToByte(Z'95'), ToByte(Z'16'), ToByte(Z'04'), ToByte(Z'28'), &
                        ToByte(Z'AF'), ToByte(Z'D7'), ToByte(Z'FB'), ToByte(Z'CA'), &
                        ToByte(Z'BB'), ToByte(Z'4B'), ToByte(Z'40'), ToByte(Z'7E')]
    ! Primes for 32 bits
    tLong,  PARAMETER   :: XXH_PRIME32_1 = ToLong(Z'000000009E3779B1')  ! < 0b10011110001101110111100110110001 >
    tLong,  PARAMETER   :: XXH_PRIME32_2 = ToLong(Z'0000000085EBCA77')  ! < 0b10000101111010111100101001110111 >
    tLong,  PARAMETER   :: XXH_PRIME32_3 = ToLong(Z'00000000C2B2AE3D')  ! < 0b11000010101100101010111000111101 >
    ! Primes for 64 bits
    tLong,  PARAMETER   :: XXH_PRIME64_1 = ToLong(Z'9E3779B185EBCA87')  ! < 0b1001111000110111011110011011000110000101111010111100101010000111 >
    tLong,  PARAMETER   :: XXH_PRIME64_2 = ToLong(Z'C2B2AE3D27D4EB4F')  ! < 0b1100001010110010101011100011110100100111110101001110101101001111 >
    tLong,  PARAMETER   :: XXH_PRIME64_3 = ToLong(Z'165667B19E3779F9')  ! < 0b0001011001010110011001111011000110011110001101110111100111111001 >
    tLong,  PARAMETER   :: XXH_PRIME64_4 = ToLong(Z'85EBCA77C2B2AE63')  ! < 0b1000010111101011110010100111011111000010101100101010111001100011 >
    tLong,  PARAMETER   :: XXH_PRIME64_5 = ToLong(Z'27D4EB2F165667C5')  ! < 0b0010011111010100111010110010111100010110010101100110011111000101 >
    ! only support fixed size secret
    tIndex, PARAMETER   :: NbStripesPerBlock = (192-64)/8
    tIndex, PARAMETER   :: Block_Len = 64*NbStripesPerBlock

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE FUNCTION XXHash_I64(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

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
    TYPE(ByteConverter) :: ByteConv

!** FLOW

    ! initialize
    SET_OPTION(Seed, 0_kInteger, StartHash)
    CALL ByteConv%Initialize()
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)

    ! perform hashing
    HashCode = XX_Hash64(InpPtr, Seed, ByteConv)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION XXHash_I64

!******************************************************************************

MODULE FUNCTION XX3Hash_I64(Input, InpSize, StartHash, RemoveSign, Secret) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the XX3Hash64 hash algorithm by Yann Collet.
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
    tByte,    OPTIONAL,     INTENT(IN)  :: Secret(:)
    !^ a byte (8-bit integer) array (of at least 192 bytes) representing
    !  a custom secret <br>
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
    IF (PRESENT(Secret)) THEN
        IF (SIZE(Secret) >= 192) THEN
            HashCode = XX3_Hash64(InpPtr, Seed, ByteConv, XXH3_kSecret, Secret)
        ELSE
            BLOCK
                tByte       :: CustomSecret(192)
                tInteger    :: InSize
                InSize = SIZE(Secret)
                CustomSecret(1:InSize)  = Secret(1:InSize)
                CustomSecret(InSize+1:) = XXH3_kSecret(1:)
                HashCode = XX3_Hash64(InpPtr, Seed, ByteConv, XXH3_kSecret, CustomSecret)
            END BLOCK
        END IF
    ELSE
        HashCode = XX3_Hash64(InpPtr, Seed, ByteConv, XXH3_kSecret)
    END IF

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION XX3Hash_I64

!******************************************************************************

MODULE FUNCTION XX3Hash_I128(Input, InpSize, StartHash, RemoveSign, &
                             Secret, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the XX3Hash128 hash algorithm by Yann Collet.
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
    tByte,    OPTIONAL,     INTENT(IN)  :: Secret(:)
    !^ a byte (8-bit integer) array (of at least 192 bytes) representing
    !  a custom secret <br>
    tLong,    OPTIONAL,     INTENT(OUT) :: HashPair(2)  !! dual (128-bit) hash codes
    tLong                               :: HashCode     !! single (64-bit) hash code

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
    IF (PRESENT(Secret)) THEN
        IF (SIZE(Secret) >= 192) THEN
            HashCode = XX3_Hash128(InpPtr, Seed, ByteConv, XXH3_kSecret, Secret, HashPair)
        ELSE
            BLOCK
                tByte       :: CustomSecret(192)
                tInteger    :: InSize
                InSize = SIZE(Secret)
                CustomSecret(1:InSize)  = Secret(1:InSize)
                CustomSecret(InSize+1:) = XXH3_kSecret(1:)
                HashCode = XX3_Hash128(InpPtr, Seed, ByteConv, XXH3_kSecret, CustomSecret, HashPair)
            END BLOCK
        END IF
    ELSE
        HashCode = XX3_Hash128(InpPtr, Seed, ByteConv, XXH3_kSecret, HashPair=HashPair)
    END IF

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION XX3Hash_I128

!******************************************************************************

FUNCTION XX_Hash64(Input, Seed, BC) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the XXHash64 hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN) :: Input(0:)    !! input bytes
    tLong,               INTENT(IN) :: Seed         !! seed
    TYPE(ByteConverter), INTENT(IN) :: BC           !! byte converter
    tLong                           :: HashCode     !! hash code

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
            XXH64_Round(V1, BC%Pack_I64(Input, Offset))
            XXH64_Round(V2, BC%Pack_I64(Input, Offset+8))
            XXH64_Round(V3, BC%Pack_I64(Input, Offset+16))
            XXH64_Round(V4, BC%Pack_I64(Input, Offset+24))
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
        XXH64_Round(K1, BC%Pack_I64(Input, Offset))
        HashCode = IEOR(HashCode, K1)
        HashCode = RotateLeft(HashCode, 27)*XXH_PRIME64_1 + XXH_PRIME64_4
        Offset = Offset + 8
        Remaining = Remaining - 8
    END DO

    IF (Remaining >= 4) THEN
        HashCode = IEOR(HashCode, BC%Pack_U32(Input, Offset)*XXH_PRIME64_1)
        HashCode = RotateLeft(HashCode, 23)*XXH_PRIME64_2 + XXH_PRIME64_3
        Offset = Offset + 4
        Remaining = Remaining - 4
    END IF

    DO WHILE (Remaining /= 0)
        HashCode = IEOR(HashCode, BC%Get_U8(Input, Offset)*XXH_PRIME64_5)
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

END FUNCTION XX_Hash64

!******************************************************************************

FUNCTION XX3_Hash64(Input, Seed, BC, BaseSecret, CustomSecret) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the XX3Hash64 hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,                   INTENT(IN) :: Input(0:)            !! input bytes
    tLong,                   INTENT(IN) :: Seed                 !! seed
    TYPE(ByteConverter),     INTENT(IN) :: BC                   !! byte converter
    tByte,           TARGET, INTENT(IN) :: BaseSecret(0:191)    !! base secret
    tByte, OPTIONAL, TARGET, INTENT(IN) :: CustomSecret(0:191)  !! custom secret
    tLong                               :: HashCode             !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: Length, Offset
    tLong           :: BitFlip1, BitFlip2, Input_Lo, Input_Hi, Acc
    tLong           :: S, Input1, Input2, BitFlip, Keyed
    tInteger        :: C1, C2, C3
    tLong           :: Combined
    tIndex          :: NbRounds, I

!** SUBROUTINE MACRO DEFINITIONS:
#define XXH3_Mix16B(Seed,Input,OffIn,Secret,OffSec) \
UnsignedLongMultiplyorFold(IEOR(BC%Pack_I64(Input, OffIn), BC%Pack_I64(Secret, OffSec) + Seed), \
                           IEOR(BC%Pack_I64(Input, OffIn+8), BC%Pack_I64(Secret, OffSec+8) - Seed))
#define XXH3_Mix2Accs(Acc_LH,Acc_RH,Secret,OffSec) \
UnsignedLongMultiplyorFold(IEOR(Acc_LH, BC%Pack_I64(Secret, OffSec)), \
                           IEOR(Acc_RH, BC%Pack_I64(Secret, OffSec+8)))
#define UnsignedInt(I)      IAND(ToLong(I), ToLong(Z'00000000FFFFFFFF'))

!** FLOW

    ! initialize
    Length = SIZE(Input)
    Offset = 0

    ! perform hashing
    IF (Length <= 16) THEN
        ! XXH3_Len_0to16_64b
        IF (Length > 8) THEN
            ! XXH3_Len_9to16_64b
            BitFlip1 = IEOR(BC%Pack_I64(BaseSecret, 24), BC%Pack_I64(BaseSecret, 32)) + Seed
            BitFlip2 = IEOR(BC%Pack_I64(BaseSecret, 40), BC%Pack_I64(BaseSecret, 48)) - Seed
            Input_Lo = IEOR(BC%Pack_I64(Input, Offset), BitFlip1)
            Input_Hi = IEOR(BC%Pack_I64(Input, Offset + Length - 8), BitFlip2)
            Acc = ToLong(Length) + LongReverseBytes(Input_Lo) + Input_Hi + &
                  UnsignedLongMultiplyorFold(Input_Lo, Input_Hi)
            HashCode = XXH3_Avalanche(Acc)
            RETURN
        END IF
        IF (Length >= 4) THEN
            ! XXH3_Len_4to8_64b
            S = IEOR(Seed, LongReverseBytes(IAND(Seed, MaxU32)))
            Input1 = ToLong(BC%Pack_I32(Input, Offset))     ! high int will be shifted
            Input2 = BC%Pack_U32(Input, Offset + Length - 4)
            BitFlip = IEOR(BC%Pack_I64(BaseSecret, 8), BC%Pack_I64(BaseSecret, 16)) - S
            Keyed = IEOR((Input2 + SHIFTL(Input1, 32)), BitFlip)
            HashCode = XXH3_RRMxMx(Keyed, Length)
            RETURN
        END IF
        IF (Length /= 0) THEN
            ! XXH3_Len_1to3_64b
            C1 = BC%Get_U8(Input, Offset)
            C2 = BC%Get_I8(Input, Offset + SHIFTA(Length, 1))   ! high 3 bytes will be shifted
            C3 = BC%Get_U8(Input, Offset + Length - 1)
            Combined = UnsignedInt(IOR(IOR(IOR(SHIFTL(C1, 16), SHIFTL(C2, 24)), C3), SHIFTL(Length, 8)))
            BitFlip = UnsignedInt(IEOR(BC%Pack_I32(BaseSecret, 0), BC%Pack_I32(BaseSecret, 4))) + Seed
            HashCode = IEOR(Combined, BitFlip)
            HashCode = XXH64_Avalanche(HashCode)
            RETURN
        END IF
        HashCode = IEOR(IEOR(Seed, BC%Pack_I64(BaseSecret, 56)), BC%Pack_I64(BaseSecret, 64))
        HashCode = XXH64_Avalanche(HashCode)
        RETURN
    END IF
    IF (Length <= 128) THEN
        ! XXH3_Len_17to128_64b
        Acc = ToLong(Length)*XXH_PRIME64_1
        IF (Length > 32) THEN
            IF (Length > 64) THEN
                IF (Length > 96) THEN
                    Acc = Acc + XXH3_Mix16B(Seed, Input, Offset + 48, BaseSecret, 96_kIndex)
                    Acc = Acc + XXH3_Mix16B(Seed, Input, Offset + Length - 64, BaseSecret, 112_kIndex)
                END IF
                Acc = Acc + XXH3_Mix16B(Seed, Input, Offset + 32, BaseSecret, 64_kIndex)
                Acc = Acc + XXH3_Mix16B(Seed, Input, Offset + Length - 48, BaseSecret, 80_kIndex)
            END IF
            Acc = Acc + XXH3_Mix16B(Seed, Input, Offset + 16, BaseSecret, 32_kIndex)
            Acc = Acc + XXH3_Mix16B(Seed, Input, Offset + Length - 32, BaseSecret, 48_kIndex)
        END IF
        Acc = Acc + XXH3_Mix16B(Seed, Input, Offset, BaseSecret, 0_kIndex)
        Acc = Acc + XXH3_Mix16B(Seed, Input, Offset + Length - 16, BaseSecret, 16_kIndex)
        HashCode = XXH3_Avalanche(Acc)
        RETURN
    END IF
    IF (Length <= 240) THEN
        ! XXH3_Len_129to240_64b
        Acc = ToLong(Length)*XXH_PRIME64_1
        NbRounds = Length / 16
        I = 0
        DO WHILE (I < 8)
            Acc = Acc + XXH3_Mix16B(Seed, Input, Offset + 16*I, BaseSecret, 16*I)
            I = I + 1
        END DO
        Acc = XXH3_Avalanche(Acc)
        DO WHILE (I < NbRounds)
            Acc = Acc + XXH3_Mix16B(Seed, Input, Offset + 16*I, BaseSecret, 16*(I-8) + 3)
            I = I + 1
        END DO
        ! last bytes
        Acc = Acc + XXH3_Mix16B(Seed, Input, Offset + Length - 16, BaseSecret, 136_kIndex - 17_kIndex)
        HashCode = XXH3_Avalanche(Acc)
        RETURN
    END IF

    BLOCK
        ! XXH3_hashLong_64b_internal
        tByte, POINTER  :: CSInput(:) => NULL()
        tLong   :: Acc(0:7)
        tIndex  :: Nb_Blocks, OffBlock, OffStripe, OffSec, NbStripes
        tIndex  :: N, S, J
        tLong   :: DataVal0, DataVal1, DataKey0, DataKey1
        ! initialize
        IF (PRESENT(CustomSecret)) THEN
            CSInput => CustomSecret
        ELSE
            CSInput => BaseSecret
        END IF
        Acc(0) = XXH_PRIME32_3
        Acc(1) = XXH_PRIME64_1
        Acc(2) = XXH_PRIME64_2
        Acc(3) = XXH_PRIME64_3
        Acc(4) = XXH_PRIME64_4
        Acc(5) = XXH_PRIME32_2
        Acc(6) = XXH_PRIME64_5
        Acc(7) = XXH_PRIME32_1

        ! XXH3_hashLong_internal_Loop
        Nb_Blocks = (Length - 1) / Block_Len
        DO N = 0, Nb_Blocks-1
            ! XXH3_accumulate
            OffBlock = Offset + N*Block_Len
            DO S = 0, NbStripesPerBlock-1
                ! XXH3_accumulate_512
                OffStripe = OffBlock + S*64
                OffSec = S*8
                DO J = 0, 7, 2
                    DataVal0 = BC%Pack_I64(Input, OffStripe + 8*J)
                    DataVal1 = BC%Pack_I64(Input, OffStripe + 8*(J+1))
                    DataKey0 = IEOR(DataVal0, BC%Pack_I64(CSInput, OffSec + 8*J))
                    DataKey1 = IEOR(DataVal1, BC%Pack_I64(CSInput, OffSec + 8*(J+1)))
                    ! swap adjacent lanes
                    Acc(J)   = Acc(J)   + DataVal1 + IAND(MaxU32, DataKey0)*SHIFTR(DataKey0, 32)
                    Acc(J+1) = Acc(J+1) + DataVal0 + IAND(MaxU32, DataKey1)*SHIFTR(DataKey1, 32)
                END DO
            END DO
            ! XXH3_scrambleAcc_scalar
            OffSec = 192 - 64
            DO J = 0, 7
                Acc(J) = (IEOR(IEOR(Acc(J), SHIFTR(Acc(J), 47)), &
                              BC%Pack_I64(CSInput, OffSec + 8*J)))*XXH_PRIME32_1
            END DO
        END DO

        ! last partial block
        NbStripes = ((Length - 1) - (Block_Len*Nb_Blocks)) / 64
        OffBlock = Offset + Block_Len*Nb_Blocks
        DO S = 0, NbStripes - 1
            ! XXH3_accumulate_512
            OffStripe = OffBlock + S*64
            OffSec = S*8
            DO J = 0, 7, 2
                DataVal0 = BC%Pack_I64(Input, OffStripe + 8*J)
                DataVal1 = BC%Pack_I64(Input, OffStripe + 8*(J+1))
                DataKey0 = IEOR(DataVal0, BC%Pack_I64(CSInput, OffSec + 8*J))
                DataKey1 = IEOR(DataVal1, BC%Pack_I64(CSInput, OffSec + 8*(J+1)))
                ! swap adjacent lanes
                Acc(J)   = Acc(J)   + DataVal1 + IAND(MaxU32, DataKey0)*SHIFTR(DataKey0, 32)
                Acc(J+1) = Acc(J+1) + DataVal0 + IAND(MaxU32, DataKey1)*SHIFTR(DataKey1, 32)
            END DO
        END DO

        ! last stripe
        ! XXH3_accumulate_512
        OffStripe = Offset + Length - 64
        OffSec = 192 - 64 - 7
        DO J = 0, 7, 2
            DataVal0 = BC%Pack_I64(Input, OffStripe + 8*J)
            DataVal1 = BC%Pack_I64(Input, OffStripe + 8*(J+1))
            DataKey0 = IEOR(DataVal0, BC%Pack_I64(CSInput, OffSec + 8*J))
            DataKey1 = IEOR(DataVal1, BC%Pack_I64(CSInput, OffSec + 8*(J+1)))
            ! swap adjacent lanes
            Acc(J)   = Acc(J)   + DataVal1 + IAND(MaxU32, DataKey0)*SHIFTR(DataKey0, 32)
            Acc(J+1) = Acc(J+1) + DataVal0 + IAND(MaxU32, DataKey1)*SHIFTR(DataKey1, 32)
        END DO

        ! XXH3_mergeAccs
        HashCode = ToLong(Length)*XXH_PRIME64_1                           &
                 + XXH3_Mix2Accs(Acc(0), Acc(1), CSInput, 11_kIndex)      &
                 + XXH3_Mix2Accs(Acc(2), Acc(3), CSInput, 11_kIndex + 16) &
                 + XXH3_Mix2Accs(Acc(4), Acc(5), CSInput, 11_kIndex + 32) &
                 + XXH3_Mix2Accs(Acc(6), Acc(7), CSInput, 11_kIndex + 48)

        HashCode = XXH3_Avalanche(HashCode)
        ! free pointers
        NULLIFY(CSInput)
    END BLOCK

#undef XXH3_Mix16B
#undef XXH3_Mix2Accs
#undef UnsignedInt
#undef MaskI32
#undef MaskI8

    RETURN

END FUNCTION XX3_Hash64

!******************************************************************************

FUNCTION XX3_Hash128(Input, Seed, BC, BaseSecret, CustomSecret, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the XX3Hash128 hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,                   INTENT(IN)     :: Input(0:)            !! input bytes
    tLong,                   INTENT(IN)     :: Seed                 !! seed
    TYPE(ByteConverter),     INTENT(IN)     :: BC                   !! byte converter
    tByte,           TARGET, INTENT(IN)     :: BaseSecret(0:191)    !! base secret
    tByte, OPTIONAL, TARGET, INTENT(IN)     :: CustomSecret(0:191)  !! custom secret
    tLong, OPTIONAL,         INTENT(OUT)    :: HashPair(2)          !! dual hash code
    tLong                                   :: HashCode             !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: Length, Offset
    tLong           :: Low, High
    tLong           :: BitFlipL, BitFlipH, Input_Lo, Input_Hi
    tLong           :: S, PL, Bitflip, Keyed, M128_Lo, M128_Hi
    tInteger        :: C1, C2, C3, CombinedL, CombinedH
    tLong           :: Acc0, Acc1, Input0, Input1, Input2, Input3
    tIndex          :: I
    tInteger        :: NbRounds

!** SUBROUTINE MACRO DEFINITIONS:
#define XXH3_Mix2Accs(Acc_LH,Acc_RH,Secret,OffSec) \
UnsignedLongMultiplyorFold(IEOR(Acc_LH, BC%Pack_I64(Secret, OffSec)), \
                           IEOR(Acc_RH, BC%Pack_I64(Secret, OffSec+8)))
#define XXH128_Mix32B_Once(Seed,Secret,OffSec,Acc,Input0,Input1,Input2,Input3) \
IEOR(Acc + UnsignedLongMultiplyorFold(                                  \
     IEOR(Input0, (BC%Pack_I64(Secret, OffSec) + Seed)),               \
     IEOR(Input1, (BC%Pack_I64(Secret, OffSec+8) - Seed))), (Input2 + Input3))
#define UnsignedInt(I)      ToLong(IAND(I, Z'FFFFFFFF'))

!** FLOW

    ! initialize
    Length = SIZE(Input)
    Offset = 0

    ! perform hashing
    IF (Length <= 16) THEN
        ! XXH3_len_0to16_128b
        IF (Length > 8) THEN
            ! XXH3_len_9to16_128b
            BitflipL = IEOR(BC%Pack_I64(BaseSecret, 32), BC%Pack_I64(BaseSecret, 40)) - Seed
            BitflipH = IEOR(BC%Pack_I64(BaseSecret, 48), BC%Pack_I64(BaseSecret, 56)) + Seed
            Input_Hi = BC%Pack_I64(Input, Offset + Length - 8)
            Input_Lo = IEOR(IEOR(BC%Pack_I64(Input, Offset), Input_Hi), BitflipL)
            M128_Lo = Input_Lo*XXH_PRIME64_1
            M128_Hi = UnsignedLongMultiplyHigh(Input_Lo, XXH_PRIME64_1)
            M128_Lo = M128_Lo + SHIFTL(ToLong(Length - 1), 54)
            Input_Hi = IEOR(Input_Hi, BitflipH)
            M128_Hi = M128_Hi + Input_Hi + &
                      UnsignedInt(ToInteger(Input_Hi))*(XXH_PRIME32_2 - 1_kLong)
            M128_Lo = IEOR(M128_Lo, LongReverseBytes(M128_Hi))
            Low = XXH3_Avalanche(M128_Lo*XXH_PRIME64_2)
            IF (PRESENT(HashPair)) THEN
                HashPair(1) = Low
                HashPair(2) = XXH3_Avalanche(UnsignedLongMultiplyHigh(M128_Lo, XXH_PRIME64_2) &
                            + M128_Hi*XXH_PRIME64_2)
            END IF
            HashCode = Low
            RETURN
        END IF
        IF (Length >= 4) THEN
            ! XXH3_len_4to8_128b
            S = IEOR(Seed, LongReverseBytes(IAND(Seed, MaxU32)))
            Input_Lo = BC%Pack_U32(Input, Offset)
            ! high int will be shifted
            Input_Hi = ToLong(BC%Pack_I32(Input, Offset + Length - 4))
            Bitflip = IEOR(BC%Pack_I64(BaseSecret, 16), BC%Pack_I64(BaseSecret, 24)) + S
            Keyed = IEOR(Input_Lo + SHIFTL(Input_Hi, 32), Bitflip)
            ! Shift len to the left to ensure it is even, this avoids even multiplies.
            PL = XXH_PRIME64_1 + ToLong(SHIFTL(Length, 2))
            M128_Lo = Keyed*PL
            M128_Hi = UnsignedLongMultiplyHigh(Keyed, PL)
            M128_Hi = M128_Hi + SHIFTL(M128_Lo, 1)
            M128_Lo = IEOR(M128_Lo, SHIFTR(M128_Hi, 3))
            M128_Lo = IEOR(M128_Lo, SHIFTR(M128_Lo, 35))
            M128_Lo = M128_Lo*ToLong(Z'9FB21C651E98DF25')
            M128_Lo = IEOR(M128_Lo, SHIFTR(M128_Lo, 28))
            IF (PRESENT(HashPair)) THEN
                HashPair(1) = M128_Lo
                HashPair(2) = XXH3_Avalanche(M128_Hi)
            END IF
            HashCode = M128_Lo
            RETURN
        END IF
        IF (Length /= 0) THEN
            ! XXH3_len_1to3_128b
            C1 = BC%Get_U8(Input, Offset)
            ! high 3 bytes will be shifted
            C2 = BC%Get_I8(Input, Offset + SHIFTA(Length, 1))
            C3 = BC%Get_U8(Input, Offset + Length - 1)
            CombinedL = IOR(IOR(IOR(SHIFTL(C1, 16), SHIFTL(C2, 24)), C3), SHIFTL(Length, 8))
            CombinedH = RotateLeft(IntegerReverseBytes(CombinedL), 13)
            BitFlipL = UnsignedInt(IEOR(BC%Pack_I32(BaseSecret, 0), BC%Pack_I32(BaseSecret, 4))) + Seed
            BitFlipH = UnsignedInt(IEOR(BC%Pack_I32(BaseSecret, 8), BC%Pack_I32(BaseSecret, 12))) - Seed
            Low = IEOR(UnsignedInt(CombinedL), BitFlipL)
            Low = XXH64_Avalanche(Low)
            IF (PRESENT(HashPair)) THEN
                High = IEOR(UnsignedInt(CombinedH), BitFlipH)
                High = XXH64_Avalanche(High)
                HashPair(1) = Low
                HashPair(2) = High
            END IF
            HashCode = Low
            RETURN
        END IF
        Low = XXH64_Avalanche(IEOR(IEOR(Seed, BC%Pack_I64(BaseSecret, 64)), &
                                              BC%Pack_I64(BaseSecret, 72)))
        IF (PRESENT(HashPair)) THEN
            HashPair(1) = Low
            HashPair(2) = XXH64_Avalanche(IEOR(IEOR(Seed, BC%Pack_I64(BaseSecret, 80)), &
                                                          BC%Pack_I64(BaseSecret, 88)))
        END IF
        HashCode = Low
        RETURN
    END IF
    IF (Length <= 128) THEN
        ! XXH3_len_17to128_128b
        Acc0 = ToLong(Length)*XXH_PRIME64_1
        Acc1 = 0
        IF (Length > 32) THEN
            IF (Length > 64) THEN
                IF (Length > 96) THEN
                    Input0 = BC%Pack_I64(Input, Offset + 48)
                    Input1 = BC%Pack_I64(Input, Offset + 56)
                    Input2 = BC%Pack_I64(Input, Offset + Length - 64)
                    Input3 = BC%Pack_I64(Input, Offset + Length - 56)
                    Acc0 = XXH128_Mix32B_Once(Seed, BaseSecret, 96_kIndex,  Acc0, \
                                              Input0, Input1, Input2, Input3)
                    Acc1 = XXH128_Mix32B_Once(Seed, BaseSecret, 112_kIndex, Acc1, \
                                              Input2, Input3, Input0, Input1)
                END IF
                Input0 = BC%Pack_I64(Input, Offset + 32)
                Input1 = BC%Pack_I64(Input, Offset + 40)
                Input2 = BC%Pack_I64(Input, Offset + Length - 48)
                Input3 = BC%Pack_I64(Input, Offset + Length - 40)
                Acc0 = XXH128_Mix32B_Once(Seed, BaseSecret, 64_kIndex, Acc0, \
                                          Input0, Input1, Input2, Input3)
                Acc1 = XXH128_Mix32B_Once(Seed, BaseSecret, 80_kIndex, Acc1,  \
                                          Input2, Input3, Input0, Input1)
            END IF
            Input0 = BC%Pack_I64(Input, Offset + 16)
            Input1 = BC%Pack_I64(Input, Offset + 24)
            Input2 = BC%Pack_I64(Input, Offset + Length - 32)
            Input3 = BC%Pack_I64(Input, Offset + Length - 24)
            Acc0 = XXH128_Mix32B_Once(Seed, BaseSecret, 32_kIndex, Acc0, \
                                      Input0, Input1, Input2, Input3)
            Acc1 = XXH128_Mix32B_Once(Seed, BaseSecret, 48_kIndex, Acc1, \
                                      Input2, Input3, Input0, Input1)
        END IF
        Input0 = BC%Pack_I64(Input, Offset)
        Input1 = BC%Pack_I64(Input, Offset + 8)
        Input2 = BC%Pack_I64(Input, Offset + Length - 16)
        Input3 = BC%Pack_I64(Input, Offset + Length - 8)
        Acc0 = XXH128_Mix32B_Once(Seed, BaseSecret, 0_kIndex,  Acc0, Input0, Input1, Input2, Input3)
        Acc1 = XXH128_Mix32B_Once(Seed, BaseSecret, 16_kIndex, Acc1, Input2, Input3, Input0, Input1)
        Low = XXH3_Avalanche(Acc0 + Acc1)
        IF (PRESENT(HashPair)) THEN
            HashPair(1) = Low
            HashPair(2) = -XXH3_Avalanche(Acc0*XXH_PRIME64_1 + Acc1*XXH_PRIME64_4 &
                        + (ToLong(Length) - Seed)*XXH_PRIME64_2)
        END IF
        HashCode = Low
        RETURN
    END IF
    IF (Length <= 240) THEN
        ! XXH3_len_129to240_128b
        NbRounds = Length / 32
        Acc0 = ToLong(Length)*XXH_PRIME64_1
        Acc1 = 0_kLong
        I = 0
        DO WHILE (I < 4)
            Input0 = BC%Pack_I64(Input, Offset + 32*I)
            Input1 = BC%Pack_I64(Input, Offset + 32*I + 8)
            Input2 = BC%Pack_I64(Input, Offset + 32*I + 16)
            Input3 = BC%Pack_I64(Input, Offset + 32*I + 24)
            Acc0 = XXH128_Mix32B_Once(Seed, BaseSecret, 32*I,      Acc0, \
                                      Input0, Input1, Input2, Input3)
            Acc1 = XXH128_Mix32B_Once(Seed, BaseSecret, 32*I + 16, Acc1, \
                                      Input2, Input3, Input0, Input1)
            I = I + 1
        END DO
        Acc0 = XXH3_Avalanche(Acc0)
        Acc1 = XXH3_Avalanche(Acc1)
        DO WHILE (I < NbRounds)
            Input0 = BC%Pack_I64(Input, Offset + 32*I)
            Input1 = BC%Pack_I64(Input, Offset + 32*I + 8)
            Input2 = BC%Pack_I64(Input, Offset + 32*I + 16)
            Input3 = BC%Pack_I64(Input, Offset + 32*I + 24)
            Acc0 = XXH128_Mix32B_Once(Seed, BaseSecret, 3  + 32*(I-4), Acc0, \
                                      Input0, Input1, Input2, Input3)
            Acc1 = XXH128_Mix32B_Once(Seed, BaseSecret, 19 + 32*(I-4), Acc1, \
                                      Input2, Input3, Input0, Input1)
            I = I + 1
        END DO

        ! last bytes
        Input0 = BC%Pack_I64(Input, Offset + Length - 16)
        Input1 = BC%Pack_I64(Input, Offset + Length - 8)
        Input2 = BC%Pack_I64(Input, Offset + Length - 32)
        Input3 = BC%Pack_I64(Input, Offset + Length - 24)
        Acc0 = XXH128_Mix32B_Once((-Seed), BaseSecret, 103_kIndex, Acc0, Input0, Input1, Input2, Input3)
        Acc1 = XXH128_Mix32B_Once((-Seed), BaseSecret, 119_kIndex, Acc1, Input2, Input3, Input0, Input1)

        Low = XXH3_Avalanche(Acc0 + Acc1)
        IF (PRESENT(HashPair)) THEN
            HashPair(1) = Low
            HashPair(2) = -(XXH3_Avalanche(Acc0*XXH_PRIME64_1 + Acc1*XXH_PRIME64_4 &
                                           + (ToLong(Length) - Seed)*XXH_PRIME64_2))
        END IF
        HashCode = Low
        RETURN
    END IF

    BLOCK
        ! XXH3_hashLong_128b_internal
        tByte, POINTER  :: CSInput(:) => NULL()
        tLong   :: Acc(0:7)
        tIndex  :: Nb_Blocks, OffBlock, OffStripe, OffSec, NbStripes
        tIndex  :: N, S, J
        tLong   :: DataVal0, DataVal1, DataKey0, DataKey1
        ! initialize
        IF (PRESENT(CustomSecret)) THEN
            CSInput => CustomSecret
        ELSE
            CSInput => BaseSecret
        END IF
        Acc(0) = XXH_PRIME32_3
        Acc(1) = XXH_PRIME64_1
        Acc(2) = XXH_PRIME64_2
        Acc(3) = XXH_PRIME64_3
        Acc(4) = XXH_PRIME64_4
        Acc(5) = XXH_PRIME32_2
        Acc(6) = XXH_PRIME64_5
        Acc(7) = XXH_PRIME32_1

        ! XXH3_hashLong_internal_Loop
        Nb_Blocks = (Length - 1) / Block_Len
        DO N = 0, Nb_Blocks-1
            ! XXH3_accumulate
            OffBlock = Offset + N*Block_Len
            DO S = 0, NbStripesPerBlock-1
                ! XXH3_accumulate_512
                OffStripe = OffBlock + S*64
                OffSec = S*8
                DO J = 0, 7, 2
                    DataVal0 = BC%Pack_I64(Input, OffStripe + 8*J)
                    DataVal1 = BC%Pack_I64(Input, OffStripe + 8*(J+1))
                    DataKey0 = IEOR(DataVal0, BC%Pack_I64(CSInput, OffSec + 8*J))
                    DataKey1 = IEOR(DataVal1, BC%Pack_I64(CSInput, OffSec + 8*(J+1)))
                    ! swap adjacent lanes
                    Acc(J)   = Acc(J)   + DataVal1 + IAND(MaxU32, DataKey0)*SHIFTR(DataKey0, 32)
                    Acc(J+1) = Acc(J+1) + DataVal0 + IAND(MaxU32, DataKey1)*SHIFTR(DataKey1, 32)
                END DO
            END DO
            ! XXH3_scrambleAcc_scalar
            OffSec = 192 - 64
            DO J = 0, 7
                Acc(J) = (IEOR(IEOR(Acc(J), SHIFTR(Acc(J), 47)), &
                              BC%Pack_I64(CSInput, OffSec + 8*J)))*XXH_PRIME32_1
            END DO
        END DO

        ! last partial block
        NbStripes = ((Length - 1) - (Block_Len*Nb_Blocks)) / 64
        OffBlock = Offset + Block_Len*Nb_Blocks
        DO S = 0, NbStripes - 1
            ! XXH3_accumulate_512
            OffStripe = OffBlock + S*64
            OffSec = S*8
            DO J = 0, 7, 2
                DataVal0 = BC%Pack_I64(Input, OffStripe + 8*J)
                DataVal1 = BC%Pack_I64(Input, OffStripe + 8*(J+1))
                DataKey0 = IEOR(DataVal0, BC%Pack_I64(CSInput, OffSec + 8*J))
                DataKey1 = IEOR(DataVal1, BC%Pack_I64(CSInput, OffSec + 8*(J+1)))
                ! swap adjacent lanes
                Acc(J)   = Acc(J)   + DataVal1 + IAND(MaxU32, DataKey0)*SHIFTR(DataKey0, 32)
                Acc(J+1) = Acc(J+1) + DataVal0 + IAND(MaxU32, DataKey1)*SHIFTR(DataKey1, 32)
            END DO
        END DO

        ! last stripe
        ! XXH3_accumulate_512
        OffStripe = Offset + Length - 64
        OffSec = 192 - 64 - 7
        DO J = 0, 7, 2
            DataVal0 = BC%Pack_I64(Input, OffStripe + 8*J)
            DataVal1 = BC%Pack_I64(Input, OffStripe + 8*(J+1))
            DataKey0 = IEOR(DataVal0, BC%Pack_I64(CSInput, OffSec + 8*J))
            DataKey1 = IEOR(DataVal1, BC%Pack_I64(CSInput, OffSec + 8*(J+1)))
            ! swap adjacent lanes
            Acc(J)   = Acc(J)   + DataVal1 + IAND(MaxU32, DataKey0)*SHIFTR(DataKey0, 32)
            Acc(J+1) = Acc(J+1) + DataVal0 + IAND(MaxU32, DataKey1)*SHIFTR(DataKey1, 32)
        END DO

        ! XXH3_mergeAccs
        Low = ToLong(Length)*XXH_PRIME64_1                                 &
                + XXH3_Mix2Accs(Acc(0), Acc(1), CSInput, 11_kIndex)        &
                + XXH3_Mix2Accs(Acc(2), Acc(3), CSInput, 11_kIndex + 16)   &
                + XXH3_Mix2Accs(Acc(4), Acc(5), CSInput, 11_kIndex + 16*2) &
                + XXH3_Mix2Accs(Acc(6), Acc(7), CSInput, 11_kIndex + 16*3)
        Low = XXH3_Avalanche(Low)
        IF (PRESENT(HashPair)) THEN
            High = NOT(ToLong(Length)*XXH_PRIME64_2)                                      &
                    + XXH3_Mix2Accs(Acc(0), Acc(1), CSInput, 192_kIndex - 64 - 11)        &
                    + XXH3_Mix2Accs(Acc(2), Acc(3), CSInput, 192_kIndex - 64 - 11 + 16)   &
                    + XXH3_Mix2Accs(Acc(4), Acc(5), CSInput, 192_kIndex - 64 - 11 + 16*2) &
                    + XXH3_Mix2Accs(Acc(6), Acc(7), CSInput, 192_kIndex - 64 - 11 + 16*3)
            High = XXH3_Avalanche(High)
            HashPair(1) = Low
            HashPair(2) = High
        END IF
        HashCode = Low
        ! free pointers
        NULLIFY(CSInput)
    END BLOCK

#undef XXH3_Mix2Accs
#undef XXH128_Mix32B_Once
#undef UnsignedInt

    RETURN

END FUNCTION XX3_Hash128

!******************************************************************************

FUNCTION XXH64_Avalanche(H64) RESULT(Res)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: H64
    tLong               :: Res

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Res = IEOR(H64, SHIFTR(H64, 33))*XXH_PRIME64_2
    Res = IEOR(Res, SHIFTR(Res, 29))*XXH_PRIME64_3
    Res = IEOR(Res, SHIFTR(Res, 32))

    RETURN

END FUNCTION XXH64_Avalanche

!******************************************************************************

FUNCTION XXH3_Avalanche(H64) RESULT(Res)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: H64
    tLong               :: Res

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Res = IEOR(H64, SHIFTR(H64, 37))*ToLong(Z'165667919E3779F9')
    Res = IEOR(Res, SHIFTR(Res, 32))

    RETURN

END FUNCTION XXH3_Avalanche

!******************************************************************************

FUNCTION XXH3_RRMxMx(H64, Length) RESULT(Res)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,  INTENT(IN)  :: H64
    tIndex, INTENT(IN)  :: Length
    tLong               :: Res

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Res = IEOR(H64, IEOR(RotateLeft(H64, 49), RotateLeft(H64, 24)))*ToLong(Z'9FB21C651E98DF25')
    Res = IEOR(Res, SHIFTR(Res, 35) + ToLong(Length))*ToLong(Z'9FB21C651E98DF25')
    Res = IEOR(Res, SHIFTR(Res, 28))

    RETURN

END FUNCTION XXH3_RRMxMx

!******************************************************************************

END SUBMODULE SubBase_XXHash_Ref

!******************************************************************************

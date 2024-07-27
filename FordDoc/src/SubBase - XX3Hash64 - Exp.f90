
SUBMODULE (ModBase_ExperimentalHash64) SubBase_XX3Hash64_Exp

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an experimental implementation for the XX3 hash algorithms
!   for 64-bit integer output by Yann Collet. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    USE ModBase_SIntUtil,   ONLY: LongReverseBytes    => ReverseBytes, &
                                  IntegerReverseBytes => ReverseBytes
    USE ModBase_UIntUtil,   ONLY: UnsignedLongMultiplyHigh => UMul128_Upper64
    USE ModBase_ExperimentalHash32

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define MaskI8(X)       IAND(ToLong(X), Z'00000000000000FF')
#define MaskI32(X)      IAND(ToLong(X), Z'00000000FFFFFFFF')

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

MODULE FUNCTION XX3_Hash64_Exp(Input, InpSize, Algo, StartHash, RemoveSign, Secret) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the XX3Hash64 hash algorithm by Yann Collet.
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
    tByte,    OPTIONAL,     INTENT(IN)  :: Secret(:)
    !^ a byte (8-bit integer) array (of at least 192 bytes) representing
    !  a custom secret <br>
    tLong                               :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER      :: InpPtr(:)
    tLong               :: Seed

!** FLOW

    ! initialize
    SET_OPTION(Seed, 0_kInteger, StartHash)
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)

    ! perform hashing
    IF (PRESENT(Secret)) THEN
        IF (SIZE(Secret) >= 192) THEN
            SELECT CASE (Algo)
            CASE (1)
                HashCode = XX3_Hash64(InpPtr, Seed, Pack_I64_A1, Pack_I32_A1, XXH3_kSecret, Secret)
            CASE (2)
                HashCode = XX3_Hash64(InpPtr, Seed, Pack_I64_A2, Pack_I32_A2, XXH3_kSecret, Secret)
            CASE (3)
                HashCode = XX3_Hash64(InpPtr, Seed, Pack_I64_A3, Pack_I32_A3, XXH3_kSecret, Secret)
            CASE (4)
                HashCode = XX3_Hash64(InpPtr, Seed, Pack_I64_A4, Pack_I32_A4, XXH3_kSecret, Secret)
            CASE (5)
                HashCode = XX3_Hash64(InpPtr, Seed, Pack_I64_A5, Pack_I32_A5, XXH3_kSecret, Secret)
            CASE (6)
                HashCode = XX3_Hash64(InpPtr, Seed, Pack_I64_A6, Pack_I32_A6, XXH3_kSecret, Secret)
            CASE (7)
                HashCode = XX3_Hash64(InpPtr, Seed, Pack_I64_A7, Pack_I32_A7, XXH3_kSecret, Secret)
            CASE (8)
                HashCode = XX3_Hash64_08(InpPtr, Seed, Pack_I32_A6, XXH3_kSecret, Secret)
            CASE (9)
                HashCode = XX3_Hash64_09(InpPtr, Seed, XXH3_kSecret, Secret)
            CASE (10)
                HashCode = XX3_Hash64_10(InpPtr, Seed, XXH3_kSecret, Secret)
            END SELECT
        ELSE
            BLOCK
                tByte       :: CustomSecret(192)
                tInteger    :: InSize
                InSize = SIZE(Secret)
                CustomSecret(1:InSize)  = Secret(1:InSize)
                CustomSecret(InSize+1:) = XXH3_kSecret(1:)
                SELECT CASE (Algo)
                CASE (1)
                    HashCode = XX3_Hash64(InpPtr, Seed, Pack_I64_A1, Pack_I32_A1, XXH3_kSecret, CustomSecret)
                CASE (2)
                    HashCode = XX3_Hash64(InpPtr, Seed, Pack_I64_A2, Pack_I32_A2, XXH3_kSecret, CustomSecret)
                CASE (3)
                    HashCode = XX3_Hash64(InpPtr, Seed, Pack_I64_A3, Pack_I32_A3, XXH3_kSecret, CustomSecret)
                CASE (4)
                    HashCode = XX3_Hash64(InpPtr, Seed, Pack_I64_A4, Pack_I32_A4, XXH3_kSecret, CustomSecret)
                CASE (5)
                    HashCode = XX3_Hash64(InpPtr, Seed, Pack_I64_A5, Pack_I32_A5, XXH3_kSecret, CustomSecret)
                CASE (6)
                    HashCode = XX3_Hash64(InpPtr, Seed, Pack_I64_A6, Pack_I32_A6, XXH3_kSecret, CustomSecret)
                CASE (7)
                    HashCode = XX3_Hash64(InpPtr, Seed, Pack_I64_A7, Pack_I32_A7, XXH3_kSecret, CustomSecret)
                CASE (8)
                    HashCode = XX3_Hash64_08(InpPtr, Seed, Pack_I32_A6, XXH3_kSecret, CustomSecret)
                CASE (9)
                    HashCode = XX3_Hash64_09(InpPtr, Seed, XXH3_kSecret, CustomSecret)
                CASE (10)
                    HashCode = XX3_Hash64_10(InpPtr, Seed, XXH3_kSecret, CustomSecret)
                END SELECT
            END BLOCK
        END IF
    ELSE
        SELECT CASE (Algo)
        CASE (1)
            HashCode = XX3_Hash64(InpPtr, Seed, Pack_I64_A1, Pack_I32_A1, XXH3_kSecret)
        CASE (2)
            HashCode = XX3_Hash64(InpPtr, Seed, Pack_I64_A2, Pack_I32_A2, XXH3_kSecret)
        CASE (3)
            HashCode = XX3_Hash64(InpPtr, Seed, Pack_I64_A3, Pack_I32_A3, XXH3_kSecret)
        CASE (4)
            HashCode = XX3_Hash64(InpPtr, Seed, Pack_I64_A4, Pack_I32_A4, XXH3_kSecret)
        CASE (5)
            HashCode = XX3_Hash64(InpPtr, Seed, Pack_I64_A5, Pack_I32_A5, XXH3_kSecret)
        CASE (6)
            HashCode = XX3_Hash64(InpPtr, Seed, Pack_I64_A6, Pack_I32_A6, XXH3_kSecret)
        CASE (7)
            HashCode = XX3_Hash64(InpPtr, Seed, Pack_I64_A7, Pack_I32_A7, XXH3_kSecret)
        CASE (8)
            HashCode = XX3_Hash64_08(InpPtr, Seed, Pack_I32_A6, XXH3_kSecret)
        CASE (9)
            HashCode = XX3_Hash64_09(InpPtr, Seed, XXH3_kSecret)
        CASE (10)
            HashCode = XX3_Hash64_10(InpPtr, Seed, XXH3_kSecret)
        END SELECT
    END IF

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION XX3_Hash64_Exp

!******************************************************************************

FUNCTION XX3_Hash64(Input, Seed, PackLong, PackInteger, BaseSecret, CustomSecret) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the XX3Hash64 hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,                   INTENT(IN) :: Input(0:)            !! input bytes
    tLong,                   INTENT(IN) :: Seed                 !! seed
    PROCEDURE(Pack_I64)                 :: PackLong             !! procedure to convert a byte array to 64-bit integer
    PROCEDURE(Pack_I32)                 :: PackInteger          !! procedure to convert a byte array to 32-bit integer
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
UnsignedLongMultiplyorFold(IEOR(PackLong(Input, OffIn), PackLong(Secret, OffSec) + Seed), \
                           IEOR(PackLong(Input, OffIn+8), PackLong(Secret, OffSec+8) - Seed))
#define XXH3_Mix2Accs(Acc_LH,Acc_RH,Secret,OffSec) \
UnsignedLongMultiplyorFold(IEOR(Acc_LH, PackLong(Secret, OffSec)), \
                           IEOR(Acc_RH, PackLong(Secret, OffSec+8)))
#define UnsignedInt(I)      IAND(ToLong(I), ToLong(Z'00000000FFFFFFFF'))
#define GetI8(In, Off)      ToInteger(In(Off))
#define GetU8(In, Off)      MaskI8(In(Off))
#define Pack_U32(In, Off)   MaskI32(PackInteger(In, Off))

!** FLOW

    ! initialize
    Length = SIZE(Input)
    Offset = 0

    ! perform hashing
    IF (Length <= 16) THEN
        ! XXH3_Len_0to16_64b
        IF (Length > 8) THEN
            ! XXH3_Len_9to16_64b
            BitFlip1 = IEOR(PackLong(BaseSecret, 24), PackLong(BaseSecret, 32)) + Seed
            BitFlip2 = IEOR(PackLong(BaseSecret, 40), PackLong(BaseSecret, 48)) - Seed
            Input_Lo = IEOR(PackLong(Input, Offset), BitFlip1)
            Input_Hi = IEOR(PackLong(Input, Offset + Length - 8), BitFlip2)
            Acc = ToLong(Length) + LongReverseBytes(Input_Lo) + Input_Hi + &
                  UnsignedLongMultiplyorFold(Input_Lo, Input_Hi)
            HashCode = XXH3_Avalanche(Acc)
            RETURN
        END IF
        IF (Length >= 4) THEN
            ! XXH3_Len_4to8_64b
            S = IEOR(Seed, LongReverseBytes(IAND(Seed, MaxU32)))
            Input1 = ToLong(PackInteger(Input, Offset))     ! high int will be shifted
            Input2 = Pack_U32(Input, Offset + Length - 4)
            BitFlip = IEOR(PackLong(BaseSecret, 8), PackLong(BaseSecret, 16)) - S
            Keyed = IEOR((Input2 + SHIFTL(Input1, 32)), BitFlip)
            HashCode = XXH3_RRMxMx(Keyed, Length)
            RETURN
        END IF
        IF (Length /= 0) THEN
            ! XXH3_Len_1to3_64b
            C1 = GetU8(Input, Offset)
            C2 = GetI8(Input, Offset + SHIFTA(Length, 1))   ! high 3 bytes will be shifted
            C3 = GetU8(Input, Offset + Length - 1)
            Combined = UnsignedInt(IOR(IOR(IOR(SHIFTL(C1, 16), SHIFTL(C2, 24)), C3), SHIFTL(Length, 8)))
            BitFlip = UnsignedInt(IEOR(PackInteger(BaseSecret, 0), PackInteger(BaseSecret, 4))) + Seed
            HashCode = IEOR(Combined, BitFlip)
            HashCode = XXH64_Avalanche(HashCode)
            RETURN
        END IF
        HashCode = IEOR(IEOR(Seed, PackLong(BaseSecret, 56)), PackLong(BaseSecret, 64))
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
                    DataVal0 = PackLong(Input, OffStripe + 8*J)
                    DataVal1 = PackLong(Input, OffStripe + 8*(J+1))
                    DataKey0 = IEOR(DataVal0, PackLong(CSInput, OffSec + 8*J))
                    DataKey1 = IEOR(DataVal1, PackLong(CSInput, OffSec + 8*(J+1)))
                    ! swap adjacent lanes
                    Acc(J)   = Acc(J)   + DataVal1 + IAND(MaxU32, DataKey0)*SHIFTR(DataKey0, 32)
                    Acc(J+1) = Acc(J+1) + DataVal0 + IAND(MaxU32, DataKey1)*SHIFTR(DataKey1, 32)
                END DO
            END DO
            ! XXH3_scrambleAcc_scalar
            OffSec = 192 - 64
            DO J = 0, 7
                Acc(J) = (IEOR(IEOR(Acc(J), SHIFTR(Acc(J), 47)), &
                              PackLong(CSInput, OffSec + 8*J)))*XXH_PRIME32_1
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
                DataVal0 = PackLong(Input, OffStripe + 8*J)
                DataVal1 = PackLong(Input, OffStripe + 8*(J+1))
                DataKey0 = IEOR(DataVal0, PackLong(CSInput, OffSec + 8*J))
                DataKey1 = IEOR(DataVal1, PackLong(CSInput, OffSec + 8*(J+1)))
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
            DataVal0 = PackLong(Input, OffStripe + 8*J)
            DataVal1 = PackLong(Input, OffStripe + 8*(J+1))
            DataKey0 = IEOR(DataVal0, PackLong(CSInput, OffSec + 8*J))
            DataKey1 = IEOR(DataVal1, PackLong(CSInput, OffSec + 8*(J+1)))
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
#undef GetI8
#undef GetU8
#undef Pack_U32

    RETURN

END FUNCTION XX3_Hash64

!******************************************************************************

FUNCTION XX3_Hash64_08(Input, Seed, PackInteger, BaseSecret, CustomSecret) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the XX3Hash64 hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, TARGET,   INTENT(IN) :: Input(0:)            ! input bytes
    tLong,           INTENT(IN) :: Seed                 ! seed
    PROCEDURE(Pack_I32)         :: PackInteger          ! procedure to convert a byte array to 32-bit integer
    tByte,           INTENT(IN) :: BaseSecret(0:191)    ! base secret
    tByte, OPTIONAL, INTENT(IN) :: CustomSecret(0:191)  ! custom secret
    TARGET                      :: BaseSecret, CustomSecret
    tLong                       :: HashCode             ! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: CSInput(:) => NULL()
    tLong, POINTER  :: InpVal(:)  => NULL()
    tLong, POINTER  :: InpVal2(:) => NULL()
    tLong, POINTER  :: SecVal(:)  => NULL()
    tIndex          :: Length, Offset
    tLong           :: BitFlip1, BitFlip2, Input_Lo, Input_Hi, Acc
    tLong           :: S, Input1, Input2, BitFlip, Keyed
    tInteger        :: C1, C2, C3
    tLong           :: Combined
    tIndex          :: NbRounds, I
    TYPE(C_PTR)     :: CPTR

!** FLOW

! define macros for better performance than using internal procedures
#define XXH3_Mix16B(Seed,Input,OffIn,Secret,OffSec) UnsignedLongMultiplyorFold(IEOR(Input(OffIn), Secret(OffSec) + Seed), IEOR(Input(OffIn+1), Secret(OffSec+1) - Seed))
#define XXH3_Mix2Accs(Acc_LH,Acc_RH,Secret,OffSec)  UnsignedLongMultiplyorFold(IEOR(Acc_LH, Secret(OffSec)), IEOR(Acc_RH, Secret(OffSec+1)))
#define Pack2I64Array(Bytes,Offset,Array,Size)      \
    CPTR = C_LOC(Bytes(Offset)); \
    CALL C_F_POINTER(CPTR, Array, SHAPE=[Size])
#define UnsignedInt(I)      IAND(ToLong(I), ToLong(Z'00000000FFFFFFFF'))

    ! initialize
    Length = SIZE(Input)
    Offset = 0

    ! perform hashing
    IF (Length <= 16) THEN
        ! XXH3_Len_0to16_64b
        IF (Length > 8) THEN
            ! XXH3_Len_9to16_64b
            Pack2I64Array(BaseSecret, 24, SecVal, 4)
            BitFlip1 = IEOR(SecVal(1), SecVal(2)) + Seed
            BitFlip2 = IEOR(SecVal(3), SecVal(4)) - Seed
            Input_Lo = IEOR(Pack_I64(Input, Offset), BitFlip1)
            Input_Hi = IEOR(Pack_I64(Input, Offset + Length - 8), BitFlip2)
            Acc = ToLong(Length) + LongReverseBytes(Input_Lo) + Input_Hi + &
                    UnsignedLongMultiplyorFold(Input_Lo, Input_Hi)
            HashCode = XXH3_Avalanche(Acc)
            RETURN
        END IF
        IF (Length >= 4) THEN
            ! XXH3_Len_4to8_64b
            S = IEOR(Seed, LongReverseBytes(IAND(Seed, MaxU32)))
            Input1 = ToLong(PackInteger(Input, Offset))    ! high int will be shifted
            Input2 = MaskI32(PackInteger(Input, Offset + Length - 4))
            Pack2I64Array(BaseSecret, 8, SecVal, 2)
            BitFlip = IEOR(SecVal(1), SecVal(2)) - S
            Keyed = IEOR((Input2 + SHIFTL(Input1, 32)), BitFlip)
            HashCode = XXH3_RRMxMx(Keyed, Length)
            RETURN
        END IF
        IF (Length /= 0) THEN
            ! XXH3_Len_1to3_64b
            C1 = MaskI8(Input(Offset))
            C2 = ToInteger(Input(Offset + SHIFTA(Length, 1)))   ! high 3 bytes will be shifted
            C3 = MaskI8(Input(Offset + Length - 1))
            Combined = UnsignedInt(IOR(IOR(IOR(SHIFTL(C1, 16), SHIFTL(C2, 24)), C3), \
                                    SHIFTL(Length, 8)))
            BitFlip = UnsignedInt(IEOR(PackInteger(BaseSecret, 0), PackInteger(BaseSecret, 4))) + Seed
            HashCode = IEOR(Combined, BitFlip)
            HashCode = XXH64_Avalanche(HashCode)
            RETURN
        END IF
        Pack2I64Array(BaseSecret, 56, SecVal, 2)
        HashCode = IEOR(IEOR(Seed, SecVal(1)), SecVal(2))
        HashCode = XXH64_Avalanche(HashCode)
        RETURN
    END IF
    IF (Length <= 128) THEN
        ! XXH3_Len_17to128_64b
        Acc = ToLong(Length)*XXH_PRIME64_1
        IF (Length > 32) THEN
            IF (Length > 64) THEN
                IF (Length > 96) THEN
                    Pack2I64Array(Input, OffSet,           InpVal,  8)
                    Pack2I64Array(Input, Offset+Length-64, InpVal2, 8)
                    Pack2I64Array(BaseSecret, 0,           SecVal,  16)
                    Acc = Acc + XXH3_Mix16B(Seed, InpVal, 7, SecVal, 13)
                    Acc = Acc + XXH3_Mix16B(Seed, InpVal2, SIZE(InpVal2)-7, SecVal, 15)
                ELSE
                    Pack2I64Array(Input, OffSet,           InpVal,  6)
                    Pack2I64Array(Input, Offset+Length-48, InpVal2, 6)
                    Pack2I64Array(BaseSecret, 0,           SecVal,  12)
                END IF
                Acc = Acc + XXH3_Mix16B(Seed, InpVal, 5, SecVal, 9)
                Acc = Acc + XXH3_Mix16B(Seed, InpVal2, SIZE(InpVal2)-5, SecVal, 11)
            ELSE
                Pack2I64Array(Input, OffSet,           InpVal,  4)
                Pack2I64Array(Input, Offset+Length-32, InpVal2, 4)
                Pack2I64Array(BaseSecret, 0,           SecVal,  8)
            END IF
            Acc = Acc + XXH3_Mix16B(Seed, InpVal, 3, SecVal, 5)
            Acc = Acc + XXH3_Mix16B(Seed, InpVal2, SIZE(InpVal2)-3, SecVal, 7)
        ELSE
            Pack2I64Array(Input, OffSet,           InpVal,  2)
            Pack2I64Array(Input, Offset+Length-16, InpVal2, 2)
            Pack2I64Array(BaseSecret, 0,           SecVal,  4)
        END IF
        Acc = Acc + XXH3_Mix16B(Seed, InpVal, 1, SecVal, 1)
        Acc = Acc + XXH3_Mix16B(Seed, InpVal2, SIZE(InpVal2)-1, SecVal, 3)
        HashCode = XXH3_Avalanche(Acc)
        RETURN
    END IF
    IF (Length <= 240) THEN
        ! XXH3_Len_129to240_64b
        Acc = ToLong(Length)*XXH_PRIME64_1
        NbRounds = Length / 16
        I = 0
        Pack2I64Array(Input, OffSet, InpVal, (NbRounds-1)*2)
        Pack2I64Array(BaseSecret, 0, SecVal, 16)
        DO WHILE (I < 8)
            Acc = Acc + XXH3_Mix16B(Seed, InpVal, 2*I+1, SecVal, 2*I+1)
            I = I + 1
        END DO
        Acc = XXH3_Avalanche(Acc)
        Pack2I64Array(BaseSecret, 3, SecVal, NbRounds*2-16)
        DO WHILE (I < NbRounds)
            Acc = Acc + XXH3_Mix16B(Seed, InpVal, 2*I+1, SecVal, 2*(I-8)+1)
            I = I + 1
        END DO
        ! last bytes
        Pack2I64Array(Input, Offset+Length-16, InpVal, 2)
        Pack2I64Array(BaseSecret, 119,         SecVal, 2)
        Acc = Acc + XXH3_Mix16B(Seed, InpVal, 1, SecVal, 1)
        HashCode = XXH3_Avalanche(Acc)
        RETURN
    END IF

    BLOCK
        ! XXH3_hashLong_64b_internal
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
                Pack2I64Array(Input, OffStripe, InpVal, 8)
                Pack2I64Array(CSInput, OffSec,  SecVal, 8)
                DO J = 0, 7, 2
                    DataVal0 = InpVal(J+1)
                    DataVal1 = InpVal(J+2)
                    DataKey0 = IEOR(DataVal0, SecVal(J+1))
                    DataKey1 = IEOR(DataVal1, SecVal(J+2))
                    ! swap adjacent lanes
                    Acc(J)   = Acc(J)   + DataVal1 + IAND(MaxU32, DataKey0)*SHIFTR(DataKey0, 32)
                    Acc(J+1) = Acc(J+1) + DataVal0 + IAND(MaxU32, DataKey1)*SHIFTR(DataKey1, 32)
                END DO
            END DO
            ! XXH3_scrambleAcc_scalar
            OffSec = 192 - 64
            Pack2I64Array(CSInput, OffSec, SecVal, 8)
            DO J = 0, 7
                Acc(J) = (IEOR(IEOR(Acc(J), SHIFTR(Acc(J), 47)), SecVal(J+1)))*XXH_PRIME32_1
            END DO
        END DO

        ! last partial block
        NbStripes = ((Length - 1) - (Block_Len*Nb_Blocks)) / 64
        OffBlock = Offset + Block_Len*Nb_Blocks
        DO S = 0, NbStripes - 1
            ! XXH3_accumulate_512
            OffStripe = OffBlock + S*64
            OffSec = S*8
            Pack2I64Array(Input, OffStripe, InpVal, 8)
            Pack2I64Array(CSInput, OffSec,  SecVal, 8)
            DO J = 0, 7, 2
                DataVal0 = InpVal(J+1)
                DataVal1 = InpVal(J+2)
                DataKey0 = IEOR(DataVal0, SecVal(J+1))
                DataKey1 = IEOR(DataVal1, SecVal(J+2))
                ! swap adjacent lanes
                Acc(J)   = Acc(J)   + DataVal1 + IAND(MaxU32, DataKey0)*SHIFTR(DataKey0, 32)
                Acc(J+1) = Acc(J+1) + DataVal0 + IAND(MaxU32, DataKey1)*SHIFTR(DataKey1, 32)
            END DO
        END DO

        ! last stripe
        ! XXH3_accumulate_512
        OffStripe = Offset + Length - 64
        OffSec = 192 - 64 - 7
        Pack2I64Array(Input, OffStripe, InpVal, 8)
        Pack2I64Array(CSInput, OffSec,  SecVal, 8)
        DO J = 0, 7, 2
            DataVal0 = InpVal(J+1)
            DataVal1 = InpVal(J+2)
            DataKey0 = IEOR(DataVal0, SecVal(J+1))
            DataKey1 = IEOR(DataVal1, SecVal(J+2))
            ! swap adjacent lanes
            Acc(J)   = Acc(J)   + DataVal1 + IAND(MaxU32, DataKey0)*SHIFTR(DataKey0, 32)
            Acc(J+1) = Acc(J+1) + DataVal0 + IAND(MaxU32, DataKey1)*SHIFTR(DataKey1, 32)
        END DO

        ! XXH3_mergeAccs
        Pack2I64Array(CSInput, 11, SecVal, 8)
        HashCode = ToLong(Length)*XXH_PRIME64_1             &
                    + XXH3_Mix2Accs(Acc(0), Acc(1), SecVal, 1) &
                    + XXH3_Mix2Accs(Acc(2), Acc(3), SecVal, 3) &
                    + XXH3_Mix2Accs(Acc(4), Acc(5), SecVal, 5) &
                    + XXH3_Mix2Accs(Acc(6), Acc(7), SecVal, 7)

        HashCode = XXH3_Avalanche(HashCode)

    END BLOCK

    ! free pointers
    NULLIFY(CSInput)

#undef XXH3_Mix16B
#undef XXH3_Mix2Accs
#undef UnsignedInt
#undef Pack2I64Array

    RETURN

CONTAINS

    PURE FUNCTION Pack_I64(Buf, Off) RESULT(Res)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To pack the array 'Buf' at offset 'Off' into the 64-bit word 'Res',
        ! in little-endian convention (least significant byte first).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,  INTENT(IN)  :: Buf(0:)  ! buffer
        tIndex, INTENT(IN)  :: Off      ! offset
        tLong               :: Res      ! result

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW

#define MaskLong(X)     IAND(ToLong(X), Z'00000000000000FF')

        ! implementation algorithm #3 (comparable to #1)
#define Byte2Integer(Val, Off)      ToInteger(Val(Off))
#define UnsignedByte(Val, Off)      IAND(Byte2Integer(Val, Off), Z'000000FF')
#define UnsignedShort(Val, Off)     IOR(UnsignedByte(Val, Off), SHIFTL(UnsignedByte(Val, Off+1), 8))
#define SignedInteger(Val, Off)     IOR(UnsignedShort(Val, Off), SHIFTL(UnsignedShort(Val, Off+2), 16))
#define UnsignedInteger(Val, Off)   IAND(ToLong(SignedInteger(Val, Off)), Z'00000000FFFFFFFF')
        Res = IOR(UnsignedInteger(Buf, Off), SHIFTL(UnsignedInteger(Buf, Off+4), 32))
#undef Byte2Integer
#undef UnsignedByte
#undef UnsignedShort
#undef SignedInteger
#undef UnsignedInteger

#undef MaskLong

        RETURN

    END FUNCTION Pack_I64

    !**************************************************************************

END FUNCTION XX3_Hash64_08

!******************************************************************************

FUNCTION XX3_Hash64_09(Input, Seed, BaseSecret, CustomSecret) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the XX3Hash64 hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, TARGET,   INTENT(IN) :: Input(0:)            ! input bytes
    tLong,           INTENT(IN) :: Seed                 ! seed
    tByte,           INTENT(IN) :: BaseSecret(0:191)    ! base secret
    tByte, OPTIONAL, INTENT(IN) :: CustomSecret(0:191)  ! custom secret
    TARGET                      :: BaseSecret, CustomSecret
    tLong                       :: HashCode             ! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: SecInput(:) => NULL()
    tLong, POINTER  :: InpVal(:)  => NULL()
    tLong, POINTER  :: InpVal2(:) => NULL()
    tIndex          :: Length, Offset
    tLong           :: BitFlip1, BitFlip2, Input_Lo, Input_Hi, Acc
    tLong           :: S, Input1, Input2, BitFlip, Keyed
    tInteger        :: C1, C2, C3
    tLong           :: Combined
    tIndex          :: NbRounds, I
    tByte           :: SecBytes(0:127)
    tLong           :: SecLongs(1:16)
    EQUIVALENCE(SecBytes, SecLongs)
    tByte           :: Bytes(0:7)
    tInteger        :: I32Val(2)
    EQUIVALENCE(Bytes, I32Val)
    TYPE(C_PTR)     :: CPTR

!** FLOW

! define macros for better performance than using internal procedures
#define XXH3_Mix16B(Seed,Input,OffIn,Secret,OffSec) UnsignedLongMultiplyorFold(IEOR(Input(OffIn), Secret(OffSec) + Seed), IEOR(Input(OffIn+1), Secret(OffSec+1) - Seed))
#define XXH3_Mix2Accs(Acc_LH,Acc_RH,Secret,OffSec)  UnsignedLongMultiplyorFold(IEOR(Acc_LH, Secret(OffSec)), IEOR(Acc_RH, Secret(OffSec+1)))
#define Pack2I64Array(Bytes,Offset,Array,Size)      \
    CPTR = C_LOC(Bytes(Offset)); \
    CALL C_F_POINTER(CPTR, Array, SHAPE=[Size])
#define UnsignedInt(I)      IAND(ToLong(I), ToLong(Z'00000000FFFFFFFF'))

    ! initialize
    Length = SIZE(Input)
    Offset = 0

    ! perform hashing
    IF (Length <= 16) THEN
        ! XXH3_Len_0to16_64b
        IF (Length > 8) THEN
            ! XXH3_Len_9to16_64b
            SecBytes(0:31) = BaseSecret(24:55)
            BitFlip1 = IEOR(SecLongs(1), SecLongs(2)) + Seed
            BitFlip2 = IEOR(SecLongs(3), SecLongs(4)) - Seed
            Input_Lo = IEOR(Pack_I64(Input, Offset), BitFlip1)
            Input_Hi = IEOR(Pack_I64(Input, Offset + Length - 8), BitFlip2)
            Acc = ToLong(Length) + LongReverseBytes(Input_Lo) + Input_Hi + &
                    UnsignedLongMultiplyorFold(Input_Lo, Input_Hi)
            HashCode = XXH3_Avalanche(Acc)
            RETURN
        END IF
        IF (Length >= 4) THEN
            ! XXH3_Len_4to8_64b
            S = IEOR(Seed, LongReverseBytes(IAND(Seed, MaxU32)))
            Bytes(0:3) = Input(Offset:Offset+3)
            Bytes(4:7) = Input(Offset+Length-4:Offset+Length-1)
            Input1 = ToLong(I32Val(1))    ! high int will be shifted
            Input2 = MaskI32(I32Val(2))
            SecBytes(0:15) = BaseSecret(8:23)
            BitFlip = IEOR(SecLongs(1), SecLongs(2)) - S
            Keyed = IEOR((Input2 + SHIFTL(Input1, 32)), BitFlip)
            HashCode = XXH3_RRMxMx(Keyed, Length)
            RETURN
        END IF
        IF (Length /= 0) THEN
            ! XXH3_Len_1to3_64b
            C1 = MaskI8(Input(Offset))
            C2 = ToInteger(Input(Offset + SHIFTA(Length, 1)))   ! high 3 bytes will be shifted
            C3 = MaskI8(Input(Offset + Length - 1))
            Combined = UnsignedInt(IOR(IOR(IOR(SHIFTL(C1, 16), SHIFTL(C2, 24)), C3), \
                                    SHIFTL(Length, 8)))
            Bytes(0:7) = BaseSecret(0:7)
            BitFlip = UnsignedInt(IEOR(I32Val(1), I32Val(2))) + Seed
            HashCode = IEOR(Combined, BitFlip)
            HashCode = XXH64_Avalanche(HashCode)
            RETURN
        END IF
        SecBytes(0:15) = BaseSecret(56:71)
        HashCode = IEOR(IEOR(Seed, SecLongs(1)), SecLongs(2))
        HashCode = XXH64_Avalanche(HashCode)
        RETURN
    END IF
    IF (Length <= 128) THEN
        ! XXH3_Len_17to128_64b
        Acc = ToLong(Length)*XXH_PRIME64_1
        IF (Length > 32) THEN
            IF (Length > 64) THEN
                IF (Length > 96) THEN
                    Pack2I64Array(Input, OffSet,           InpVal,  8)
                    Pack2I64Array(Input, Offset+Length-64, InpVal2, 8)
                    SecBytes(0:127) = BaseSecret(0:127)
                    Acc = Acc + XXH3_Mix16B(Seed, InpVal, 7, SecLongs, 13)
                    Acc = Acc + XXH3_Mix16B(Seed, InpVal2, SIZE(InpVal2)-7, SecLongs, 15)
                ELSE
                    Pack2I64Array(Input, OffSet,           InpVal,  6)
                    Pack2I64Array(Input, Offset+Length-48, InpVal2, 6)
                    SecBytes(0:95) = BaseSecret(0:95)
                END IF
                Acc = Acc + XXH3_Mix16B(Seed, InpVal, 5, SecLongs, 9)
                Acc = Acc + XXH3_Mix16B(Seed, InpVal2, SIZE(InpVal2)-5, SecLongs, 11)
            ELSE
                Pack2I64Array(Input, OffSet,           InpVal,  4)
                Pack2I64Array(Input, Offset+Length-32, InpVal2, 4)
                SecBytes(0:63) = BaseSecret(0:63)
            END IF
            Acc = Acc + XXH3_Mix16B(Seed, InpVal, 3, SecLongs, 5)
            Acc = Acc + XXH3_Mix16B(Seed, InpVal2, SIZE(InpVal2)-3, SecLongs, 7)
        ELSE
            Pack2I64Array(Input, OffSet,           InpVal,  2)
            Pack2I64Array(Input, Offset+Length-16, InpVal2, 2)
            SecBytes(0:31) = BaseSecret(0:31)
        END IF
        Acc = Acc + XXH3_Mix16B(Seed, InpVal, 1, SecLongs, 1)
        Acc = Acc + XXH3_Mix16B(Seed, InpVal2, SIZE(InpVal2)-1, SecLongs, 3)
        HashCode = XXH3_Avalanche(Acc)
        RETURN
    END IF
    IF (Length <= 240) THEN
        ! XXH3_Len_129to240_64b
        Acc = ToLong(Length)*XXH_PRIME64_1
        NbRounds = Length / 16
        I = 0
        Pack2I64Array(Input, OffSet, InpVal, (NbRounds-1)*2)
        SecBytes(0:127) = BaseSecret(0:127)
        DO WHILE (I < 8)
            Acc = Acc + XXH3_Mix16B(Seed, InpVal, 2*I+1, SecLongs, 2*I+1)
            I = I + 1
        END DO
        Acc = XXH3_Avalanche(Acc)
        SecBytes(0:(NbRounds*2-16)*8-1) = BaseSecret(3:3+(NbRounds*2-16)*8-1)
        DO WHILE (I < NbRounds)
            Acc = Acc + XXH3_Mix16B(Seed, InpVal, 2*I+1, SecLongs, 2*(I-8)+1)
            I = I + 1
        END DO
        ! last bytes
        Pack2I64Array(Input, Offset+Length-16, InpVal, 2)
        SecBytes(0:15) = BaseSecret(119:134)
        Acc = Acc + XXH3_Mix16B(Seed, InpVal, 1, SecLongs, 1)
        HashCode = XXH3_Avalanche(Acc)
        RETURN
    END IF

    BLOCK
        ! XXH3_hashLong_64b_internal
        tLong   :: Acc(0:7)
        tIndex  :: Nb_Blocks, OffBlock, OffStripe, OffSec, NbStripes
        tIndex  :: N, S, J
        tLong   :: DataVal0, DataVal1, DataKey0, DataKey1
        ! initialize
        IF (PRESENT(CustomSecret)) THEN
            SecInput => CustomSecret
        ELSE
            SecInput => BaseSecret
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
                Pack2I64Array(Input, OffStripe, InpVal, 8)
                SecBytes(0:63) = SecInput(OffSec:OffSec+63)
                DO J = 0, 7, 2
                    DataVal0 = InpVal(J+1)
                    DataVal1 = InpVal(J+2)
                    DataKey0 = IEOR(DataVal0, SecLongs(J+1))
                    DataKey1 = IEOR(DataVal1, SecLongs(J+2))
                    ! swap adjacent lanes
                    Acc(J)   = Acc(J)   + DataVal1 + IAND(MaxU32, DataKey0)*SHIFTR(DataKey0, 32)
                    Acc(J+1) = Acc(J+1) + DataVal0 + IAND(MaxU32, DataKey1)*SHIFTR(DataKey1, 32)
                END DO
            END DO
            ! XXH3_scrambleAcc_scalar
            OffSec = 192 - 64
            SecBytes(0:63) = SecInput(OffSec:OffSec+63)
            DO J = 0, 7
                Acc(J) = (IEOR(IEOR(Acc(J), SHIFTR(Acc(J), 47)), SecLongs(J+1)))*XXH_PRIME32_1
            END DO
        END DO

        ! last partial block
        NbStripes = ((Length - 1) - (Block_Len*Nb_Blocks)) / 64
        OffBlock = Offset + Block_Len*Nb_Blocks
        DO S = 0, NbStripes - 1
            ! XXH3_accumulate_512
            OffStripe = OffBlock + S*64
            OffSec = S*8
            Pack2I64Array(Input, OffStripe, InpVal, 8)
            SecBytes(0:63) = SecInput(OffSec:OffSec+63)
            DO J = 0, 7, 2
                DataVal0 = InpVal(J+1)
                DataVal1 = InpVal(J+2)
                DataKey0 = IEOR(DataVal0, SecLongs(J+1))
                DataKey1 = IEOR(DataVal1, SecLongs(J+2))
                ! swap adjacent lanes
                Acc(J)   = Acc(J)   + DataVal1 + IAND(MaxU32, DataKey0)*SHIFTR(DataKey0, 32)
                Acc(J+1) = Acc(J+1) + DataVal0 + IAND(MaxU32, DataKey1)*SHIFTR(DataKey1, 32)
            END DO
        END DO

        ! last stripe
        ! XXH3_accumulate_512
        OffStripe = Offset + Length - 64
        OffSec = 192 - 64 - 7
        Pack2I64Array(Input, OffStripe, InpVal, 8)
        SecBytes(0:63) = SecInput(OffSec:OffSec+63)
        DO J = 0, 7, 2
            DataVal0 = InpVal(J+1)
            DataVal1 = InpVal(J+2)
            DataKey0 = IEOR(DataVal0, SecLongs(J+1))
            DataKey1 = IEOR(DataVal1, SecLongs(J+2))
            ! swap adjacent lanes
            Acc(J)   = Acc(J)   + DataVal1 + IAND(MaxU32, DataKey0)*SHIFTR(DataKey0, 32)
            Acc(J+1) = Acc(J+1) + DataVal0 + IAND(MaxU32, DataKey1)*SHIFTR(DataKey1, 32)
        END DO

        ! XXH3_mergeAccs
        SecBytes(0:63) = SecInput(11:74)
        HashCode = ToLong(Length)*XXH_PRIME64_1             &
                    + XXH3_Mix2Accs(Acc(0), Acc(1), SecLongs, 1) &
                    + XXH3_Mix2Accs(Acc(2), Acc(3), SecLongs, 3) &
                    + XXH3_Mix2Accs(Acc(4), Acc(5), SecLongs, 5) &
                    + XXH3_Mix2Accs(Acc(6), Acc(7), SecLongs, 7)

        HashCode = XXH3_Avalanche(HashCode)

    END BLOCK

    ! free pointers
    NULLIFY(SecInput)

#undef XXH3_Mix16B
#undef XXH3_Mix2Accs
#undef UnsignedInt
#undef Pack2I64Array

    RETURN

CONTAINS

    PURE FUNCTION Pack_I64(Buf, Off) RESULT(Res)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To pack the array 'Buf' at offset 'Off' into the 64-bit word 'Res',
        ! in little-endian convention (least significant byte first).

        IMPLICIT NONE

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,  INTENT(IN)  :: Buf(0:)  ! buffer
        tIndex, INTENT(IN)  :: Off      ! offset
        tLong               :: Res      ! result

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW

#define MaskLong(X)     IAND(ToLong(X), Z'00000000000000FF')

        ! implementation algorithm #3 (comparable to #1)
#define Byte2Integer(Val, Off)      ToInteger(Val(Off))
#define UnsignedByte(Val, Off)      IAND(Byte2Integer(Val, Off), Z'000000FF')
#define UnsignedShort(Val, Off)     IOR(UnsignedByte(Val, Off), SHIFTL(UnsignedByte(Val, Off+1), 8))
#define SignedInteger(Val, Off)     IOR(UnsignedShort(Val, Off), SHIFTL(UnsignedShort(Val, Off+2), 16))
#define UnsignedInteger(Val, Off)   IAND(ToLong(SignedInteger(Val, Off)), Z'00000000FFFFFFFF')
        Res = IOR(UnsignedInteger(Buf, Off), SHIFTL(UnsignedInteger(Buf, Off+4), 32))
#undef Byte2Integer
#undef UnsignedByte
#undef UnsignedShort
#undef SignedInteger
#undef UnsignedInteger

#undef MaskLong

        RETURN

    END FUNCTION Pack_I64

    !**************************************************************************

END FUNCTION XX3_Hash64_09

!******************************************************************************

FUNCTION XX3_Hash64_10(Input, Seed, BaseSecret, CustomSecret) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the XX3Hash64 hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,           INTENT(IN) :: Input(0:)            ! input bytes
    tLong,           INTENT(IN) :: Seed                 ! seed
    tByte,           INTENT(IN) :: BaseSecret(0:191)    ! base secret
    tByte, OPTIONAL, INTENT(IN) :: CustomSecret(0:191)  ! custom secret
    TARGET                      :: BaseSecret, CustomSecret
    tLong                       :: HashCode             ! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: SecInput(:) => NULL()
    tIndex          :: Length, Offset
    tLong           :: BitFlip1, BitFlip2, Input_Lo, Input_Hi, Acc
    tLong           :: S, Input1, Input2, BitFlip, Keyed
    tInteger        :: C1, C2, C3
    tLong           :: Combined
    tIndex          :: NbRounds, I
    ! variables used to quickly access input and secret bytes
    tByte           :: InpBytes(0:127)
    tLong           :: InpLongs(1:16)
    EQUIVALENCE(InpBytes, InpLongs)
    tByte           :: SecBytes(0:127)
    tLong           :: SecLongs(1:16)
    EQUIVALENCE(SecBytes, SecLongs)
    tByte           :: Bytes(0:7)
    tInteger        :: I32Val(2)
    EQUIVALENCE(Bytes, I32Val)

!** FLOW

! define macros for better performance than using internal procedures
#define XXH3_Mix16B(Seed,Input,OffIn,Secret,OffSec) UnsignedLongMultiplyorFold(IEOR(Input(OffIn), Secret(OffSec) + Seed), IEOR(Input(OffIn+1), Secret(OffSec+1) - Seed))
#define XXH3_Mix2Accs(Acc_LH,Acc_RH,Secret,OffSec)  UnsignedLongMultiplyorFold(IEOR(Acc_LH, Secret(OffSec)), IEOR(Acc_RH, Secret(OffSec+1)))
#define UnsignedInt(I)      IAND(ToLong(I), ToLong(Z'00000000FFFFFFFF'))

    ! initialize
    Length = SIZE(Input)
    Offset = 0

    ! perform hashing
    IF (Length <= 16) THEN
        ! XXH3_Len_0to16_64b
        IF (Length > 8) THEN
            ! XXH3_Len_9to16_64b
            SecBytes(0:31) = BaseSecret(24:55)
            BitFlip1 = IEOR(SecLongs(1), SecLongs(2)) + Seed
            BitFlip2 = IEOR(SecLongs(3), SecLongs(4)) - Seed
            InpBytes(0:7) = Input(Offset:Offset+7)
            InpBytes(8:15) = Input(Offset+Length-8:Offset+Length-1)
            Input_Lo = IEOR(InpLongs(1), BitFlip1)
            Input_Hi = IEOR(InpLongs(2), BitFlip2)
            Acc = ToLong(Length) + LongReverseBytes(Input_Lo) + Input_Hi + &
                    UnsignedLongMultiplyorFold(Input_Lo, Input_Hi)
            HashCode = XXH3_Avalanche(Acc)
            RETURN
        END IF
        IF (Length >= 4) THEN
            ! XXH3_Len_4to8_64b
            S = IEOR(Seed, LongReverseBytes(IAND(Seed, MaxU32)))
            Bytes(0:3) = Input(Offset:Offset+3)
            Bytes(4:7) = Input(Offset+Length-4:Offset+Length-1)
            Input1 = ToLong(I32Val(1))      ! high int will be shifted
            Input2 = MaskI32(I32Val(2))
            SecBytes(0:15) = BaseSecret(8:23)
            BitFlip = IEOR(SecLongs(1), SecLongs(2)) - S
            Keyed = IEOR((Input2 + SHIFTL(Input1, 32)), BitFlip)
            HashCode = XXH3_RRMxMx(Keyed, Length)
            RETURN
        END IF
        IF (Length /= 0) THEN
            ! XXH3_Len_1to3_64b
            C1 = MaskI8(Input(Offset))
            C2 = ToInteger(Input(Offset + SHIFTA(Length, 1)))   ! high 3 bytes will be shifted
            C3 = MaskI8(Input(Offset + Length - 1))
            Combined = UnsignedInt(IOR(IOR(IOR(SHIFTL(C1, 16), SHIFTL(C2, 24)), C3), \
                                    SHIFTL(Length, 8)))
            Bytes(0:7) = BaseSecret(0:7)
            BitFlip = UnsignedInt(IEOR(I32Val(1), I32Val(2))) + Seed
            HashCode = IEOR(Combined, BitFlip)
            HashCode = XXH64_Avalanche(HashCode)
            RETURN
        END IF
        SecBytes(0:15) = BaseSecret(56:71)
        HashCode = IEOR(IEOR(Seed, SecLongs(1)), SecLongs(2))
        HashCode = XXH64_Avalanche(HashCode)
        RETURN
    END IF
    IF (Length <= 128) THEN
        ! XXH3_Len_17to128_64b
        Acc = ToLong(Length)*XXH_PRIME64_1
        IF (Length > 32) THEN
            IF (Length > 64) THEN
                IF (Length > 96) THEN
                    InpBytes(0:63)   = Input(Offset:Offset+63)
                    InpBytes(64:127) = Input(Offset+Length-64:Offset+Length-1)
                    SecBytes(0:127)  = BaseSecret(0:127)
                    Acc = Acc + XXH3_Mix16B(Seed, InpLongs, 7, SecLongs, 13)
                    Acc = Acc + XXH3_Mix16B(Seed, InpLongs, 9, SecLongs, 15)
                ELSE
                    InpBytes(0:47)   = Input(Offset:Offset+47)
                    InpBytes(80:127) = Input(Offset+Length-48:Offset+Length-1)
                    SecBytes(0:95)   = BaseSecret(0:95)
                END IF
                Acc = Acc + XXH3_Mix16B(Seed, InpLongs,  5, SecLongs, 9)
                Acc = Acc + XXH3_Mix16B(Seed, InpLongs, 11, SecLongs, 11)
            ELSE
                InpBytes(0:31)   = Input(Offset:Offset+31)
                InpBytes(96:127) = Input(Offset+Length-32:Offset+Length-1)
                SecBytes(0:63)   = BaseSecret(0:63)
            END IF
            Acc = Acc + XXH3_Mix16B(Seed, InpLongs,  3, SecLongs, 5)
            Acc = Acc + XXH3_Mix16B(Seed, InpLongs, 13, SecLongs, 7)
        ELSE
            InpBytes(0:15)    = Input(Offset:Offset+15)
            InpBytes(112:127) = Input(Offset+Length-16:Offset+Length-1)
            SecBytes(0:31)    = BaseSecret(0:31)
        END IF
        Acc = Acc + XXH3_Mix16B(Seed, InpLongs,  1, SecLongs, 1)
        Acc = Acc + XXH3_Mix16B(Seed, InpLongs, 15, SecLongs, 3)
        HashCode = XXH3_Avalanche(Acc)
        RETURN
    END IF
    IF (Length <= 240) THEN
        ! XXH3_Len_129to240_64b
        Acc = ToLong(Length)*XXH_PRIME64_1
        NbRounds = Length / 16
        I = 0
        InpBytes(0:127) = Input(Offset:Offset+127)
        SecBytes(0:127) = BaseSecret(0:127)
        DO WHILE (I < 8)
            Acc = Acc + XXH3_Mix16B(Seed, InpLongs, 2*I+1, SecLongs, 2*I+1)
            I = I + 1
        END DO
        Acc = XXH3_Avalanche(Acc)
        Offset = Offset + 128
        InpBytes(0:(NbRounds*2-16)*8-1) = Input(Offset:Offset+(NbRounds*2-16)*8-1)
        SecBytes(0:(NbRounds*2-16)*8-1) = BaseSecret(3:3+(NbRounds*2-16)*8-1)
        DO WHILE (I < NbRounds)
            Acc = Acc + XXH3_Mix16B(Seed, InpLongs, 2*(I-8)+1, SecLongs, 2*(I-8)+1)
            I = I + 1
        END DO
        ! last bytes
        Offset = Offset - 128
        InpBytes(0:15) = Input(Offset+Length-16:Length-1)
        SecBytes(0:15) = BaseSecret(119:134)
        Acc = Acc + XXH3_Mix16B(Seed, InpLongs, 1, SecLongs, 1)
        HashCode = XXH3_Avalanche(Acc)
        RETURN
    END IF

    BLOCK
        ! XXH3_hashLong_64b_internal
        tLong   :: Acc(0:7)
        tIndex  :: Nb_Blocks, OffBlock, OffStripe, OffSec, NbStripes
        tIndex  :: N, S, J
        tLong   :: DataVal0, DataVal1, DataKey0, DataKey1
        ! initialize
        IF (PRESENT(CustomSecret)) THEN
            SecInput => CustomSecret
        ELSE
            SecInput => BaseSecret
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
                InpBytes(0:63) = Input(OffStripe:OffStripe+63)
                SecBytes(0:63) = SecInput(OffSec:OffSec+63)
                DO J = 0, 7, 2
                    DataVal0 = InpLongs(J+1)
                    DataVal1 = InpLongs(J+2)
                    DataKey0 = IEOR(DataVal0, SecLongs(J+1))
                    DataKey1 = IEOR(DataVal1, SecLongs(J+2))
                    ! swap adjacent lanes
                    Acc(J)   = Acc(J)   + DataVal1 + IAND(MaxU32, DataKey0)*SHIFTR(DataKey0, 32)
                    Acc(J+1) = Acc(J+1) + DataVal0 + IAND(MaxU32, DataKey1)*SHIFTR(DataKey1, 32)
                END DO
            END DO
            ! XXH3_scrambleAcc_scalar
            OffSec = 192 - 64
            SecBytes(0:63) = SecInput(OffSec:OffSec+63)
            DO J = 0, 7
                Acc(J) = (IEOR(IEOR(Acc(J), SHIFTR(Acc(J), 47)), SecLongs(J+1)))*XXH_PRIME32_1
            END DO
        END DO

        ! last partial block
        NbStripes = ((Length - 1) - (Block_Len*Nb_Blocks)) / 64
        OffBlock = Offset + Block_Len*Nb_Blocks
        DO S = 0, NbStripes - 1
            ! XXH3_accumulate_512
            OffStripe = OffBlock + S*64
            OffSec = S*8
            InpBytes(0:63) = Input(OffStripe:OffStripe+63)
            SecBytes(0:63) = SecInput(OffSec:OffSec+63)
            DO J = 0, 7, 2
                DataVal0 = InpLongs(J+1)
                DataVal1 = InpLongs(J+2)
                DataKey0 = IEOR(DataVal0, SecLongs(J+1))
                DataKey1 = IEOR(DataVal1, SecLongs(J+2))
                ! swap adjacent lanes
                Acc(J)   = Acc(J)   + DataVal1 + IAND(MaxU32, DataKey0)*SHIFTR(DataKey0, 32)
                Acc(J+1) = Acc(J+1) + DataVal0 + IAND(MaxU32, DataKey1)*SHIFTR(DataKey1, 32)
            END DO
        END DO

        ! last stripe
        ! XXH3_accumulate_512
        OffStripe = Offset + Length - 64
        OffSec = 192 - 64 - 7
        InpBytes(0:63) = Input(OffStripe:OffStripe+63)
        SecBytes(0:63) = SecInput(OffSec:OffSec+63)
        DO J = 0, 7, 2
            DataVal0 = InpLongs(J+1)
            DataVal1 = InpLongs(J+2)
            DataKey0 = IEOR(DataVal0, SecLongs(J+1))
            DataKey1 = IEOR(DataVal1, SecLongs(J+2))
            ! swap adjacent lanes
            Acc(J)   = Acc(J)   + DataVal1 + IAND(MaxU32, DataKey0)*SHIFTR(DataKey0, 32)
            Acc(J+1) = Acc(J+1) + DataVal0 + IAND(MaxU32, DataKey1)*SHIFTR(DataKey1, 32)
        END DO

        ! XXH3_mergeAccs
        SecBytes(0:63) = SecInput(11:74)
        HashCode = ToLong(Length)*XXH_PRIME64_1             &
                    + XXH3_Mix2Accs(Acc(0), Acc(1), SecLongs, 1) &
                    + XXH3_Mix2Accs(Acc(2), Acc(3), SecLongs, 3) &
                    + XXH3_Mix2Accs(Acc(4), Acc(5), SecLongs, 5) &
                    + XXH3_Mix2Accs(Acc(6), Acc(7), SecLongs, 7)

        HashCode = XXH3_Avalanche(HashCode)

    END BLOCK

    ! free pointers
    NULLIFY(SecInput)

#undef XXH3_Mix16B
#undef XXH3_Mix2Accs
#undef UnsignedInt

    RETURN

END FUNCTION XX3_Hash64_10

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

END SUBMODULE SubBase_XX3Hash64_Exp

!******************************************************************************

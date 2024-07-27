
SUBMODULE (ModBase_ExperimentalHash64 : SubBase_XX3Hash64_Exp) SubBase_XX3Hash128_Exp

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an experimental implementation for the XX3 hash algorithms
!   for 128-bit integer output by Yann Collet. <br>

!** USE STATEMENTS:
    ! na

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
    ABSTRACT INTERFACE
        !----------------------------------------------------------------------
        FUNCTION PackInt64(ByteArr, Offset) RESULT(Res)
            !^ To convert an array of 8-bit integers starting at the offset to
            !  a 64-bit integer value.
            IMPORT
            tByte, TARGET, INTENT(IN)   :: ByteArr(0:)  !! byte array
            tIndex,        INTENT(IN)   :: Offset       !! offset
            tLong                       :: Res          !! result
        END FUNCTION PackInt64
        !----------------------------------------------------------------------
    END INTERFACE

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE FUNCTION XX3_Hash128_Exp(Input, InpSize, Algo, StartHash, RemoveSign, &
                                Secret, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the XX3Hash128 hash algorithm by Yann Collet.
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
    tLong,    OPTIONAL,     INTENT(OUT) :: HashPair(2)  !! dual (128-bit) hash codes
    tLong                               :: HashCode     !! single (64-bit) hash code

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
                HashCode = XX3_Hash128(InpPtr, Seed, Pack_I64_A1, Pack_I32_A1, XXH3_kSecret, Secret, HashPair)
            CASE (2)
                HashCode = XX3_Hash128(InpPtr, Seed, Pack_I64_A2, Pack_I32_A2, XXH3_kSecret, Secret, HashPair)
            CASE (3)
                HashCode = XX3_Hash128(InpPtr, Seed, Pack_I64_A3, Pack_I32_A3, XXH3_kSecret, Secret, HashPair)
            CASE (4)
                HashCode = XX3_Hash128(InpPtr, Seed, Pack_I64_A4, Pack_I32_A4, XXH3_kSecret, Secret, HashPair)
            CASE (5)
                HashCode = XX3_Hash128(InpPtr, Seed, Pack_I64_A5, Pack_I32_A5, XXH3_kSecret, Secret, HashPair)
            CASE (6)
                HashCode = XX3_Hash128(InpPtr, Seed, Pack_I64_A6, Pack_I32_A6, XXH3_kSecret, Secret, HashPair)
            CASE (7)
                HashCode = XX3_Hash128(InpPtr, Seed, Pack_I64_A7, Pack_I32_A7, XXH3_kSecret, Secret, HashPair)
            CASE (8)
                HashCode = XX3_Hash128_08(InpPtr, Seed, Pack_I32_A6, XXH3_kSecret, Secret, HashPair)
            CASE (9)
                HashCode = XX3_Hash128_09(InpPtr, Seed, XXH3_kSecret, Secret, HashPair)
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
                    HashCode = XX3_Hash128(InpPtr, Seed, Pack_I64_A1, Pack_I32_A1, XXH3_kSecret, CustomSecret, HashPair)
                CASE (2)
                    HashCode = XX3_Hash128(InpPtr, Seed, Pack_I64_A2, Pack_I32_A2, XXH3_kSecret, CustomSecret, HashPair)
                CASE (3)
                    HashCode = XX3_Hash128(InpPtr, Seed, Pack_I64_A3, Pack_I32_A3, XXH3_kSecret, CustomSecret, HashPair)
                CASE (4)
                    HashCode = XX3_Hash128(InpPtr, Seed, Pack_I64_A4, Pack_I32_A4, XXH3_kSecret, CustomSecret, HashPair)
                CASE (5)
                    HashCode = XX3_Hash128(InpPtr, Seed, Pack_I64_A5, Pack_I32_A5, XXH3_kSecret, CustomSecret, HashPair)
                CASE (6)
                    HashCode = XX3_Hash128(InpPtr, Seed, Pack_I64_A6, Pack_I32_A6, XXH3_kSecret, CustomSecret, HashPair)
                CASE (7)
                    HashCode = XX3_Hash128(InpPtr, Seed, Pack_I64_A7, Pack_I32_A7, XXH3_kSecret, CustomSecret, HashPair)
                CASE (8)
                    HashCode = XX3_Hash128_08(InpPtr, Seed, Pack_I32_A6, XXH3_kSecret, CustomSecret, HashPair)
                CASE (9)
                    HashCode = XX3_Hash128_09(InpPtr, Seed, XXH3_kSecret, CustomSecret, HashPair)
                END SELECT
            END BLOCK
        END IF
    ELSE
        SELECT CASE (Algo)
        CASE (1)
            HashCode = XX3_Hash128(InpPtr, Seed, Pack_I64_A1, Pack_I32_A1, XXH3_kSecret, HashPair=HashPair)
        CASE (2)
            HashCode = XX3_Hash128(InpPtr, Seed, Pack_I64_A2, Pack_I32_A2, XXH3_kSecret, HashPair=HashPair)
        CASE (3)
            HashCode = XX3_Hash128(InpPtr, Seed, Pack_I64_A3, Pack_I32_A3, XXH3_kSecret, HashPair=HashPair)
        CASE (4)
            HashCode = XX3_Hash128(InpPtr, Seed, Pack_I64_A4, Pack_I32_A4, XXH3_kSecret, HashPair=HashPair)
        CASE (5)
            HashCode = XX3_Hash128(InpPtr, Seed, Pack_I64_A5, Pack_I32_A5, XXH3_kSecret, HashPair=HashPair)
        CASE (6)
            HashCode = XX3_Hash128(InpPtr, Seed, Pack_I64_A6, Pack_I32_A6, XXH3_kSecret, HashPair=HashPair)
        CASE (7)
            HashCode = XX3_Hash128(InpPtr, Seed, Pack_I64_A7, Pack_I32_A7, XXH3_kSecret, HashPair=HashPair)
        CASE (8)
            HashCode = XX3_Hash128_08(InpPtr, Seed, Pack_I32_A6, XXH3_kSecret, HashPair=HashPair)
        CASE (9)
            HashCode = XX3_Hash128_09(InpPtr, Seed, XXH3_kSecret, HashPair=HashPair)
        END SELECT
    END IF

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION XX3_Hash128_Exp

!******************************************************************************

FUNCTION XX3_Hash128(Input, Seed, PackLong, PackInteger, BaseSecret, CustomSecret, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the XX3Hash128 hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,                   INTENT(IN)     :: Input(0:)            !! input bytes
    tLong,                   INTENT(IN)     :: Seed                 !! seed
    PROCEDURE(PackInt64)                    :: PackLong             !! procedure to convert a byte array to 64-bit integer
    PROCEDURE(Pack_I32)                     :: PackInteger          !! procedure to convert a byte array to 32-bit integer
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
UnsignedLongMultiplyorFold(IEOR(Acc_LH, PackLong(Secret, OffSec)), \
                           IEOR(Acc_RH, PackLong(Secret, OffSec+8)))
#define XXH128_Mix32B_Once(Seed,Secret,OffSec,Acc,Input0,Input1,Input2,Input3) \
IEOR(Acc + UnsignedLongMultiplyorFold(                                  \
     IEOR(Input0, (PackLong(Secret, OffSec) + Seed)),               \
     IEOR(Input1, (PackLong(Secret, OffSec+8) - Seed))), (Input2 + Input3))
#define UnsignedInt(I)      ToLong(IAND(I, Z'FFFFFFFF'))
#define GetI8(In, Off)      ToInteger(In(Off))
#define GetU8(In, Off)      MaskI8(In(Off))
#define Pack_U32(In, Off)   MaskI32(PackInteger(In, Off))

!** FLOW

    ! initialize
    Length = SIZE(Input)
    Offset = 0

    ! perform hashing
    IF (Length <= 16) THEN
        ! XXH3_len_0to16_128b
        IF (Length > 8) THEN
            ! XXH3_len_9to16_128b
            BitflipL = IEOR(PackLong(BaseSecret, 32), PackLong(BaseSecret, 40)) - Seed
            BitflipH = IEOR(PackLong(BaseSecret, 48), PackLong(BaseSecret, 56)) + Seed
            Input_Hi = PackLong(Input, Offset + Length - 8)
            Input_Lo = IEOR(IEOR(PackLong(Input, Offset), Input_Hi), BitflipL)
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
            Input_Lo = Pack_U32(Input, Offset)
            ! high int will be shifted
            Input_Hi = ToLong(PackInteger(Input, Offset + Length - 4))
            Bitflip = IEOR(PackLong(BaseSecret, 16), PackLong(BaseSecret, 24)) + S
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
            C1 = GetU8(Input, Offset)
            ! high 3 bytes will be shifted
            C2 = GetI8(Input, Offset + SHIFTA(Length, 1))
            C3 = GetU8(Input, Offset + Length - 1)
            CombinedL = IOR(IOR(IOR(SHIFTL(C1, 16), SHIFTL(C2, 24)), C3), SHIFTL(Length, 8))
            CombinedH = RotateLeft(IntegerReverseBytes(CombinedL), 13)
            BitFlipL = UnsignedInt(IEOR(PackInteger(BaseSecret, 0), PackInteger(BaseSecret, 4))) + Seed
            BitFlipH = UnsignedInt(IEOR(PackInteger(BaseSecret, 8), PackInteger(BaseSecret, 12))) - Seed
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
        Low = XXH64_Avalanche(IEOR(IEOR(Seed, PackLong(BaseSecret, 64)), &
                                              PackLong(BaseSecret, 72)))
        IF (PRESENT(HashPair)) THEN
            HashPair(1) = Low
            HashPair(2) = XXH64_Avalanche(IEOR(IEOR(Seed, PackLong(BaseSecret, 80)), &
                                                          PackLong(BaseSecret, 88)))
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
                    Input0 = PackLong(Input, Offset + 48)
                    Input1 = PackLong(Input, Offset + 56)
                    Input2 = PackLong(Input, Offset + Length - 64)
                    Input3 = PackLong(Input, Offset + Length - 56)
                    Acc0 = XXH128_Mix32B_Once(Seed, BaseSecret, 96_kIndex,  Acc0, \
                                              Input0, Input1, Input2, Input3)
                    Acc1 = XXH128_Mix32B_Once(Seed, BaseSecret, 112_kIndex, Acc1, \
                                              Input2, Input3, Input0, Input1)
                END IF
                Input0 = PackLong(Input, Offset + 32)
                Input1 = PackLong(Input, Offset + 40)
                Input2 = PackLong(Input, Offset + Length - 48)
                Input3 = PackLong(Input, Offset + Length - 40)
                Acc0 = XXH128_Mix32B_Once(Seed, BaseSecret, 64_kIndex, Acc0, \
                                          Input0, Input1, Input2, Input3)
                Acc1 = XXH128_Mix32B_Once(Seed, BaseSecret, 80_kIndex, Acc1,  \
                                          Input2, Input3, Input0, Input1)
            END IF
            Input0 = PackLong(Input, Offset + 16)
            Input1 = PackLong(Input, Offset + 24)
            Input2 = PackLong(Input, Offset + Length - 32)
            Input3 = PackLong(Input, Offset + Length - 24)
            Acc0 = XXH128_Mix32B_Once(Seed, BaseSecret, 32_kIndex, Acc0, \
                                      Input0, Input1, Input2, Input3)
            Acc1 = XXH128_Mix32B_Once(Seed, BaseSecret, 48_kIndex, Acc1, \
                                      Input2, Input3, Input0, Input1)
        END IF
        Input0 = PackLong(Input, Offset)
        Input1 = PackLong(Input, Offset + 8)
        Input2 = PackLong(Input, Offset + Length - 16)
        Input3 = PackLong(Input, Offset + Length - 8)
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
            Input0 = PackLong(Input, Offset + 32*I)
            Input1 = PackLong(Input, Offset + 32*I + 8)
            Input2 = PackLong(Input, Offset + 32*I + 16)
            Input3 = PackLong(Input, Offset + 32*I + 24)
            Acc0 = XXH128_Mix32B_Once(Seed, BaseSecret, 32*I,      Acc0, \
                                      Input0, Input1, Input2, Input3)
            Acc1 = XXH128_Mix32B_Once(Seed, BaseSecret, 32*I + 16, Acc1, \
                                      Input2, Input3, Input0, Input1)
            I = I + 1
        END DO
        Acc0 = XXH3_Avalanche(Acc0)
        Acc1 = XXH3_Avalanche(Acc1)
        DO WHILE (I < NbRounds)
            Input0 = PackLong(Input, Offset + 32*I)
            Input1 = PackLong(Input, Offset + 32*I + 8)
            Input2 = PackLong(Input, Offset + 32*I + 16)
            Input3 = PackLong(Input, Offset + 32*I + 24)
            Acc0 = XXH128_Mix32B_Once(Seed, BaseSecret, 3  + 32*(I-4), Acc0, \
                                      Input0, Input1, Input2, Input3)
            Acc1 = XXH128_Mix32B_Once(Seed, BaseSecret, 19 + 32*(I-4), Acc1, \
                                      Input2, Input3, Input0, Input1)
            I = I + 1
        END DO

        ! last bytes
        Input0 = PackLong(Input, Offset + Length - 16)
        Input1 = PackLong(Input, Offset + Length - 8)
        Input2 = PackLong(Input, Offset + Length - 32)
        Input3 = PackLong(Input, Offset + Length - 24)
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
#undef GetI8
#undef GetU8
#undef Pack_U32

    RETURN

END FUNCTION XX3_Hash128

!******************************************************************************

FUNCTION XX3_Hash128_08(Input, Seed, PackInteger, BaseSecret, CustomSecret, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the XX3Hash128 hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, TARGET,   INTENT(IN)     :: Input(0:)            ! input bytes
    tLong,           INTENT(IN)     :: Seed                 ! seed
    PROCEDURE(Pack_I32)             :: PackInteger          ! procedure to convert a byte array to 32-bit integer
    tByte,           INTENT(IN)     :: BaseSecret(0:191)    ! base secret
    tByte, OPTIONAL, INTENT(IN)     :: CustomSecret(0:191)  ! custom secret
    tLong, OPTIONAL, INTENT(OUT)    :: HashPair(2)          ! dual hash code
    TARGET                          :: BaseSecret, CustomSecret
    tLong                           :: HashCode             ! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: CSInput(:) => NULL()
    tLong, POINTER  :: InpVal(:)  => NULL()
    tLong, POINTER  :: InpVal2(:) => NULL()
    tLong, POINTER  :: SecVal(:)  => NULL()
    tIndex          :: Length, Offset
    tLong           :: Low, High
    tLong           :: BitFlipL, BitFlipH, Input_Lo, Input_Hi
    tLong           :: S, PL, Bitflip, Keyed, M128_Lo, M128_Hi
    tInteger        :: C1, C2, C3, CombinedL, CombinedH
    tLong           :: Acc0, Acc1, Input0, Input1, Input2, Input3
    tIndex          :: I
    tInteger        :: NbRounds
    TYPE(C_PTR)     :: CPTR

!** FLOW

! define macros for better performance than using internal procedures
#define XXH3_Mix2Accs(Acc_LH,Acc_RH,Secret,OffSec)  UnsignedLongMultiplyorFold(IEOR(Acc_LH, Secret(OffSec)), IEOR(Acc_RH, Secret(OffSec+1)))
#define XXH128_Mix32B_Once(Seed,Secret,OffSec,Acc,Input0,Input1,Input2,Input3)  IEOR(Acc + UnsignedLongMultiplyorFold(IEOR(Input0, (Secret(OffSec) + Seed)), IEOR(Input1, (Secret(OffSec+1) - Seed))), (Input2 + Input3))
#define Pack2I64Array(Bytes,Offset,Array,Size) \
    CPTR = C_LOC(Bytes(Offset)); \
    CALL C_F_POINTER(CPTR, Array, SHAPE=[Size])
#define UnsignedInt(I)      IAND(ToLong(I), ToLong(Z'00000000FFFFFFFF'))

    ! initialize
    Length = SIZE(Input)
    Offset = 0

    ! perform hashing
    IF (Length <= 16) THEN
        ! XXH3_len_0to16_128b
        IF (Length > 8) THEN
            ! XXH3_len_9to16_128b
            Pack2I64Array(BaseSecret, 32, SecVal, 4)
            BitflipL = IEOR(SecVal(1), SecVal(2)) - Seed
            BitflipH = IEOR(SecVal(3), SecVal(4)) + Seed
            Input_Hi = Pack_I64(Input, Offset + Length - 8)
            Input_Lo = IEOR(IEOR(Pack_I64(Input, Offset), Input_Hi), BitflipL)
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
            Input_Lo = MaskI32(PackInteger(Input, Offset))
            ! high int will be shifted
            Input_Hi = ToLong(PackInteger(Input, Offset + Length - 4))
            Pack2I64Array(BaseSecret, 16, SecVal, 2)
            Bitflip = IEOR(SecVal(1), SecVal(2)) + S
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
            C1 = MaskI8(Input(Offset))
            ! high 3 bytes will be shifted
            C2 = ToInteger(Input(Offset + SHIFTA(Length, 1)))
            C3 = MaskI8(Input(Offset + Length - 1))
            CombinedL = IOR(IOR(IOR(SHIFTL(C1, 16), SHIFTL(C2, 24)), C3), SHIFTL(Length, 8))
            CombinedH = RotateLeft(IntegerReverseBytes(CombinedL), 13)
            BitFlipL = UnsignedInt(IEOR(PackInteger(BaseSecret, 0), \
                                        PackInteger(BaseSecret, 4))) + Seed
            BitFlipH = UnsignedInt(IEOR(PackInteger(BaseSecret, 8), \
                                        PackInteger(BaseSecret, 12))) - Seed
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
        Pack2I64Array(BaseSecret, 64, SecVal, 2)
        Low = XXH64_Avalanche(IEOR(IEOR(Seed, SecVal(1)), SecVal(2)))
        IF (PRESENT(HashPair)) THEN
            HashPair(1) = Low
            Pack2I64Array(BaseSecret, 64, SecVal, 2)
            HashPair(2) = XXH64_Avalanche(IEOR(IEOR(Seed, SecVal(1)), SecVal(2)))
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
                    Pack2I64Array(Input, OffSet,           InpVal,  8)
                    Pack2I64Array(Input, Offset+Length-64, InpVal2, 8)
                    Pack2I64Array(BaseSecret, 0,           SecVal,  16)
                    Input0 = InpVal(7)
                    Input1 = InpVal(8)
                    Input2 = InpVal2(SIZE(InpVal2)-7)
                    Input3 = InpVal2(SIZE(InpVal2)-6)
                    Acc0 = XXH128_Mix32B_Once(Seed, SecVal, 13, Acc0, \
                                                Input0, Input1, Input2, Input3)
                    Acc1 = XXH128_Mix32B_Once(Seed, SecVal, 15, Acc1, \
                                                Input2, Input3, Input0, Input1)
                ELSE
                    Pack2I64Array(Input, OffSet,           InpVal,  6)
                    Pack2I64Array(Input, Offset+Length-48, InpVal2, 6)
                    Pack2I64Array(BaseSecret, 0,           SecVal,  12)
                END IF
                Input0 = InpVal(5)
                Input1 = InpVal(6)
                Input2 = InpVal2(SIZE(InpVal2)-5)
                Input3 = InpVal2(SIZE(InpVal2)-4)
                Acc0 = XXH128_Mix32B_Once(Seed, SecVal, 9, Acc0, \
                                            Input0, Input1, Input2, Input3)
                Acc1 = XXH128_Mix32B_Once(Seed, SecVal, 11, Acc1, \
                                            Input2, Input3, Input0, Input1)
            ELSE
                Pack2I64Array(Input, OffSet,           InpVal,  4)
                Pack2I64Array(Input, Offset+Length-32, InpVal2, 4)
                Pack2I64Array(BaseSecret, 0,           SecVal,  8)
            END IF
            Input0 = InpVal(3)
            Input1 = InpVal(4)
            Input2 = InpVal2(SIZE(InpVal2)-3)
            Input3 = InpVal2(SIZE(InpVal2)-2)
            Acc0 = XXH128_Mix32B_Once(Seed, SecVal, 5, Acc0, Input0, Input1, Input2, Input3)
            Acc1 = XXH128_Mix32B_Once(Seed, SecVal, 7, Acc1, Input2, Input3, Input0, Input1)
        ELSE
            Pack2I64Array(Input, OffSet,           InpVal,  2)
            Pack2I64Array(Input, Offset+Length-16, InpVal2, 2)
            Pack2I64Array(BaseSecret, 0,           SecVal,  4)
        END IF
        Input0 = InpVal(1)
        Input1 = InpVal(2)
        Input2 = InpVal2(SIZE(InpVal2)-1)
        Input3 = InpVal2(SIZE(InpVal2))
        Acc0 = XXH128_Mix32B_Once(Seed, SecVal, 1,  Acc0, Input0, Input1, Input2, Input3)
        Acc1 = XXH128_Mix32B_Once(Seed, SecVal, 3, Acc1, Input2, Input3, Input0, Input1)
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
        Pack2I64Array(Input, OffSet, InpVal, (NbRounds-1)*4)
        Pack2I64Array(BaseSecret, 0, SecVal, 16)
        DO WHILE (I < 4)
            Input0 = InpVal(4*I+1)
            Input1 = InpVal(4*I+2)
            Input2 = InpVal(4*I+3)
            Input3 = InpVal(4*I+4)
            Acc0 = XXH128_Mix32B_Once(Seed, SecVal, 4*I+1, Acc0, \
                                        Input0, Input1, Input2, Input3)
            Acc1 = XXH128_Mix32B_Once(Seed, SecVal, 4*I+3, Acc1, \
                                        Input2, Input3, Input0, Input1)
            I = I + 1
        END DO
        Acc0 = XXH3_Avalanche(Acc0)
        Acc1 = XXH3_Avalanche(Acc1)
        Pack2I64Array(BaseSecret, 3, SecVal, NbRounds*4-16)
        DO WHILE (I < NbRounds)
            Input0 = InpVal(4*I+1)
            Input1 = InpVal(4*I+2)
            Input2 = InpVal(4*I+3)
            Input3 = InpVal(4*I+4)
            Acc0 = XXH128_Mix32B_Once(Seed, SecVal, 4*(I-4)+1, Acc0, \
                                        Input0, Input1, Input2, Input3)
            Acc1 = XXH128_Mix32B_Once(Seed, SecVal, 4*(I-4)+3, Acc1, \
                                        Input2, Input3, Input0, Input1)
            I = I + 1
        END DO

        ! last bytes
        Pack2I64Array(Input, Offset+Length-32, InpVal, 4)
        Pack2I64Array(BaseSecret, 103,         SecVal, 4)
        Input0 = InpVal(3)
        Input1 = InpVal(4)
        Input2 = InpVal(1)
        Input3 = InpVal(2)
        Acc0 = XXH128_Mix32B_Once((-Seed), SecVal, 1, Acc0, Input0, Input1, Input2, Input3)
        Acc1 = XXH128_Mix32B_Once((-Seed), SecVal, 3, Acc1, Input2, Input3, Input0, Input1)

        Low = XXH3_Avalanche(Acc0 + Acc1)
        IF (PRESENT(HashPair)) THEN
            HashPair(1) = Low
            HashPair(2) = -XXH3_Avalanche(Acc0*XXH_PRIME64_1 + Acc1*XXH_PRIME64_4 &
                        + (ToLong(Length) - Seed)*XXH_PRIME64_2)
        END IF
        HashCode = Low
        RETURN
    END IF

    BLOCK
        ! XXH3_hashLong_128b_internal
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

        ! XXH3_hashLong_internal_loop
        Nb_Blocks = (Length - 1) / Block_Len
        DO N = 0, Nb_Blocks-1
            ! XXH3_Accumulate
            OffBlock = Offset + N*Block_Len
            DO S = 0, NbStripesPerBlock-1
                ! XXH3_Accumulate_512
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
        DO S = 0, NbStripes-1
            ! XXH3_Accumulate_512
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
        ! XXH3_Accumulate_512
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
        Low = ToLong(Length)*XXH_PRIME64_1                  &
                    + XXH3_Mix2Accs(Acc(0), Acc(1), SecVal, 1) &
                    + XXH3_Mix2Accs(Acc(2), Acc(3), SecVal, 3) &
                    + XXH3_Mix2Accs(Acc(4), Acc(5), SecVal, 5) &
                    + XXH3_Mix2Accs(Acc(6), Acc(7), SecVal, 7)
        Low = XXH3_Avalanche(Low)
        IF (PRESENT(HashPair)) THEN
            Pack2I64Array(CSInput, 192 - 64 - 11, SecVal, 8)
            High = NOT(ToLong(Length)*XXH_PRIME64_2)            &
                        + XXH3_Mix2Accs(Acc(0), Acc(1), SecVal, 1) &
                        + XXH3_Mix2Accs(Acc(2), Acc(3), SecVal, 3) &
                        + XXH3_Mix2Accs(Acc(4), Acc(5), SecVal, 5) &
                        + XXH3_Mix2Accs(Acc(6), Acc(7), SecVal, 7)
            High = XXH3_Avalanche(High)
            HashPair(1) = Low
            HashPair(2) = High
        END IF
        HashCode = Low

    END BLOCK

    ! free pointers
    NULLIFY(CSInput)

#undef XXH3_Mix2Accs
#undef XXH128_Mix32B_Once
#undef UnsignedInt
#undef Pack2I64Arra

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
        tByte       :: Input(0:7)
        tLong       :: Output
        EQUIVALENCE (Output, Input)

    ! FLOW

        ! implementation algorithm #7
        Input(0:7) = Buf(Off:Off+7)
        Res = Output

        RETURN

    END FUNCTION Pack_I64

    !**************************************************************************

END FUNCTION XX3_Hash128_08

!******************************************************************************

FUNCTION XX3_Hash128_09(Input, Seed, BaseSecret, CustomSecret, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the XX3Hash128 hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, TARGET,   INTENT(IN)     :: Input(0:)            ! input bytes
    tLong,           INTENT(IN)     :: Seed                 ! seed
    tByte,           INTENT(IN)     :: BaseSecret(0:191)    ! base secret
    tByte, OPTIONAL, INTENT(IN)     :: CustomSecret(0:191)  ! custom secret
    tLong, OPTIONAL, INTENT(OUT)    :: HashPair(2)          ! dual hash code
    TARGET                          :: BaseSecret, CustomSecret
    tLong                           :: HashCode             ! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: SecInput(:) => NULL()
    tIndex          :: Length, Offset
    tLong           :: Low, High
    tLong           :: BitFlipL, BitFlipH, Input_Lo, Input_Hi
    tLong           :: S, PL, Bitflip, Keyed, M128_Lo, M128_Hi
    tInteger        :: C1, C2, C3, CombinedL, CombinedH
    tLong           :: Acc0, Acc1, Input0, Input1, Input2, Input3
    tIndex          :: I
    tInteger        :: NbRounds
    ! variables used to quickly access input and secret bytes
    tByte           :: InpBytes(0:127)
    tLong           :: InpLongs(1:16)
    EQUIVALENCE(InpBytes, InpLongs)
    tByte           :: SecBytes(0:127)
    tLong           :: SecLongs(1:16)
    EQUIVALENCE(SecBytes, SecLongs)
    tByte           :: Bytes(0:15)
    tInteger        :: I32Val(4)
    EQUIVALENCE(Bytes, I32Val)

!** FLOW

! define macros for better performance than using internal procedures
#define XXH3_Mix2Accs(Acc_LH,Acc_RH,Secret,OffSec)  UnsignedLongMultiplyorFold(IEOR(Acc_LH, Secret(OffSec)), IEOR(Acc_RH, Secret(OffSec+1)))
#define XXH128_Mix32B_Once(Seed,Secret,OffSec,Acc,Input0,Input1,Input2,Input3)  IEOR(Acc + UnsignedLongMultiplyorFold(IEOR(Input0, (Secret(OffSec)   + Seed)), IEOR(Input1, (Secret(OffSec+1) - Seed))), (Input2 + Input3))
#define UnsignedInt(I)      IAND(ToLong(I), ToLong(Z'00000000FFFFFFFF'))

    ! initialize
    Length = SIZE(Input)
    Offset = 0

    ! perform hashing
    IF (Length <= 16) THEN
        ! XXH3_len_0to16_128b
        IF (Length > 8) THEN
            ! XXH3_len_9to16_128b
            SecBytes(0:31) = BaseSecret(32:63)
            BitflipL = IEOR(SecLongs(1), SecLongs(2)) - Seed
            BitflipH = IEOR(SecLongs(3), SecLongs(4)) + Seed
            InpBytes(0:7) = Input(Offset+Length-8:Offset+Length-1)
            InpBytes(8:15) = Input(Offset:Offset+7)
            Input_Hi = InpLongs(1)
            Input_Lo = IEOR(IEOR(InpLongs(2), Input_Hi), BitflipL)
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
            ! high int will be shifted
            Bytes(0:3) = Input(Offset:Offset+3)
            Bytes(4:7) = Input(Offset+Length-4:Offset+Length-1)
            Input_Lo = MaskI32(I32Val(1))
            Input_Hi = ToLong(I32Val(2))
            SecBytes(0:15) = BaseSecret(16:31)
            Bitflip = IEOR(SecLongs(1), SecLongs(2)) + S
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
            C1 = MaskI8(Input(Offset))
            ! high 3 bytes will be shifted
            C2 = ToInteger(Input(Offset + SHIFTA(Length, 1)))
            C3 = MaskI8(Input(Offset + Length - 1))
            CombinedL = IOR(IOR(IOR(SHIFTL(C1, 16), SHIFTL(C2, 24)), C3), SHIFTL(Length, 8))
            CombinedH = RotateLeft(IntegerReverseBytes(CombinedL), 13)
            Bytes(0:15) = BaseSecret(0:15)
            BitFlipL = UnsignedInt(IEOR(I32Val(1), I32Val(2))) + Seed
            BitFlipH = UnsignedInt(IEOR(I32Val(3), I32Val(4))) - Seed
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
        SecBytes(0:15) = BaseSecret(64:79)
        Low = XXH64_Avalanche(IEOR(IEOR(Seed, SecLongs(1)), SecLongs(2)))
        IF (PRESENT(HashPair)) THEN
            HashPair(1) = Low
            SecBytes(0:15) = BaseSecret(80:95)
            HashPair(2) = XXH64_Avalanche(IEOR(IEOR(Seed, SecLongs(1)), SecLongs(2)))
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
                    InpBytes(0:63)   = Input(Offset:Offset+63)
                    InpBytes(64:127) = Input(Offset+Length-64:Offset+Length-1)
                    SecBytes(0:127)  = BaseSecret(0:127)
                    Input0 = InpLongs(7)
                    Input1 = InpLongs(8)
                    Input2 = InpLongs(9)
                    Input3 = InpLongs(10)
                    Acc0 = XXH128_Mix32B_Once(Seed, SecLongs, 13, Acc0, \
                                                Input0, Input1, Input2, Input3)
                    Acc1 = XXH128_Mix32B_Once(Seed, SecLongs, 15, Acc1, \
                                                Input2, Input3, Input0, Input1)
                ELSE
                    InpBytes(0:47)   = Input(Offset:Offset+47)
                    InpBytes(80:127) = Input(Offset+Length-48:Offset+Length-1)
                    SecBytes(0:95)   = BaseSecret(0:95)
                END IF
                Input0 = InpLongs(5)
                Input1 = InpLongs(6)
                Input2 = InpLongs(11)
                Input3 = InpLongs(12)
                Acc0 = XXH128_Mix32B_Once(Seed, SecLongs, 9, Acc0, \
                                            Input0, Input1, Input2, Input3)
                Acc1 = XXH128_Mix32B_Once(Seed, SecLongs, 11, Acc1, \
                                            Input2, Input3, Input0, Input1)
            ELSE
                InpBytes(0:31)   = Input(Offset:Offset+31)
                InpBytes(96:127) = Input(Offset+Length-32:Offset+Length-1)
                SecBytes(0:63)   = BaseSecret(0:63)
            END IF
            Input0 = InpLongs(3)
            Input1 = InpLongs(4)
            Input2 = InpLongs(13)
            Input3 = InpLongs(14)
            Acc0 = XXH128_Mix32B_Once(Seed, SecLongs, 5, Acc0, Input0, Input1, Input2, Input3)
            Acc1 = XXH128_Mix32B_Once(Seed, SecLongs, 7, Acc1, Input2, Input3, Input0, Input1)
        ELSE
            InpBytes(0:15)    = Input(Offset:Offset+15)
            InpBytes(112:127) = Input(Offset+Length-16:Offset+Length-1)
            SecBytes(0:31)    = BaseSecret(0:31)
        END IF
        Input0 = InpLongs(1)
        Input1 = InpLongs(2)
        Input2 = InpLongs(15)
        Input3 = InpLongs(16)
        Acc0 = XXH128_Mix32B_Once(Seed, SecLongs, 1,  Acc0, Input0, Input1, Input2, Input3)
        Acc1 = XXH128_Mix32B_Once(Seed, SecLongs, 3, Acc1, Input2, Input3, Input0, Input1)
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
        InpBytes(0:127) = Input(Offset:Offset+127)
        SecBytes(0:127) = BaseSecret(0:127)
        DO WHILE (I < 4)
            Input0 = InpLongs(4*I+1)
            Input1 = InpLongs(4*I+2)
            Input2 = InpLongs(4*I+3)
            Input3 = InpLongs(4*I+4)
            Acc0 = XXH128_Mix32B_Once(Seed, SecLongs, 4*I+1, Acc0, \
                                        Input0, Input1, Input2, Input3)
            Acc1 = XXH128_Mix32B_Once(Seed, SecLongs, 4*I+3, Acc1, \
                                        Input2, Input3, Input0, Input1)
            I = I + 1
        END DO
        Acc0 = XXH3_Avalanche(Acc0)
        Acc1 = XXH3_Avalanche(Acc1)
        Offset = Offset + 128
        InpBytes(0:(NbRounds*4-16)*8-1) = Input(Offset:Offset+(NbRounds*4-16)*8-1)
        SecBytes(0:(NbRounds*4-16)*8-1) = BaseSecret(3:3+(NbRounds*4-16)*8-1)
        DO WHILE (I < NbRounds)
            Input0 = InpLongs(4*(I-4)+1)
            Input1 = InpLongs(4*(I-4)+2)
            Input2 = InpLongs(4*(I-4)+3)
            Input3 = InpLongs(4*(I-4)+4)
            Acc0 = XXH128_Mix32B_Once(Seed, SecLongs, 4*(I-4)+1, Acc0, \
                                        Input0, Input1, Input2, Input3)
            Acc1 = XXH128_Mix32B_Once(Seed, SecLongs, 4*(I-4)+3, Acc1, \
                                        Input2, Input3, Input0, Input1)
            I = I + 1
        END DO

        ! last bytes
        Offset = Offset - 128
        InpBytes(0:31) = Input(Offset+Length-32:Length-1)
        SecBytes(0:31) = BaseSecret(103:134)
        Input0 = InpLongs(3)
        Input1 = InpLongs(4)
        Input2 = InpLongs(1)
        Input3 = InpLongs(2)
        Acc0 = XXH128_Mix32B_Once((-Seed), SecLongs, 1, Acc0, Input0, Input1, Input2, Input3)
        Acc1 = XXH128_Mix32B_Once((-Seed), SecLongs, 3, Acc1, Input2, Input3, Input0, Input1)

        Low = XXH3_Avalanche(Acc0 + Acc1)
        IF (PRESENT(HashPair)) THEN
            HashPair(1) = Low
            HashPair(2) = -XXH3_Avalanche(Acc0*XXH_PRIME64_1 + Acc1*XXH_PRIME64_4 &
                        + (ToLong(Length) - Seed)*XXH_PRIME64_2)
        END IF
        HashCode = Low
        RETURN
    END IF

    BLOCK
        ! XXH3_hashLong_128b_internal
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

        ! XXH3_hashLong_internal_loop
        Nb_Blocks = (Length - 1) / Block_Len
        DO N = 0, Nb_Blocks-1
            ! XXH3_Accumulate
            OffBlock = Offset + N*Block_Len
            DO S = 0, NbStripesPerBlock-1
                ! XXH3_Accumulate_512
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
        DO S = 0, NbStripes-1
            ! XXH3_Accumulate_512
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
        ! XXH3_Accumulate_512
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
        Low = ToLong(Length)*XXH_PRIME64_1                    &
                    + XXH3_Mix2Accs(Acc(0), Acc(1), SecLongs, 1) &
                    + XXH3_Mix2Accs(Acc(2), Acc(3), SecLongs, 3) &
                    + XXH3_Mix2Accs(Acc(4), Acc(5), SecLongs, 5) &
                    + XXH3_Mix2Accs(Acc(6), Acc(7), SecLongs, 7)
        Low = XXH3_Avalanche(Low)
        IF (PRESENT(HashPair)) THEN
            SecBytes(0:63) = SecInput(117:180)
            High = NOT(ToLong(Length)*XXH_PRIME64_2)              &
                        + XXH3_Mix2Accs(Acc(0), Acc(1), SecLongs, 1) &
                        + XXH3_Mix2Accs(Acc(2), Acc(3), SecLongs, 3) &
                        + XXH3_Mix2Accs(Acc(4), Acc(5), SecLongs, 5) &
                        + XXH3_Mix2Accs(Acc(6), Acc(7), SecLongs, 7)
            High = XXH3_Avalanche(High)
            HashPair(1) = Low
            HashPair(2) = High
        END IF
        HashCode = Low

    END BLOCK

    NULLIFY(SecInput)

#undef XXH3_Mix2Accs
#undef XXH128_Mix32B_Once
#undef UnsignedInt

    RETURN

END FUNCTION XX3_Hash128_09

!******************************************************************************

END SUBMODULE SubBase_XX3Hash128_Exp

!******************************************************************************

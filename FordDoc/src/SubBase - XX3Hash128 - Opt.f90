
SUBMODULE (ModBase_OptimalHash64 : SubBase_XX3Hash64_Opt) SubBase_XX3Hash128_Opt

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an optimal implementation for the XX3 hash algorithms
!   for 128-bit integer output by Yann Collet. <br>

!** USE STATEMENTS:
    USE ModBase_SIntUtil,   ONLY: IntegerReverseBytes => ReverseBytes
    USE ModBase_UIntUtil,   ONLY: UnsignedLongMultiplyHigh => UMul128_Upper64

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define MaskI8(X)           IAND(ToLong(X), Z'00000000000000FF')
#define MaskI32(X)          IAND(ToLong(X), Z'00000000FFFFFFFF')
#define UnsignedInt(I)      IAND(ToLong(I), ToLong(Z'00000000FFFFFFFF'))
#define XXH128_Mix32B_Once(Seed,Secret,OffSec,Acc,Input0,Input1,Input2,Input3) \
    IEOR(Acc + UnsignedLongMultiplyorFold(IEOR(Input0, (Secret(OffSec)   + Seed)),  \
                                          IEOR(Input1, (Secret(OffSec+1) - Seed))), \
                                              (Input2 + Input3))

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

MODULE FUNCTION XX3_Hash128_Opt(Input, InpSize, StartHash, RemoveSign, &
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

!** FLOW

    ! initialize
    SET_OPTION(Seed, 0_kInteger, StartHash)
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)

    ! perform hashing
    IF (PRESENT(Secret)) THEN
        IF (SIZE(Secret) >= 192) THEN
            HashCode = XX3_Hash128(InpPtr, Seed, Secret, HashPair)
        ELSE
            BLOCK
                tByte       :: CustomSecret(192)
                tInteger    :: InSize
                InSize = SIZE(Secret)
                CustomSecret(1:InSize)  = Secret(1:InSize)
                CustomSecret(InSize+1:) = XXH3_Secret_Bytes(1:)
                HashCode = XX3_Hash128(InpPtr, Seed, CustomSecret, HashPair)
            END BLOCK
        END IF
    ELSE
        HashCode = XX3_Hash128(InpPtr, Seed, HashPair=HashPair)
    END IF

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION XX3_Hash128_Opt

!******************************************************************************

FUNCTION XX3_Hash128(Input, Seed, CustomSecret, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the XX3Hash128 hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,           INTENT(IN)     :: Input(0:)            ! input bytes
    tLong,           INTENT(IN)     :: Seed                 ! seed
    tByte, OPTIONAL, INTENT(IN)     :: CustomSecret(0:191)  ! custom secret
    tLong, OPTIONAL, INTENT(OUT)    :: HashPair(2)          ! dual hash code
    tLong                           :: HashCode             ! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Length

!** FLOW

    Length = SIZE(Input)
    Length = SIZE(Input)
    IF (Length <= 16) THEN
        HashCode = XX3Hash128_Len1To16(Input, Seed, Length, XXH3_Secret_Integer, &
                                       XXH3_Secret_Long1, HashPair)
    ELSEIF (Length <= 128) THEN
        HashCode = XX3Hash128_Len17To128(Input, Seed, Length, XXH3_Secret_Long1, HashPair)
    ELSEIF (Length <= 240) THEN
        HashCode = XX3Hash128_Len129To240(Input, Seed, Length, XXH3_Secret_Long1, &
                                          XXH3_Secret_Long2, XXH3_Secret_Long3, HashPair)
    ELSE
        IF (PRESENT(CustomSecret)) THEN
            HashCode = XX3Hash_Len241Up_Custom(Input, Seed, Length, CustomSecret)
        ELSE
            HashCode = XX3Hash_Len241Up_Base(Input, Seed, Length, XXH3_Secret_Long1, &
                                             XXH3_Secret_Long4, XXH3_Secret_Long5,   &
                                             XXH3_Secret_Long6, HashPair)
        END IF
    END IF

    RETURN

END FUNCTION XX3_Hash128

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                             +
!                           HASH FUNCTIONS - ENGINES                          +
!                                                                             +
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION XX3Hash128_Len1To16(Input, Seed, Length, IntSecret, &
                             LongSecret, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the XX3Hash128 hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,           INTENT(IN)     :: Input(0:)            ! input bytes
    tLong,           INTENT(IN)     :: Seed                 ! seed
    tIndex,          INTENT(IN)     :: Length               ! length of input bytes
    tInteger,        INTENT(IN)     :: IntSecret(0:3)       ! base secret in integers
    tLong,           INTENT(IN)     :: LongSecret(0:23)     ! base secret in longs
    tLong, OPTIONAL, INTENT(OUT)    :: HashPair(2)          ! dual hash code
    tLong                           :: HashCode             ! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong           :: Low, High
    tLong           :: BitFlipL, BitFlipH, Input_Lo, Input_Hi
    tLong           :: S, PL, Bitflip, Keyed, M128_Lo, M128_Hi
    tInteger        :: C1, C2, C3, CombinedL, CombinedH
    ! variables used to quickly access input bytes
    tByte           :: InpBytes(0:127)
    tLong           :: InpLongs(1:16)
    EQUIVALENCE(InpBytes, InpLongs)
    tByte           :: Bytes(0:7)
    tInteger        :: I32Val(1:2)
    EQUIVALENCE(Bytes, I32Val)

!** FLOW

    ! perform hashing
    ! XXH3_len_0to16_128b
    IF (Length > 8) THEN
        ! XXH3_len_9to16_128b
        BitflipL = IEOR(LongSecret(4), LongSecret(5)) - Seed
        BitflipH = IEOR(LongSecret(6), LongSecret(7)) + Seed
        InpBytes(0:7) = Input(Length-8:Length-1)
        InpBytes(8:15) = Input(0:7)
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
        Bytes(0:3) = Input(0:3)
        Bytes(4:7) = Input(Length-4:Length-1)
        Input_Lo = MaskI32(I32Val(1))
        Input_Hi = ToLong(I32Val(2))
        Bitflip = IEOR(LongSecret(2), LongSecret(3)) + S
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
        C1 = MaskI8(Input(0))
        ! high 3 bytes will be shifted
        C2 = ToInteger(Input(0 + SHIFTA(Length, 1)))
        C3 = MaskI8(Input(0 + Length - 1))
        CombinedL = IOR(IOR(IOR(SHIFTL(C1, 16), SHIFTL(C2, 24)), C3), SHIFTL(Length, 8))
        CombinedH = RotateLeft(IntegerReverseBytes(CombinedL), 13)
        BitFlipL = UnsignedInt(IEOR(IntSecret(0), IntSecret(1))) + Seed
        BitFlipH = UnsignedInt(IEOR(IntSecret(2), IntSecret(3))) - Seed
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
    Low = XXH64_Avalanche(IEOR(IEOR(Seed, LongSecret(8)), LongSecret(9)))
    IF (PRESENT(HashPair)) THEN
        HashPair(1) = Low
        HashPair(2) = XXH64_Avalanche(IEOR(IEOR(Seed, LongSecret(10)), LongSecret(11)))
    END IF
    HashCode = Low

    RETURN

END FUNCTION XX3Hash128_Len1To16

!**************************************************************************

FUNCTION XX3Hash128_Len17To128(Input, Seed, Length, LongSecret, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the XX3Hash128 hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,           INTENT(IN)     :: Input(0:)            ! input bytes
    tLong,           INTENT(IN)     :: Seed                 ! seed
    tIndex,          INTENT(IN)     :: Length               ! length of input bytes
    tLong,           INTENT(IN)     :: LongSecret(0:23)     ! base secret in longs
    tLong, OPTIONAL, INTENT(OUT)    :: HashPair(2)          ! dual hash code
    tLong                           :: HashCode             ! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong           :: Low
    tLong           :: Acc0, Acc1, Input0, Input1, Input2, Input3
    ! variables used to quickly access input and secret bytes
    tByte           :: InpBytes(0:127)
    tLong           :: InpLongs(1:16)
    EQUIVALENCE(InpBytes, InpLongs)

!** FLOW

    ! perform hashing
    ! XXH3_len_17to128_128b
    Acc0 = ToLong(Length)*XXH_PRIME64_1
    Acc1 = 0
    IF (Length > 32) THEN
        IF (Length > 64) THEN
            IF (Length > 96) THEN
                InpBytes(0:63)   = Input(0:63)
                InpBytes(64:127) = Input(Length-64:Length-1)
                Input0 = InpLongs(7)
                Input1 = InpLongs(8)
                Input2 = InpLongs(9)
                Input3 = InpLongs(10)
                Acc0 = XXH128_Mix32B_Once(Seed, LongSecret, 12, Acc0, \
                                            Input0, Input1, Input2, Input3)
                Acc1 = XXH128_Mix32B_Once(Seed, LongSecret, 14, Acc1, \
                                            Input2, Input3, Input0, Input1)
            ELSE
                InpBytes(0:47)   = Input(0:47)
                InpBytes(80:127) = Input(Length-48:Length-1)
            END IF
            Input0 = InpLongs(5)
            Input1 = InpLongs(6)
            Input2 = InpLongs(11)
            Input3 = InpLongs(12)
            Acc0 = XXH128_Mix32B_Once(Seed, LongSecret, 8, Acc0, \
                                        Input0, Input1, Input2, Input3)
            Acc1 = XXH128_Mix32B_Once(Seed, LongSecret, 10, Acc1,  \
                                        Input2, Input3, Input0, Input1)
        ELSE
            InpBytes(0:31)   = Input(0:31)
            InpBytes(96:127) = Input(Length-32:Length-1)
        END IF
        Input0 = InpLongs(3)
        Input1 = InpLongs(4)
        Input2 = InpLongs(13)
        Input3 = InpLongs(14)
        Acc0 = XXH128_Mix32B_Once(Seed, LongSecret, 4, Acc0, Input0, Input1, Input2, Input3)
        Acc1 = XXH128_Mix32B_Once(Seed, LongSecret, 6, Acc1, Input2, Input3, Input0, Input1)
    ELSE
        InpBytes(0:15)    = Input(0:15)
        InpBytes(112:127) = Input(Length-16:Length-1)
    END IF
    Input0 = InpLongs(1)
    Input1 = InpLongs(2)
    Input2 = InpLongs(15)
    Input3 = InpLongs(16)
    Acc0 = XXH128_Mix32B_Once(Seed, LongSecret, 0,  Acc0, Input0, Input1, Input2, Input3)
    Acc1 = XXH128_Mix32B_Once(Seed, LongSecret, 2, Acc1, Input2, Input3, Input0, Input1)
    Low = XXH3_Avalanche(Acc0 + Acc1)
    IF (PRESENT(HashPair)) THEN
        HashPair(1) = Low
        HashPair(2) = -XXH3_Avalanche(Acc0*XXH_PRIME64_1 + Acc1*XXH_PRIME64_4 &
                    + (ToLong(Length) - Seed)*XXH_PRIME64_2)
    END IF
    HashCode = Low

    RETURN

END FUNCTION XX3Hash128_Len17To128

!**************************************************************************

FUNCTION XX3Hash128_Len129To240(Input, Seed, Length, LongSecret1, LongSecret2, &
                                LongSecret3, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the XX3Hash128 hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,           INTENT(IN)     :: Input(0:)            ! input bytes
    tLong,           INTENT(IN)     :: Seed                 ! seed
    tIndex,          INTENT(IN)     :: Length               ! length of input bytes
    tLong,           INTENT(IN)     :: LongSecret1(0:23)    ! base secret
    tLong,           INTENT(IN)     :: LongSecret2(0:13)    ! base secret
    tLong,           INTENT(IN)     :: LongSecret3(0:3)     ! base secret
    tLong, OPTIONAL, INTENT(OUT)    :: HashPair(2)          ! dual hash code
    tLong                           :: HashCode             ! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: Offset
    tLong           :: Low
    tLong           :: Acc0, Acc1, Input0, Input1, Input2, Input3
    tIndex          :: I
    tInteger        :: NbRounds
    ! variables used to quickly access input and secret bytes
    tByte           :: InpBytes(0:127)
    tLong           :: InpLongs(1:16)
    EQUIVALENCE(InpBytes, InpLongs)

!** FLOW

    ! initialize
    Offset = 0

    ! XXH3_len_129to240_128b
    NbRounds = Length / 32
    Acc0 = ToLong(Length)*XXH_PRIME64_1
    Acc1 = 0_kLong
    I = 0
    InpBytes(0:127) = Input(Offset:Offset+127)
    DO WHILE (I < 4)
        Input0 = InpLongs(4*I+1)
        Input1 = InpLongs(4*I+2)
        Input2 = InpLongs(4*I+3)
        Input3 = InpLongs(4*I+4)
        Acc0 = XXH128_Mix32B_Once(Seed, LongSecret1, 4*I, Acc0, \
                                    Input0, Input1, Input2, Input3)
        Acc1 = XXH128_Mix32B_Once(Seed, LongSecret1, 4*I+2, Acc1, \
                                    Input2, Input3, Input0, Input1)
        I = I + 1
    END DO
    Acc0 = XXH3_Avalanche(Acc0)
    Acc1 = XXH3_Avalanche(Acc1)
    Offset = Offset + 128
    InpBytes(0:(NbRounds*4-16)*8-1) = Input(Offset:Offset+(NbRounds*4-16)*8-1)
    DO WHILE (I < NbRounds)
        Input0 = InpLongs(4*(I-4)+1)
        Input1 = InpLongs(4*(I-4)+2)
        Input2 = InpLongs(4*(I-4)+3)
        Input3 = InpLongs(4*(I-4)+4)
        Acc0 = XXH128_Mix32B_Once(Seed, LongSecret2, 4*(I-4), Acc0, \
                                    Input0, Input1, Input2, Input3)
        Acc1 = XXH128_Mix32B_Once(Seed, LongSecret2, 4*(I-4)+2, Acc1, \
                                    Input2, Input3, Input0, Input1)
        I = I + 1
    END DO

    ! last bytes
    Offset = Offset - 128
    InpBytes(0:31) = Input(Offset+Length-32:Length-1)
    Input0 = InpLongs(3)
    Input1 = InpLongs(4)
    Input2 = InpLongs(1)
    Input3 = InpLongs(2)
    Acc0 = XXH128_Mix32B_Once((-Seed), LongSecret3, 0, Acc0, Input0, Input1, Input2, Input3)
    Acc1 = XXH128_Mix32B_Once((-Seed), LongSecret3, 2, Acc1, Input2, Input3, Input0, Input1)

    Low = XXH3_Avalanche(Acc0 + Acc1)
    IF (PRESENT(HashPair)) THEN
        HashPair(1) = Low
        HashPair(2) = -XXH3_Avalanche(Acc0*XXH_PRIME64_1 + Acc1*XXH_PRIME64_4 &
                    + (ToLong(Length) - Seed)*XXH_PRIME64_2)
    END IF
    HashCode = Low

    RETURN

END FUNCTION XX3Hash128_Len129To240

!**************************************************************************

END SUBMODULE SubBase_XX3Hash128_Opt

!******************************************************************************

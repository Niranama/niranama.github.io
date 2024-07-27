
SUBMODULE (ModBase_OptimalHash64) SubBase_XX3Hash64_Opt

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an optimal implementation for the XX3 hash algorithms
!   for 64-bit integer output by Yann Collet. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    USE ModBase_SIntUtil,   ONLY: LongReverseBytes    => ReverseBytes

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define MaskI8(X)           IAND(ToLong(X), Z'00000000000000FF')
#define MaskI32(X)          IAND(ToLong(X), Z'00000000FFFFFFFF')
#define UnsignedInt(I)      IAND(ToLong(I), ToLong(Z'00000000FFFFFFFF'))
#define XXH3_Mix16B(Seed,Input,OffIn,Secret,OffSec) \
    UnsignedLongMultiplyorFold(IEOR(Input(OffIn),   Secret(OffSec)   + Seed), \
                    IEOR(Input(OffIn+1), Secret(OffSec+1) - Seed))
#define XXH3_Mix2Accs(Acc_LH,Acc_RH,Secret,OffSec) \
    UnsignedLongMultiplyorFold(IEOR(Acc_LH, Secret(OffSec)), IEOR(Acc_RH, Secret(OffSec+1)))

!** MODULE PARAMETERS:
    tLong,  PARAMETER   :: MaxU32 = ToLong(Z'00000000FFFFFFFF')
    ! *****************************************************************
    ! *****     Pseudorandom secret taken directly from FARSH     *****
    ! *****************************************************************
    ! base secret parameters in bytes for non-optimized versions
    tByte,     PARAMETER    :: XXH3_Secret_Bytes(192) =                     &
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
    ! base secret parameters in longs used for general optimization
    tLong,     PARAMETER    :: XXH3_Secret_Long1(0:23) = &
               [ToLong(Z'BE4BA423396CFEB8'), ToLong(Z'1CAD21F72C81017C'), &
                ToLong(Z'DB979083E96DD4DE'), ToLong(Z'1F67B3B7A4A44072'), &
                ToLong(Z'78E5C0CC4EE679CB'), ToLong(Z'2172FFCC7DD05A82'), &
                ToLong(Z'8E2443F7744608B8'), ToLong(Z'4C263A81E69035E0'), &
                ToLong(Z'CB00C391BB52283C'), ToLong(Z'A32E531B8B65D088'), &
                ToLong(Z'4EF90DA297486471'), ToLong(Z'D8ACDEA946EF1938'), &
                ToLong(Z'3F349CE33F76FAA8'), ToLong(Z'1D4F0BC7C7BBDCF9'), &
                ToLong(Z'3159B4CD4BE0518A'), ToLong(Z'647378D9C97E9FC8'), &
                ToLong(Z'C3EBD33483ACC5EA'), ToLong(Z'EB6313FAFFA081C5'), &
                ToLong(Z'49DAF0B751DD0D17'), ToLong(Z'9E68D429265516D3'), &
                ToLong(Z'FCA1477D58BE162B'), ToLong(Z'CE31D07AD1B8F88F'), &
                ToLong(Z'280416958F3ACB45'), ToLong(Z'7E404BBBCAFBD7AF')]
    ! base secret parameters in longs used for medium-message (129-240) optimization
    tLong,     PARAMETER    :: XXH3_Secret_Long2(0:13) = &
               [ToLong(Z'81017CBE4BA42339'), ToLong(Z'6DD4DE1CAD21F72C'), &
                ToLong(Z'A44072DB979083E9'), ToLong(Z'E679CB1F67B3B7A4'), &
                ToLong(Z'D05A8278E5C0CC4E'), ToLong(Z'4608B82172FFCC7D'), &
                ToLong(Z'9035E08E2443F774'), ToLong(Z'52283C4C263A81E6'), &
                ToLong(Z'65D088CB00C391BB'), ToLong(Z'486471A32E531B8B'), &
                ToLong(Z'EF19384EF90DA297'), ToLong(Z'76FAA8D8ACDEA946'), &
                ToLong(Z'BBDCF93F349CE33F'), ToLong(Z'E0518A1D4F0BC7C7')]
    tLong,     PARAMETER    :: XXH3_Secret_Long3(0:3) = &
               [ToLong(Z'4F0BC7C7BBDCF93F'), ToLong(Z'59B4CD4BE0518A1D'), &
                ToLong(Z'7378D9C97E9FC831'), ToLong(Z'EBD33483ACC5EA64')]
    ! base secret parameters in longs used for long-message (240 up) optimization
    tLong,     PARAMETER    :: XXH3_Secret_Long4(0:7) = &
               [ToLong(Z'EA647378D9C97E9F'), ToLong(Z'C5C3EBD33483ACC5'), &
                ToLong(Z'17EB6313FAFFA081'), ToLong(Z'D349DAF0B751DD0D'), &
                ToLong(Z'2B9E68D429265516'), ToLong(Z'8FFCA1477D58BE16'), &
                ToLong(Z'45CE31D07AD1B8F8'), ToLong(Z'AF280416958F3ACB')]
    tLong,     PARAMETER    :: XXH3_Secret_Long5(0:7) = &
               [ToLong(Z'6DD4DE1CAD21F72C'), ToLong(Z'A44072DB979083E9'), &
                ToLong(Z'E679CB1F67B3B7A4'), ToLong(Z'D05A8278E5C0CC4E'), &
                ToLong(Z'4608B82172FFCC7D'), ToLong(Z'9035E08E2443F774'), &
                ToLong(Z'52283C4C263A81E6'), ToLong(Z'65D088CB00C391BB')]
    tLong,     PARAMETER    :: XXH3_Secret_Long6(0:7) = &
               [ToLong(Z'D9C97E9FC83159B4'), ToLong(Z'3483ACC5EA647378'), &
                ToLong(Z'FAFFA081C5C3EBD3'), ToLong(Z'B751DD0D17EB6313'), &
                ToLong(Z'29265516D349DAF0'), ToLong(Z'7D58BE162B9E68D4'), &
                ToLong(Z'7AD1B8F88FFCA147'), ToLong(Z'958F3ACB45CE31D0')]
    ! base secret parameters in integers used for short-message (1-16) optimization
    tInteger,  PARAMETER    :: XXH3_Secret_Integer(0:3) = &
               [ToInteger(Z'396CFEB8'), ToInteger(Z'BE4BA423'), &
                ToInteger(Z'2C81017C'), ToInteger(Z'1CAD21F7')]
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
    tIndex, PARAMETER   :: NbStripesPerBlock = (192-64)/8   ! 16
    tIndex, PARAMETER   :: Block_Len = 64*NbStripesPerBlock ! 1024

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE FUNCTION XX3_Hash64_Opt(Input, InpSize, StartHash, RemoveSign, Secret) RESULT(HashCode)

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

!** FLOW

    ! initialize
    SET_OPTION(Seed, 0_kInteger, StartHash)
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)

    ! perform hashing
    IF (PRESENT(Secret)) THEN
        IF (SIZE(Secret) >= 192) THEN
            HashCode = XX3_Hash64(InpPtr, Seed, Secret)
        ELSE
            BLOCK
                tByte       :: CustomSecret(192)
                tInteger    :: InSize
                InSize = SIZE(Secret)
                CustomSecret(1:InSize)  = Secret(1:InSize)
                CustomSecret(InSize+1:) = XXH3_Secret_Bytes(1:)
                HashCode = XX3_Hash64(InpPtr, Seed, CustomSecret)
            END BLOCK
        END IF
    ELSE
        HashCode = XX3_Hash64(InpPtr, Seed)
    END IF

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION XX3_Hash64_Opt

!******************************************************************************

FUNCTION XX3_Hash64(Input, Seed, CustomSecret) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the XX3Hash64 hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,           INTENT(IN) :: Input(0:)            ! input bytes
    tLong,           INTENT(IN) :: Seed                 ! seed
    tByte, OPTIONAL, INTENT(IN) :: CustomSecret(0:191)  ! custom secret
    tLong                       :: HashCode             ! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Length

!** FLOW

    Length = SIZE(Input)
    IF (Length <= 16) THEN
        HashCode = XX3Hash64_Len1To16(Input, Seed, Length, XXH3_Secret_Integer, &
                                      XXH3_Secret_Long1)
    ELSEIF (Length <= 128) THEN
        HashCode = XX3Hash64_Len17To128(Input, Seed, Length, XXH3_Secret_Long1)
    ELSEIF (Length <= 240) THEN
        HashCode = XX3Hash64_Len129To240(Input, Seed, Length, XXH3_Secret_Long1, &
                                         XXH3_Secret_Long2, XXH3_Secret_Long3)
    ELSE
        IF (PRESENT(CustomSecret)) THEN
            HashCode = XX3Hash_Len241Up_Custom(Input, Seed, Length, CustomSecret)
        ELSE
            HashCode = XX3Hash_Len241Up_Base(Input, Seed, Length, XXH3_Secret_Long1, &
                                             XXH3_Secret_Long4, XXH3_Secret_Long5,   &
                                             XXH3_Secret_Long6)
        END IF
    END IF

    RETURN

END FUNCTION XX3_Hash64

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                             +
!                           HASH FUNCTIONS - ENGINES                          +
!                                                                             +
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION XX3Hash64_Len1To16(Input, Seed, Length, IntSecret, &
                            LongSecret) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the XX3Hash64 hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)    :: Input(0:)            ! input bytes
    tLong,    INTENT(IN)    :: Seed                 ! seed
    tIndex,   INTENT(IN)    :: Length               ! length of input bytes
    tInteger, INTENT(IN)    :: IntSecret(0:3)       ! base secret in integers
    tLong,    INTENT(IN)    :: LongSecret(0:23)     ! base secret in longs
    tLong                   :: HashCode             ! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong           :: BitFlip1, BitFlip2, Input_Lo, Input_Hi, Acc
    tLong           :: S, Input1, Input2, BitFlip, Keyed
    tInteger        :: C1, C2, C3
    tLong           :: Combined
    ! variables used to quickly access input and secret bytes
    tByte           :: InpBytes(0:127)
    tLong           :: InpLongs(1:16)
    EQUIVALENCE(InpBytes, InpLongs)
    tByte           :: Bytes(0:7)
    tInteger        :: I32Val(2)
    EQUIVALENCE(Bytes, I32Val)

!** FLOW

    ! XXH3_Len_0to16_64b
    IF (Length > 8) THEN
        ! XXH3_Len_9to16_64b
        BitFlip1 = IEOR(LongSecret(3), LongSecret(4)) + Seed
        BitFlip2 = IEOR(LongSecret(5), LongSecret(6)) - Seed
        InpBytes(0:7) = Input(0:7)
        InpBytes(8:15) = Input(Length-8:Length-1)
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
        Bytes(0:3) = Input(0:3)
        Bytes(4:7) = Input(Length-4:Length-1)
        Input1 = ToLong(I32Val(1))      ! high int will be shifted
        Input2 = MaskI32(I32Val(2))
        BitFlip = IEOR(LongSecret(1), LongSecret(2)) - S
        Keyed = IEOR((Input2 + SHIFTL(Input1, 32)), BitFlip)
        HashCode = XXH3_RRMxMx(Keyed, Length)
        RETURN
    END IF
    IF (Length /= 0) THEN
        ! XXH3_Len_1to3_64b
        C1 = MaskI8(Input(0))
        C2 = ToInteger(Input(SHIFTA(Length, 1)))   ! high 3 bytes will be shifted
        C3 = MaskI8(Input(Length - 1))
        Combined = UnsignedInt(IOR(IOR(IOR(SHIFTL(C1, 16), SHIFTL(C2, 24)), C3), \
                                SHIFTL(Length, 8)))
        BitFlip = UnsignedInt(IEOR(IntSecret(0), IntSecret(1))) + Seed
        HashCode = IEOR(Combined, BitFlip)
        HashCode = XXH64_Avalanche(HashCode)
        RETURN
    END IF
    HashCode = IEOR(IEOR(Seed, LongSecret(7)), LongSecret(8))
    HashCode = XXH64_Avalanche(HashCode)

    RETURN

END FUNCTION XX3Hash64_Len1To16

!***************************************************************************

FUNCTION XX3Hash64_Len17To128(Input, Seed, Length, LongSecret) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the XX3Hash64 hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,  INTENT(IN)  :: Input(0:)            ! input bytes
    tLong,  INTENT(IN)  :: Seed                 ! seed
    tIndex, INTENT(IN)  :: Length               ! length of input bytes
    tLong,  INTENT(IN)  :: LongSecret(0:23)     ! base secret
    tLong               :: HashCode             ! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong           :: Acc
    ! variables used to quickly access input and secret bytes
    tByte           :: InpBytes(0:127)
    tLong           :: InpLongs(1:16)
    EQUIVALENCE(InpBytes, InpLongs)

!** FLOW

    ! XXH3_Len_17to128_64b
    Acc = ToLong(Length)*XXH_PRIME64_1
    IF (Length > 32) THEN
        IF (Length > 64) THEN
            IF (Length > 96) THEN
                InpBytes(0:63)   = Input(0:63)
                InpBytes(64:127) = Input(Length-64:Length-1)
                Acc = Acc + XXH3_Mix16B(Seed, InpLongs, 7, LongSecret, 12)
                Acc = Acc + XXH3_Mix16B(Seed, InpLongs, 9, LongSecret, 14)
            ELSE
                InpBytes(0:47)   = Input(0:47)
                InpBytes(80:127) = Input(Length-48:Length-1)
            END IF
            Acc = Acc + XXH3_Mix16B(Seed, InpLongs,  5, LongSecret, 8)
            Acc = Acc + XXH3_Mix16B(Seed, InpLongs, 11, LongSecret, 10)
        ELSE
            InpBytes(0:31)   = Input(0:31)
            InpBytes(96:127) = Input(Length-32:Length-1)
        END IF
        Acc = Acc + XXH3_Mix16B(Seed, InpLongs,  3, LongSecret, 4)
        Acc = Acc + XXH3_Mix16B(Seed, InpLongs, 13, LongSecret, 6)
    ELSE
        InpBytes(0:15)    = Input(0:15)
        InpBytes(112:127) = Input(Length-16:Length-1)
    END IF
    Acc = Acc + XXH3_Mix16B(Seed, InpLongs,  1, LongSecret, 0)
    Acc = Acc + XXH3_Mix16B(Seed, InpLongs, 15, LongSecret, 2)
    HashCode = XXH3_Avalanche(Acc)

    RETURN

END FUNCTION XX3Hash64_Len17To128

!***************************************************************************

FUNCTION XX3Hash64_Len129To240(Input, Seed, Length, LongSecret1, &
                               LongSecret2, LongSecret3) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the XX3Hash64 hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,  INTENT(IN)  :: Input(0:)            ! input bytes
    tLong,  INTENT(IN)  :: Seed                 ! seed
    tIndex, INTENT(IN)  :: Length               ! length of input bytes
    tLong,  INTENT(IN)  :: LongSecret1(0:23)    ! base secret
    tLong,  INTENT(IN)  :: LongSecret2(0:13)    ! base secret
    tLong,  INTENT(IN)  :: LongSecret3(0:3)     ! base secret
    tLong               :: HashCode             ! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong           :: Acc
    tIndex          :: NbRounds, I
    ! variables used to quickly access input and secret bytes
    tByte           :: InpBytes(0:127)
    tLong           :: InpLongs(1:16)
    EQUIVALENCE(InpBytes, InpLongs)

!** FLOW

    ! XXH3_Len_129to240_64b
    Acc = ToLong(Length)*XXH_PRIME64_1
    NbRounds = Length / 16
    I = 0
    InpBytes(0:127) = Input(0:127)
    DO WHILE (I < 8)
        Acc = Acc + XXH3_Mix16B(Seed, InpLongs, 2*I+1, LongSecret1, 2*I)
        I = I + 1
    END DO
    Acc = XXH3_Avalanche(Acc)
    InpBytes(0:(NbRounds*2-16)*8-1) = Input(128:128+(NbRounds*2-16)*8-1)
    DO WHILE (I < NbRounds)
        Acc = Acc + XXH3_Mix16B(Seed, InpLongs, 2*(I-8)+1, LongSecret2, 2*(I-8))
        I = I + 1
    END DO
    ! last bytes
    InpBytes(0:15) = Input(Length-16:Length-1)
    Acc = Acc + XXH3_Mix16B(Seed, InpLongs, 1, LongSecret3, 2)
    HashCode = XXH3_Avalanche(Acc)

    RETURN

END FUNCTION XX3Hash64_Len129To240

!******************************************************************************

FUNCTION XX3Hash_Len241Up_Base(Input, Seed, Length, Secret1, Secret2, &
                               Secret3, Secret4, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the XX3Hash64 hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,           INTENT(IN)     :: Input(0:)        ! input bytes
    tLong,           INTENT(IN)     :: Seed             ! seed
    tIndex,          INTENT(IN)     :: Length           ! length of input bytes
    tLong,           INTENT(IN)     :: Secret1(0:23)    ! base secret
    tLong,           INTENT(IN)     :: Secret2(0:7)     ! base secret
    tLong,           INTENT(IN)     :: Secret3(0:7)     ! base secret
    tLong,           INTENT(IN)     :: Secret4(0:7)     ! base secret
    tLong, OPTIONAL, INTENT(OUT)    :: HashPair(2)      ! dual hash code
    tLong                           :: HashCode         ! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: Offset
    tLong           :: Low, High
    tLong           :: Acc(0:7)
    tIndex          :: Nb_Blocks, OffBlock, OffStripe, OffSec, NbStripes
    tIndex          :: N, S, J
    tLong           :: DataVal0, DataVal1, DataKey0, DataKey1
    ! variables used to quickly access input and secret bytes
    tByte           :: InpBytes(0:63)
    tLong           :: InpLongs(1:8)
    EQUIVALENCE(InpBytes, InpLongs)

!** FLOW

    ! initialize
    Offset = 0

    ! XXH3_hashLong_64b_internal
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
            OffSec = S
            InpBytes(0:63) = Input(OffStripe:OffStripe+63)
            DO J = 0, 7, 2
                DataVal0 = InpLongs(J+1)
                DataVal1 = InpLongs(J+2)
                DataKey0 = IEOR(DataVal0, Secret1(Offsec+J))
                DataKey1 = IEOR(DataVal1, Secret1(Offsec+J+1))
                ! swap adjacent lanes
                Acc(J)   = Acc(J)   + DataVal1 + IAND(MaxU32, DataKey0)*SHIFTR(DataKey0, 32)
                Acc(J+1) = Acc(J+1) + DataVal0 + IAND(MaxU32, DataKey1)*SHIFTR(DataKey1, 32)
            END DO
        END DO
        ! XXH3_scrambleAcc_scalar
        OffSec = 16
        DO J = 0, 7
            Acc(J) = (IEOR(IEOR(Acc(J), SHIFTR(Acc(J), 47)),  &
                                Secret1(Offsec+J)))*XXH_PRIME32_1
        END DO
    END DO

    ! last partial block
    NbStripes = ((Length - 1) - (Block_Len*Nb_Blocks)) / 64
    OffBlock = Offset + Block_Len*Nb_Blocks
    DO S = 0, NbStripes - 1
        ! XXH3_accumulate_512
        OffStripe = OffBlock + S*64
        OffSec = S
        InpBytes(0:63) = Input(OffStripe:OffStripe+63)
        DO J = 0, 7, 2
            DataVal0 = InpLongs(J+1)
            DataVal1 = InpLongs(J+2)
            DataKey0 = IEOR(DataVal0, Secret1(Offsec+J))
            DataKey1 = IEOR(DataVal1, Secret1(Offsec+J+1))
            ! swap adjacent lanes
            Acc(J)   = Acc(J)   + DataVal1 + IAND(MaxU32, DataKey0)*SHIFTR(DataKey0, 32)
            Acc(J+1) = Acc(J+1) + DataVal0 + IAND(MaxU32, DataKey1)*SHIFTR(DataKey1, 32)
        END DO
    END DO

    ! last stripe
    ! XXH3_accumulate_512
    OffStripe = Offset + Length - 64
    InpBytes(0:63) = Input(OffStripe:OffStripe+63)
    DO J = 0, 7, 2
        DataVal0 = InpLongs(J+1)
        DataVal1 = InpLongs(J+2)
        DataKey0 = IEOR(DataVal0, Secret2(J))
        DataKey1 = IEOR(DataVal1, Secret2(J+1))
        ! swap adjacent lanes
        Acc(J)   = Acc(J)   + DataVal1 + IAND(MaxU32, DataKey0)*SHIFTR(DataKey0, 32)
        Acc(J+1) = Acc(J+1) + DataVal0 + IAND(MaxU32, DataKey1)*SHIFTR(DataKey1, 32)
    END DO

    ! XXH3_mergeAccs
    Low = ToLong(Length)*XXH_PRIME64_1                      &
                + XXH3_Mix2Accs(Acc(0), Acc(1), Secret3, 0) &
                + XXH3_Mix2Accs(Acc(2), Acc(3), Secret3, 2) &
                + XXH3_Mix2Accs(Acc(4), Acc(5), Secret3, 4) &
                + XXH3_Mix2Accs(Acc(6), Acc(7), Secret3, 6)
    Low = XXH3_Avalanche(Low)
    IF (PRESENT(HashPair)) THEN
        High = NOT(ToLong(Length)*XXH_PRIME64_2)                &
                    + XXH3_Mix2Accs(Acc(0), Acc(1), Secret4, 0) &
                    + XXH3_Mix2Accs(Acc(2), Acc(3), Secret4, 2) &
                    + XXH3_Mix2Accs(Acc(4), Acc(5), Secret4, 4) &
                    + XXH3_Mix2Accs(Acc(6), Acc(7), Secret4, 6)
        High = XXH3_Avalanche(High)
        HashPair(1) = Low
        HashPair(2) = High
    END IF
    HashCode = Low

    ASSOCIATE(Dummy => Seed); END ASSOCIATE

    RETURN

END FUNCTION XX3Hash_Len241Up_Base

!******************************************************************************

FUNCTION XX3Hash_Len241Up_Custom(Input, Seed, Length, Secret, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the XX3Hash64 hash algorithm by Yann Collet.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,           INTENT(IN)     :: Input(0:)        ! input bytes
    tLong,           INTENT(IN)     :: Seed             ! seed
    tIndex,          INTENT(IN)     :: Length           ! length of input bytes
    tByte,           INTENT(IN)     :: Secret(0:191)    ! custom secret
    tLong, OPTIONAL, INTENT(OUT)    :: HashPair(2)      ! dual hash code
    tLong                           :: HashCode         ! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: Offset
    tLong           :: Low, High
    tLong           :: Acc(0:7)
    tIndex          :: Nb_Blocks, OffBlock, OffStripe, OffSec, NbStripes
    tIndex          :: N, S, J
    tLong           :: DataVal0, DataVal1, DataKey0, DataKey1
    ! variables used to quickly access input and secret bytes
    tByte           :: InpBytes(0:63)
    tLong           :: InpLongs(1:8)
    EQUIVALENCE(InpBytes, InpLongs)
    tByte           :: SecBytes(0:127)
    tLong           :: SecLongs(1:16)
    EQUIVALENCE(SecBytes, SecLongs)

!** FLOW

    ! initialize
    Offset = 0

    ! XXH3_hashLong_64b_internal
    ! initialize
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
            SecBytes(0:63) = Secret(OffSec:OffSec+63)
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
        OffSec = 128    ! 192 - 64
        SecBytes(0:63) = Secret(128:191)
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
        SecBytes(0:63) = Secret(OffSec:OffSec+63)
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
    OffSec = 121    ! 192 - 64 - 7
    InpBytes(0:63) = Input(OffStripe:OffStripe+63)
    SecBytes(0:63) = Secret(121:184)
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
    SecBytes(0:63) = Secret(11:74)
    Low = ToLong(Length)*XXH_PRIME64_1                       &
                + XXH3_Mix2Accs(Acc(0), Acc(1), SecLongs, 1) &
                + XXH3_Mix2Accs(Acc(2), Acc(3), SecLongs, 3) &
                + XXH3_Mix2Accs(Acc(4), Acc(5), SecLongs, 5) &
                + XXH3_Mix2Accs(Acc(6), Acc(7), SecLongs, 7)

    Low = XXH3_Avalanche(Low)
    IF (PRESENT(HashPair)) THEN
        SecBytes(0:63) = Secret(117:180)
        High = NOT(ToLong(Length)*XXH_PRIME64_2)                 &
                    + XXH3_Mix2Accs(Acc(0), Acc(1), SecLongs, 1) &
                    + XXH3_Mix2Accs(Acc(2), Acc(3), SecLongs, 3) &
                    + XXH3_Mix2Accs(Acc(4), Acc(5), SecLongs, 5) &
                    + XXH3_Mix2Accs(Acc(6), Acc(7), SecLongs, 7)
        High = XXH3_Avalanche(High)
        HashPair(1) = Low
        HashPair(2) = High
    END IF
    HashCode = Low

    ASSOCIATE(Dummy => Seed); END ASSOCIATE

    RETURN

END FUNCTION XX3Hash_Len241Up_Custom

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                             +
!                   SUPPORTING ROUTINES FOR XXHASH ALGORITHMS                 +
!                                                                             +
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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

END SUBMODULE SubBase_XX3Hash64_Opt

!******************************************************************************

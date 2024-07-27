
SUBMODULE (ModBase_ReferenceHash64) SubBase_KomiHash_Ref

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains a reference implementation for the KomiHash hash algorithm
!   by Aleksey Vaneev. <br>

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

MODULE FUNCTION KomiHash_I64(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the KomiHash hash algorithm by Aleksey Vaneev.
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
    HashCode = Komi_Hash64(InpPtr, Seed, ByteConv)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION KomiHash_I64

!******************************************************************************

FUNCTION Komi_Hash64(Input, Seed, BC) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the KomiHash hash algorithm by Aleksey Vaneev.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN) :: Input(0:)    !! input bytes
    tLong,               INTENT(IN) :: Seed         !! seed
    TYPE(ByteConverter), INTENT(IN) :: BC           !! byte converter
    tLong                           :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Seed1, Seed2, Seed3, Seed4
    tLong       :: Seed5, Seed6, Seed7, Seed8
    tLong       :: R1L, R1H, R2L, R2H
    tLong       :: R3L, R3H, R4L, R4H
    tLong       :: FByte, Inp64
    tIndex      :: Length, BLShift, Remaining, Offset

!** SUBROUTINE MACRO DEFINITIONS:
#define MaskI32(X)              IAND(ToLong(X), Z'00000000FFFFFFFF')
#define KH_LU32EC(Buf, Off)     BC%Pack_I32(Buf, Off)
#define KH_LU64EC(Buf, Off)     BC%Pack_I64(Buf, Off)
#define KH_LPU64EC_L3(Buf, Off, BLen, FByte, Inp) \
    BLShift = SHIFTL(BLen, 3); \
    IF (BLen < 4) THEN; \
        Inp = IOR(SHIFTL(FByte, BLShift), \
                  SHIFTR(BC%Pack_I64_Partial(Buf, Off+BLen-3, 3), (24 - BLShift))); \
    ELSE; \
        Inp = IOR(IOR(SHIFTL(FByte, BLShift), MaskI32(KH_LU32EC(Buf, Off))), \
                      SHIFTL(SHIFTR(MaskI32(KH_LU32EC(Buf, Off+BLen-4)), (64 - BLShift)), 32)); \
    END IF;
#define KH_LPU64EC_NZ(Buf, Off, BLen, FByte, Inp) \
    BLShift = SHIFTL(BLen, 3); \
    IF (BLen < 4) THEN; \
        Inp = IOR(SHIFTL(FByte, BLShift), BC%Pack_I64_Partial(Buf, Off, BLen)); \
    ELSE; \
        Inp = IOR(IOR(SHIFTL(FByte, BLShift), MaskI32(KH_LU32EC(Buf, Off))), \
                      SHIFTL(SHIFTR(MaskI32(KH_LU32EC(Buf, Off+BLen-4)), (64 - BLShift)), 32)); \
    END IF;
#define KH_LPU64EC_L4(Buf, Off, BLen, FByte, Inp) \
    BLShift = SHIFTL(BLen, 3); \
    IF (BLen < 5) THEN; \
        Inp = IOR(SHIFTL(FByte, BLShift), \
                  SHIFTR(MaskI32(KH_LU32EC(Buf, Off+BLen-4)), (32 - BLShift))); \
    ELSE; \
        Inp = IOR(SHIFTL(FByte, BLShift), \
                  SHIFTR(KH_LU64EC(Buf, Off+BLen-8), (64 - BLShift))); \
    END IF;
#define KOMIHASH_HASH16(Buf, Off) \
    CALL KH_M128(IEOR(Seed1, KH_LU64EC(Buf, Off)), \
                 IEOR(Seed5, KH_LU64EC(Buf, Off+8)), R1L, R1H); \
    Seed5 = Seed5 + R1H; \
    Seed1 = IEOR(Seed5, R1L);
#define KOMIHASH_HASHROUND() \
    CALL KH_M128(Seed1, Seed5, R2L, R2H); \
    Seed5 = Seed5 + R2H; \
    Seed1 = IEOR(Seed5, R2L);
#define KOMIHASH_HASHFINAL() \
    CALL KH_M128(R2L, R2H, R1L, R1H); \
    Seed5 = Seed5 + R1H; \
    Seed1 = IEOR(Seed5, R1L); \
    KOMIHASH_HASHROUND();

!** FLOW

    ! initialize
    Length = SIZE(Input)
    FByte = SHIFTL(1, SHIFTR(Input(Length-1), 7))

    ! The seeds are initialized to the first mantissa bits of PI.
    Seed1 = IEOR(Z'243F6A8885A308D3', IAND(Seed, Z'5555555555555555'))
    Seed5 = IEOR(Z'452821E638D01377', IAND(Seed, Z'AAAAAAAAAAAAAAAA'))

    ! The three instructions in the "KOMIHASH_HASHROUND" macro represent the
    ! simplest constant-less PRNG, scalable to any even-sized state
    ! variables, with the `Seed1` being the PRNG output (2^64 PRNG period).
    ! It passes `PractRand` tests with rare non-systematic "unusual"
    ! evaluations.
    !
    ! To make this PRNG reliable, self-starting, and eliminate a risk of
    ! stopping, the following variant can be used, which is a "register
    ! checker-board", a source of raw entropy. The PRNG is available as the
    ! komirand() function. Not required for hashing (but works for it) since
    ! the input entropy is usually available in abundance during hashing.
    !
    ! Seed5 += R2H + 0xAAAAAAAAAAAAAAAA
    !
    ! (the `0xAAAA...` constant should match register's size essentially,
    ! it is a replication of the `10` bit-pair it is not an arbitrary
    ! constant).

    KOMIHASH_HASHROUND() ! Required for PerlinNoise.

    IF (Length < 16) THEN
        R2L = Seed1
        R2H = Seed5
        IF (Length > 7) THEN
            ! The following two XOR instructions are equivalent to mixing a
            ! message with a cryptographic one-time-pad (bitwise modulo 2
            ! addition). Message's statistics and distribution are thus
            ! unimportant.
            KH_LPU64EC_L3(Input, 8, Length-8, FByte, Inp64)
            R2H = IEOR(R2H, Inp64)
            R2L = IEOR(R2L, KH_LU64EC(Input, 0))
        ELSEIF (Length /= 0) THEN
            KH_LPU64EC_NZ(Input, 0, Length, FByte, Inp64)
            R2L = IEOR(R2L, Inp64)
        END IF
        KOMIHASH_HASHFINAL()
        HashCode = Seed1
        RETURN
    END IF

    IF (Length  < 32) THEN
        KOMIHASH_HASH16(Input, 0_kIndex)
        IF (Length  > 23) THEN
            KH_LPU64EC_L4(Input, 24, Length-24, FByte, Inp64)
            R2H = IEOR(Seed5, Inp64)
            R2L = IEOR(Seed1, KH_LU64EC(Input, 16))
        ELSE
            KH_LPU64EC_L4(Input, 16, Length-16, FByte, Inp64)
            R2L = IEOR(Seed1, Inp64)
            R2H = Seed5
        END IF
        KOMIHASH_HASHFINAL()
        HashCode = Seed1
        RETURN
    END IF

    Remaining = Length
    Offset = 0

    IF (Length >= 64) THEN
        ! initialize
        Seed2 = IEOR(Z'13198A2E03707344', Seed1)
        Seed3 = IEOR(Z'A4093822299F31D0', Seed1)
        Seed4 = IEOR(Z'082EFA98EC4E6C89', Seed1)
        Seed6 = IEOR(Z'BE5466CF34E90C6C', Seed5)
        Seed7 = IEOR(Z'C0AC29B7C97C50DD', Seed5)
        Seed8 = IEOR(Z'3F84D5B5B5470917', Seed5)
        DO
            CALL KH_M128(IEOR(Seed1, KH_LU64EC(Input, Offset)), &
                         IEOR(Seed5, KH_LU64EC(Input, Offset+8)), R1L, R1H)

            CALL KH_M128(IEOR(Seed2, KH_LU64EC(Input, Offset+16)), &
                         IEOR(Seed6, KH_LU64EC(Input, Offset+24)), R2L, R2H)

            CALL KH_M128(IEOR(Seed3, KH_LU64EC(Input, Offset+32)), &
                         IEOR(Seed7, KH_LU64EC(Input, Offset+40)), R3L, R3H)

            CALL KH_M128(IEOR(Seed4, KH_LU64EC(Input, Offset+48)), &
                         IEOR(Seed8, KH_LU64EC(Input, Offset+56)), R4L, R4H)
            ! Such "shifting" arrangement (below) does not increase
            ! individual SeedN's PRNG period beyond 2^64, but reduces a
            ! chance of any occassional synchronization between PRNG lanes
            ! happening. Practically, Seed1-4 together become a single
            ! "fused" 256-bit PRNG value, having a summary PRNG period of
            ! 2^66.
            Seed5 = Seed5 + R1H
            Seed6 = Seed6 + R2H
            Seed7 = Seed7 + R3H
            Seed8 = Seed8 + R4H
            Seed2 = IEOR(Seed5, R2L)
            Seed3 = IEOR(Seed6, R3L)
            Seed4 = IEOR(Seed7, R4L)
            Seed1 = IEOR(Seed8, R1L)
            ! update indices
            Offset = Offset + 64
            Remaining = Remaining - 64
            IF (Remaining < 64) EXIT
        END DO
        Seed5 = IEOR(Seed5, IEOR(IEOR(Seed6, Seed7), Seed8))
        Seed1 = IEOR(Seed1, IEOR(IEOR(Seed2, Seed3), Seed4))
    END IF

    IF (Remaining > 31) THEN
        KOMIHASH_HASH16(Input, Offset)
        KOMIHASH_HASH16(Input, Offset+16)
        ! update indices
        Offset = Offset + 32
        Remaining = Remaining - 32
    END IF

    IF (Remaining > 15) THEN
        KOMIHASH_HASH16(Input, Offset)
        ! update indices
        Offset = Offset + 16
        Remaining = Remaining - 16
    END IF

    IF (Remaining > 7) THEN
        KH_LPU64EC_L4(Input, Offset+8, Remaining-8, FByte, Inp64)
        R2H = IEOR(Seed5, Inp64)
        R2L = IEOR(Seed1, KH_LU64EC(Input, Offset))
    ELSE
        KH_LPU64EC_L4(Input, Offset, Remaining, FByte, Inp64)
        R2L = IEOR(Seed1, Inp64)
        R2H = Seed5
    END IF

    KOMIHASH_HASHFINAL()
    HashCode = Seed1

    RETURN

CONTAINS

    SUBROUTINE KH_M128(LHS, RHS, ResLo, ResHi)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute 128-bit result of multiplication of two 64-bit unsigned integers.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(IN)   :: LHS
        tLong, INTENT(IN)   :: RHS
        tLong, INTENT(OUT)  :: ResLo
        tLong, INTENT(OUT)  :: ResHi

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong   :: LHS_Lo, LHS_Hi, RHS_Lo, RHS_Hi
        tLong   :: Lo_Lo, Hi_Lo
        tLong   :: Cross

    !** FLOW

        ! the Grade School method of multiplication.
        LHS_Lo = MaskI32(LHS)
        LHS_Hi = SHIFTR(LHS, 32)
        RHS_Lo = MaskI32(RHS)
        RHS_Hi = SHIFTR(RHS, 32)
        Lo_Lo = LHS_Lo*RHS_Lo
        Hi_Lo = LHS_Hi*RHS_Lo

        ! Add the products together. This will never overfLow.
        Cross = SHIFTR(Lo_Lo, 32) + MaskI32(Hi_Lo) + LHS_Lo*RHS_Hi
        ResLo = IOR(SHIFTL(Cross, 32), MaskI32(Lo_Lo))
        ResHi = SHIFTR(Hi_Lo, 32) + SHIFTR(Cross, 32) + LHS_Hi*RHS_Hi

        RETURN

    END SUBROUTINE KH_M128

    !**************************************************************************

#undef MaskI32
#undef KH_LU32EC
#undef KH_LU64EC
#undef KH_LPU64EC_L3
#undef KH_LPU64EC_NZ
#undef KH_LPU64EC_L4
#undef KOMIHASH_HASH16

END FUNCTION Komi_Hash64

!******************************************************************************

END SUBMODULE SubBase_KomiHash_Ref

!******************************************************************************

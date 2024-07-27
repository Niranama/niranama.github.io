
SUBMODULE (ModBase_OptimalHash64) SubBase_KomiHash_Opt

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an optimal implementation for the KomiHash hash algorithm
!   by Aleksey Vaneev. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define MaskI32(X)              IAND(ToLong(X), Z'00000000FFFFFFFF')
#define KOMIHASH_HASHROUND() \
    CALL KH_M128(Seed1, Seed5, R2L, R2H); \
    Seed5 = Seed5 + R2H; \
    Seed1 = IEOR(Seed5, R2L);
#define KOMIHASH_HASHFINAL() \
    CALL KH_M128(R2L, R2H, R1L, R1H); \
    Seed5 = Seed5 + R1H; \
    Seed1 = IEOR(Seed5, R1L); \
    KOMIHASH_HASHROUND();

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

MODULE FUNCTION Komi_Hash64_Opt(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

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

!** FLOW

    ! initialize
    SET_OPTION(Seed, 0_kInteger, StartHash)
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)

    ! perform hashing
    HashCode = Komi_Hash64(InpPtr, Seed)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION Komi_Hash64_Opt

!******************************************************************************

FUNCTION Komi_Hash64(Input, Seed) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the KomiHash hash algorithm by Aleksey Vaneev.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, INTENT(IN)   :: Input(0:)    !! input bytes
    tLong, INTENT(IN)   :: Seed         !! seed
    tLong               :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong           :: Seed1, Seed2, Seed3, Seed4
    tLong           :: Seed5, Seed6, Seed7, Seed8
    tLong           :: R1L, R1H, R2L, R2H
    tLong           :: R3L, R3H, R4L, R4H
    tLong           :: FByte, Inp64
    tIndex          :: Length, BLShift, Remaining, Offset
    ! variables used to quickly access input and secret bytes
    tByte           :: I64Bytes(0:63)
    tLong           :: I64Val(1:8)
    EQUIVALENCE(I64Bytes, I64Val)
    tByte           :: I32Bytes(0:7)
    tInteger        :: I32Val(2)
    EQUIVALENCE(I32Bytes, I32Val)

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
            BLShift = SHIFTL(Length-8, 3)
            IF (Length < 12) THEN
                I64Bytes(0:2) = Input(Length-3:Length-1)
                I64Bytes(3:7) = 0_kByte
                Inp64 = IOR(SHIFTL(FByte, BLShift), SHIFTR(I64Val(1), (24 - BLShift)))
            ELSE
                I32Bytes(0:3) = Input(8:11)
                I32Bytes(4:7) = Input(Length-4:Length-1)
                Inp64 = IOR(IOR(SHIFTL(FByte, BLShift), MaskI32(I32Val(1))), &
                                SHIFTL(SHIFTR(MaskI32(I32Val(2)), (64 - BLShift)), 32))
            END IF
            R2H = IEOR(R2H, Inp64)
            I64Bytes(0:7) = Input(0:7)
            R2L = IEOR(R2L, I64Val(1))
        ELSEIF (Length /= 0) THEN
            BLShift = SHIFTL(Length, 3)
            IF (Length < 4) THEN
                I64Bytes(0:Length-1) = Input(0:Length-1)
                I64Bytes(Length:7)   = 0_kByte
                Inp64 = IOR(SHIFTL(FByte, BLShift), I64Val(1))
            ELSE
                I32Bytes(0:3) = Input(0:3)
                I32Bytes(4:7) = Input(Length-4:Length-1)
                Inp64 = IOR(IOR(SHIFTL(FByte, BLShift), MaskI32(I32Val(1))), &
                                SHIFTL(SHIFTR(MaskI32(I32Val(2)), (64 - BLShift)), 32))
            END IF
            R2L = IEOR(R2L, Inp64)
        END IF
        KOMIHASH_HASHFINAL()
        HashCode = Seed1
        RETURN
    END IF

    IF (Length  < 32) THEN
        I64Bytes(0:15) = Input(0:15)
        CALL KH_M128(IEOR(Seed1, I64Val(1)), IEOR(Seed5, I64Val(2)), R1L, R1H)
        Seed5 = Seed5 + R1H
        Seed1 = IEOR(Seed5, R1L)
        IF (Length  > 23) THEN
            BLShift = SHIFTL(Length-24, 3)
            IF (Length < 29) THEN
                I32Bytes(0:3) = Input(Length-4:Length-1)
                Inp64 = IOR(SHIFTL(FByte, BLShift), SHIFTR(MaskI32(I32Val(1)), (32 - BLShift)))
            ELSE
                I64Bytes(0:7) = Input(Length-8:Length-1)
                Inp64 = IOR(SHIFTL(FByte, BLShift), SHIFTR(I64Val(1), (64 - BLShift)))
            END IF
            R2H = IEOR(Seed5, Inp64)
            I64Bytes(0:7) = Input(16:23)
            R2L = IEOR(Seed1, I64Val(1))
        ELSE
            BLShift = SHIFTL(Length-16, 3)
            IF (Length < 21) THEN
                I32Bytes(0:3) = Input(Length-4:Length-1)
                Inp64 = IOR(SHIFTL(FByte, BLShift), SHIFTR(MaskI32(I32Val(1)), (32 - BLShift)))
            ELSE
                I64Bytes(0:7) = Input(Length-8:Length-1)
                Inp64 = IOR(SHIFTL(FByte, BLShift), SHIFTR(I64Val(1), (64 - BLShift)))
            END IF
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
            I64Bytes(0:63) = Input(Offset:Offset+63)
            CALL KH_M128(IEOR(Seed1, I64Val(1)), IEOR(Seed5, I64Val(2)), R1L, R1H)
            CALL KH_M128(IEOR(Seed2, I64Val(3)), IEOR(Seed6, I64Val(4)), R2L, R2H)
            CALL KH_M128(IEOR(Seed3, I64Val(5)), IEOR(Seed7, I64Val(6)), R3L, R3H)
            CALL KH_M128(IEOR(Seed4, I64Val(7)), IEOR(Seed8, I64Val(8)), R4L, R4H)
                
            ! Such "shifting" arrangement (below) does not increase
            ! individual SeedN's PRNG period beyond 2^64, but reduces a
            ! chance of any occasional synchronization between PRNG lanes
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
        I64Bytes(0:15) = Input(Offset:Offset+15)
        CALL KH_M128(IEOR(Seed1, I64Val(1)), IEOR(Seed5, I64Val(2)), R1L, R1H)
        Seed5 = Seed5 + R1H
        Seed1 = IEOR(Seed5, R1L)
        I64Bytes(16:31) = Input(Offset+16:Offset+31)
        CALL KH_M128(IEOR(Seed1, I64Val(3)), IEOR(Seed5, I64Val(4)), R1L, R1H)
        Seed5 = Seed5 + R1H
        Seed1 = IEOR(Seed5, R1L)
        ! update indices
        Offset = Offset + 32
        Remaining = Remaining - 32
    END IF

    IF (Remaining > 15) THEN
        I64Bytes(0:15) = Input(Offset:Offset+15)
        CALL KH_M128(IEOR(Seed1, I64Val(1)), IEOR(Seed5, I64Val(2)), R1L, R1H)
        Seed5 = Seed5 + R1H
        Seed1 = IEOR(Seed5, R1L)
        ! update indices
        Offset = Offset + 16
        Remaining = Remaining - 16
    END IF

    IF (Remaining > 7) THEN
        BLShift = SHIFTL(Remaining-8, 3)
        IF (Remaining < 13) THEN
            I32Bytes(0:3) = Input(Offset+Remaining-4:Offset+Remaining-1)
            Inp64 = IOR(SHIFTL(FByte, BLShift), SHIFTR(MaskI32(I32Val(1)), (32 - BLShift)))
        ELSE
            I64Bytes(0:7) = Input(Offset+Remaining-8:Offset+Remaining-1)
            Inp64 = IOR(SHIFTL(FByte, BLShift), SHIFTR(I64Val(1), (64 - BLShift)))
        END IF
        R2H = IEOR(Seed5, Inp64)
        I64Bytes(0:7) = Input(Offset:Offset+7)
        R2L = IEOR(Seed1, I64Val(1))
    ELSE
        BLShift = SHIFTL(Remaining, 3)
        IF (Remaining < 5) THEN
            I32Bytes(0:3) = Input(Offset+Remaining-4:Offset+Remaining-1)
            Inp64 = IOR(SHIFTL(FByte, BLShift), SHIFTR(MaskI32(I32Val(1)), (32 - BLShift)))
        ELSE
            I64Bytes(0:7) = Input(Offset+Remaining-8:Offset+Remaining-1)
            Inp64 = IOR(SHIFTL(FByte, BLShift), SHIFTR(I64Val(1), (64 - BLShift)))
        END IF
        R2L = IEOR(Seed1, Inp64)
        R2H = Seed5
    END IF

    KOMIHASH_HASHFINAL()
    HashCode = Seed1
        
    RETURN

END FUNCTION Komi_Hash64

!******************************************************************************

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

!******************************************************************************

END SUBMODULE SubBase_KomiHash_Opt

!******************************************************************************

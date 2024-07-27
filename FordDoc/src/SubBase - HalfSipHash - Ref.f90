
SUBMODULE (ModBase_ReferenceHash32) SubBase_HalfSipHash_Ref

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains a reference implementation for the HalfSipHash hash algorithm
!   by Jean-Philippe Aumasson. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,       ONLY: AnyType_2_ByteArrPtr
    USE ModBase_PrgnBuilder,    ONLY: Jsf32RNG
    USE Class_ByteConverter
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define MaskI32(X)      IAND(ToLong(X), ToLong(Z'00000000FFFFFFFF'))

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

MODULE FUNCTION HalfSipHash24_I32(Input, InpSize, Key, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the HalfSipHash24 hash algorithm by Jean-Philippe
    !  Aumasson.  This is an interface routine that calls the working routine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), DIMENSION(..), INTENT(IN)  :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
    tByte,                  INTENT(IN)  :: Key(:)       !! key (at least 8 bytes)
    tInteger, OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
    tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tInteger                            :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER      :: InpPtr(:)
    tInteger            :: Seed
    TYPE(ByteConverter) :: ByteConv

!** FLOW

    ! initialize
    SET_OPTION(Seed, 0_kInteger, StartHash)
    CALL ByteConv%Initialize()
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)

    ! perform hashing
    IF (SIZE(Key) >= 8) THEN
        HashCode = HalfSip24_Hash32(InpPtr, Seed, Key, ByteConv)
    ELSE
        BLOCK
            tInteger        :: KeySize, I
            tByte           :: GenKey(8)
            TYPE(Jsf32RNG)  :: RNG
            CALL RNG%Initialize()
            KeySize = SIZE(Key)
            GenKey(1:KeySize) = Key(1:KeySize)
            DO I = KeySize+1, 8
                GenKey(I) = RNG%NextByte()
            END DO
            HashCode = HalfSip24_Hash32(InpPtr, Seed, GenKey, ByteConv)
        END BLOCK
    END IF

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION HalfSipHash24_I32

!******************************************************************************

MODULE FUNCTION HalfSipHash_I32(Input, InpSize, Key, cRound, dRound, &
                                StartHash, RemoveSign, HashLong) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the HalfSipHash hash algorithm by Jean-Philippe
    !  Aumasson where the numbers of rounds are specified. <br>
    !  This is an interface routine that calls the working routine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), DIMENSION(..), INTENT(IN)  :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
    tByte,                  INTENT(IN)  :: Key(:)       !! key (at least 8 bytes)
    tIndex,                 INTENT(IN)  :: cRound       !! number of C rounds
    tIndex,                 INTENT(IN)  :: dRound       !! number of D rounds
    tInteger, OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
    tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tLong,    OPTIONAL,     INTENT(OUT) :: HashLong     !! long integer hash code
    tInteger                            :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER      :: InpPtr(:)
    tInteger            :: Seed
    TYPE(ByteConverter) :: ByteConv

!** FLOW

    ! initialize
    SET_OPTION(Seed, 0_kInteger, StartHash)
    CALL ByteConv%Initialize()
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)

    ! perform hashing
    IF (SIZE(Key) >= 8) THEN
        HashCode = HalfSip_Hash32(InpPtr, Seed, Key, cRound, dRound, ByteConv, HashLong)
    ELSE
        BLOCK
            tInteger        :: KeySize, I
            tByte           :: GenKey(8)
            TYPE(Jsf32RNG)  :: RNG
            CALL RNG%Initialize()
            KeySize = SIZE(Key)
            GenKey(1:KeySize) = Key(1:KeySize)
            DO I = KeySize+1, 8
                GenKey(I) = RNG%NextByte()
            END DO
            HashCode = HalfSip_Hash32(InpPtr, Seed, GenKey, cRound, dRound, ByteConv, HashLong)
        END BLOCK
    END IF

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION HalfSipHash_I32

!******************************************************************************

FUNCTION HalfSip24_Hash32(Input, Seed, Key, BC) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code using the HalfSipHash24 hash algorithm by Jean-Philippe
    !  Aumasson.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN) :: Input(0:)    !! input bytes
    tInteger,            INTENT(IN) :: Seed         !! seed
    tByte,               INTENT(IN) :: Key(0:7)     !! key bytes
    TYPE(ByteConverter), INTENT(IN) :: BC           !! byte converter
    tInteger                        :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: V0, V1, V2, V3, K0, K1, Mi, B
    tIndex      :: Length, Remaining, Offset

!** FLOW

    ! initialize
    V0 = 0
    V1 = Seed
    V2 = ToInteger(Z'6C796765')
    V3 = ToInteger(Z'74656462')
    K0 = BC%Pack_I32(Key, 0)
    K1 = BC%Pack_I32(Key, 4)
    Length = SIZE(Input)
    Remaining = Length
    Offset = 0

    B  = SHIFTL(ToInteger(Length), 24)
    V3 = IEOR(V3, K1)
    V2 = IEOR(V2, K0)
    V1 = IEOR(V1, K1)
    V0 = IEOR(V0, K0)

    ! perform hashing
    DO WHILE (Remaining >= 4)
        ! get input
        Mi = BC%Pack_I32(Input, Offset)
        V3 = IEOR(V3, Mi)
        CALL SipRound(V0, V1, V2, V3)
        CALL SipRound(V0, V1, V2, V3)
        V0 = IEOR(V0, Mi)
        ! update indices
        Remaining = Remaining - 4
        Offset = Offset + 4
    END DO

    ! get remaining bytes
    IF (Remaining > 0) THEN
        B = B + BC%Pack_I32_Partial(Input, Offset, Remaining)
    END IF

    V3 = IEOR(V3, B)
    CALL SipRound(V0, V1, V2, V3)
    CALL SipRound(V0, V1, V2, V3)
    V0 = IEOR(V0, B)
    V2 = IEOR(V2, Z'000000FF')
    CALL SipRound(V0, V1, V2, V3)
    CALL SipRound(V0, V1, V2, V3)
    CALL SipRound(V0, V1, V2, V3)
    CALL SipRound(V0, V1, V2, V3)
    HashCode = IEOR(V1, V3)

    RETURN

END FUNCTION HalfSip24_Hash32

!******************************************************************************

FUNCTION HalfSip_Hash32(Input, Seed, Key, cRound, dRound, BC, HashLong) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code using the HalfSipHash hash algorithm by Jean-Philippe
    !  Aumasson where the numbers of rounds are specified.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN)     :: Input(0:)    !! input bytes
    tInteger,            INTENT(IN)     :: Seed         !! seed
    tByte,               INTENT(IN)     :: Key(0:7)     !! key bytes
    tIndex,              INTENT(IN)     :: cRound       !! number of C rounds
    tIndex,              INTENT(IN)     :: dRound       !! number of D rounds
    TYPE(ByteConverter), INTENT(IN)     :: BC           !! byte converter
    tLong, OPTIONAL,     INTENT(OUT)    :: HashLong     !! long integer hash code
    tInteger                            :: HashCode     !! default integer hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: V0, V1, V2, V3, K0, K1, M, B, HashHigh
    tIndex      :: Length, Remaining, Offset, I

!** FLOW

    ! initialize
    Length = SIZE(Input)
    Remaining = Length
    Offset = 0
    K0 = BC%Pack_I32(Key, 0)
    K1 = BC%Pack_I32(Key, 4)
    B = SHIFTL(ToInteger(Length), 24)
    V0 = IEOR(K0, 0)
    V1 = IEOR(K1, Seed)
    V2 = IEOR(K0, Z'6C796765')
    V3 = IEOR(K1, Z'74656462')
    IF (PRESENT(HashLong)) V1 = IEOR(V1, Z'000000EE')

    ! perform hashing
    DO WHILE (Remaining >= 4)
        ! get input
        M = BC%Pack_I32(Input, Offset)
        V3 = IEOR(V3, M)
        DO I = 1, cRound
            CALL SipRound(V0, V1, V2, V3)
        END DO
        V0 = IEOR(V0, M)
        ! update indices
        Remaining = Remaining - 4
        Offset = Offset + 4
    END DO

    ! get remaining bytes
    IF (Remaining > 0) THEN
        B = B + BC%Pack_I32_Partial(Input, Offset, Remaining)
    END IF

    V3 = IEOR(V3, B)
    DO I = 1, cRound
        CALL SipRound(V0, V1, V2, V3)
    END DO
    V0 = IEOR(V0, B)

    ! finish
    IF (PRESENT(HashLong)) THEN
        V2 = IEOR(V2, Z'000000EE')
    ELSE
        V2 = IEOR(V2, Z'000000FF')
    END IF

    DO I = 1, dRound
        CALL SipRound(V0, V1, V2, V3)
    END DO

    HashCode = IEOR(V1, V3)
    IF (PRESENT(HashLong)) THEN
        V1 = IEOR(V1, Z'000000DD')
        DO I = 1, dRound
            CALL SipRound(V0, V1, V2, V3)
        END DO
        HashHigh = IEOR(V1, V3)
        IF (IsLittleEndian) THEN
            HashLong = IEOR(SHIFTL(MaskI32(HashHigh), 32), MaskI32(HashCode))
        ELSE
            HashLong = IEOR(SHIFTL(MaskI32(HashCode), 32), MaskI32(HashHigh))
        END IF
    END IF

    RETURN

END FUNCTION HalfSip_Hash32

!******************************************************************************

SUBROUTINE SipRound(V0, V1, V2, V3) 

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform mixing

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(INOUT) :: V0, V1, V2, V3

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    V0 = V0 + V1
    V1 = RotateLeft(V1, 5)
    V1 = IEOR(V1,  V0)
    V0 = RotateLeft(V0, 16)
    V2 = V2 + V3
    V3 = RotateLeft(V3, 8)
    V3 = IEOR(V3,  V2)
    V0 = V0 + V3
    V3 = RotateLeft(V3, 7)
    V3 = IEOR(V3,  V0)
    V2 = V2 + V1
    V1 = RotateLeft(V1, 13)
    V1 = IEOR(V1,  V2)
    V2 = RotateLeft(V2, 16)

    RETURN

END SUBROUTINE SipRound

!******************************************************************************

END SUBMODULE SubBase_HalfSipHash_Ref

!******************************************************************************


SUBMODULE (ModBase_ReferenceHash64) SubBase_SipHash_Ref

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains a reference implementation for the SipHash hash algorithm
!   by Jean-Philippe Aumasson. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    USE Class_ByteConverter
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tLong, PARAMETER    :: IV0 = ToLong(Z'736F6D6570736575')
    tLong, PARAMETER    :: IV1 = ToLong(Z'646F72616E646F6D')
    tLong, PARAMETER    :: IV2 = ToLong(Z'6C7967656E657261')
    tLong, PARAMETER    :: IV3 = ToLong(Z'7465646279746573')

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE FUNCTION SipHash24_I64(Input, InpSize, Key, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the SipHash24 hash algorithm by Jean-Philippe Aumasson.
    !  This is an interface routine that calls the working routine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), DIMENSION(..), INTENT(IN)  :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
    tByte,                  INTENT(IN)  :: Key(0:15)    !! key bytes
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
    HashCode = Sip24_Hash64(InpPtr, Seed, Key, ByteConv)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION SipHash24_I64

!******************************************************************************

MODULE FUNCTION SipHash_I128(Input, InpSize, Key, cRound, dRound, StartHash, &
                             RemoveSign, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the SipHash hash algorithm by Jean-Philippe Aumasson
    !  where the numbers of rounds are specified. <br>
    !  This is an interface routine that calls the working routine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), DIMENSION(..), INTENT(IN)  :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
    tByte,                  INTENT(IN)  :: Key(0:15)    !! key bytes
    tIndex,                 INTENT(IN)  :: cRound       ! number of C rounds
    tIndex,                 INTENT(IN)  :: dRound       ! number of D rounds
    tLong,    OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
    tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
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
    HashCode = Sip_Hash128(InpPtr, Seed, Key, cRound, dRound, ByteConv, HashPair)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION SipHash_I128

!******************************************************************************

MODULE FUNCTION TSipHash_I64(Input, InpSize, Key, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code using the TSipHash hash algorithm (a variant of SipHash)
    !  from SMHasher. <br>
    !  This is an interface routine that calls the working routine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), DIMENSION(..), INTENT(IN)  :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
    tByte,                  INTENT(IN)  :: Key(0:15)    !! key bytes
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
    HashCode = TSip_Hash64(InpPtr, Seed, Key, ByteConv)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION TSipHash_I64

!******************************************************************************

FUNCTION Sip24_Hash64(Input, Seed, Key, BC) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the SipHash24 hash algorithm by Jean-Philippe Aumasson.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN) :: Input(0:)    !! input bytes
    tLong,               INTENT(IN) :: Seed         !! seed
    tByte,               INTENT(IN) :: Key(0:15)    !! key bytes
    TYPE(ByteConverter), INTENT(IN) :: BC           !! byte converter
    tLong                           :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: V0, V1, V2, V3, K0, K1, Mi, B
    tIndex      :: Length, Remaining, Offset

!** SUBROUTINE MACRO DEFINITIONS:
#define HalfRound(A,B,C,D,S,T) \
    A = A + B; \
    C = C + D; \
    B = IEOR(RotateLeft(B, S), A); \
    D = IEOR(RotateLeft(D, T), C); \
    A = RotateLeft(A, 32);
#define DoubleRound(V0,V1,V2,V3) \
    HalfRound(V0, V1, V2, V3, 13, 16); \
    HalfRound(V2, V1, V0, V3, 17, 21); \
    HalfRound(V0, V1, V2, V3, 13, 16); \
    HalfRound(V2, V1, V0, V3, 17, 21);

!** FLOW

    ! initialize
    Length = SIZE(Input)
    Remaining = Length
    Offset = 0
    K0 = BC%Pack_I64(Key, 0)
    K1 = BC%Pack_I64(Key, 8)
    B  = SHIFTL(ToLong(IAND(Length, Z'000000FF')), 56)
    V0 = IEOR(IV0, K0)
    V1 = IEOR(IV1, K1) + Seed
    V2 = IEOR(IV2, K0)
    V3 = IEOR(IV3, K1)

    ! perform hashing
    DO WHILE (Remaining >= 8)
        ! get input and mix it with constants
        Mi = BC%Pack_I64(Input, Offset)
        V3 = IEOR(V3, Mi)
        DoubleRound(V0, V1, V2, V3)
        V0 = IEOR(V0, Mi)
        ! update indices
        Offset = Offset + 8
        Remaining = Remaining - 8
    END DO

    ! get input (remaining bytes) and mix it with constants
    IF (Remaining > 0) THEN
        B = IOR(B, BC%Pack_I64_Partial(Input, Offset, Remaining))
    END IF
    V3 = IEOR(V3, B)
    DoubleRound(V0, V1, V2, V3)
    V0 = IEOR(V0, B)

    ! finish
    V2 = IEOR(V2, Z'00000000000000FF')
    DoubleRound(V0, V1, V2, V3)
    DoubleRound(V0, V1, V2, V3)
    HashCode = IEOR(IEOR(V0, V1), IEOR(V2, V3))

    RETURN

#undef HalfRound
#undef DoubleRound

END FUNCTION Sip24_Hash64

!******************************************************************************

FUNCTION Sip_Hash128(Input, Seed, Key, cRound, dRound, BC, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the SipHash24 hash algorithm by Jean-Philippe Aumasson.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN)     :: Input(0:)    !! input bytes
    tLong,               INTENT(IN)     :: Seed         !! seed
    tByte,               INTENT(IN)     :: Key(0:15)    !! key bytes
    tIndex,              INTENT(IN)     :: cRound       !! number of C rounds
    tIndex,              INTENT(IN)     :: dRound       !! number of D rounds
    TYPE(ByteConverter), INTENT(IN)     :: BC           !! byte converter
    tLong, OPTIONAL,     INTENT(OUT)    :: HashPair(2)  !! dual (128-bit) hash codes
    tLong                               :: HashCode     !! single (64-bit) hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: V0, V1, V2, V3, K0, K1, M, B
    tIndex      :: Length, Remaining, Offset, I

!** SUBROUTINE MACRO DEFINITIONS:
#define SipRound(A,B,C,D) \
    A = A + B; \
    B = RotateLeft(B, 13); \
    B = IEOR(B, A); \
    A = RotateLeft(A, 32); \
    C = C + D; \
    D = RotateLeft(D, 16); \
    D = IEOR(D, C); \
    A = A + D; \
    D = RotateLeft(D, 21); \
    D = IEOR(D, A); \
    C = C + B; \
    B = RotateLeft(B, 17); \
    B = IEOR(B, C); \
    C = RotateLeft(C, 32);

!** FLOW

    ! initialize
    Length = SIZE(Input)
    Remaining = Length
    Offset = 0
    K0 = BC%Pack_I64(Key, 0)
    K1 = BC%Pack_I64(Key, 8)
    B  = SHIFTL(ToLong(Length), 56)
    V0 = IEOR(IV0, K0)
    V1 = IEOR(IV1, K1) + Seed
    V2 = IEOR(IV2, K0)
    V3 = IEOR(IV3, K1)
    IF (PRESENT(HashPair)) V1 = IEOR(V1, Z'00000000000000EE')

    ! perform hashing
    DO WHILE (Remaining >= 8)
        ! get input and mix it with constants
        M = BC%Pack_I64(Input, Offset)
        V3 = IEOR(V3, M)
        DO I = 1, cRound
            SipRound(V0, V1, V2, V3)
        END DO
        V0 = IEOR(V0, M)
        ! update indices
        Offset = Offset + 8
        Remaining = Remaining - 8
    END DO

    ! get input (remaining bytes) and mix it with constants
    IF (Remaining > 0) THEN
        B = IOR(B, BC%Pack_I64_Partial(Input, Offset, Remaining))
    END IF

    V3 = IEOR(V3, B)
    DO I = 1, cRound
        SipRound(V0, V1, V2, V3)
    END DO
    V0 = IEOR(V0, B)

    ! finish
    IF (PRESENT(HashPair)) THEN
        V2 = IEOR(V2, Z'00000000000000EE')
    ELSE
        V2 = IEOR(V2, Z'00000000000000FF')
    END IF

    DO I = 1, dRound
        SipRound(V0, V1, V2, V3)
    END DO

    HashCode = IEOR(IEOR(IEOR(V0, V1), V2), V3)
    IF (PRESENT(HashPair)) THEN
        V1 = IEOR(V1, Z'00000000000000DD')
        DO I = 1, dRound
            SipRound(V0, V1, V2, V3)
        END DO
        HashPair(1) = HashCode
        HashPair(2) = IEOR(IEOR(IEOR(V0, V1), V2), V3)
    END IF

    RETURN

#undef SipRound

END FUNCTION Sip_Hash128

!******************************************************************************

FUNCTION TSip_Hash64(Input, Seed, Key, BC) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code using the TSipHash hash algorithm (a variant of SipHash)
    !  from SMHasher.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN) :: Input(0:)    !! input bytes
    tLong,               INTENT(IN) :: Seed         !! seed
    tByte,               INTENT(IN) :: Key(0:15)    !! key bytes
    TYPE(ByteConverter), INTENT(IN) :: BC           !! byte converter
    tLong                           :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: V0, V1, K0, K1, Mi, Last7
    tIndex      :: Length, Remaining, Offset

!** SUBROUTINE MACRO DEFINITIONS:
#define SipCompress(A,B) \
    A = A + B; \
    B = IEOR(RotateLeft(B, 13), A); \
    A = RotateLeft(A, 35) + B; \
    B = IEOR(RotateLeft(B, 17), A); \
    A = RotateLeft(A, 21);

!** FLOW

    ! initialize
    Length = SIZE(Input)
    Remaining = Length
    Offset = 0
    K0 = BC%Pack_I64(Key, 0)
    K1 = BC%Pack_I64(Key, 8)
    Last7 = SHIFTL(ToLong(IAND(Length, Z'000000FF')), 56)
    V0 = IEOR(IV0, K0)
    V1 = IEOR(IV1, K1) + Seed

    ! perform hashing
    DO WHILE (Remaining >= 8)
        ! get input and mix it with constants
        Mi = BC%Pack_I64(Input, Offset)
        V1 = IEOR(V1, Mi)
        SipCompress(V0, V1)
        V0 = IEOR(V0, Mi)
        ! update indices
        Offset = Offset + 8
        Remaining = Remaining - 8
    END DO

    ! get input (remaining bytes) and mix it with constants
    IF (Remaining > 0) THEN
        Last7 = IOR(Last7, BC%Pack_I64_Partial(Input, Offset, Remaining))
    END IF
    V1 = IEOR(V1, Last7)
    SipCompress(V0, V1)
    V0 = IEOR(V0, Last7)

    ! finish
    V1 = IEOR(V1, Z'00000000000000FF')
    SipCompress(V0, V1)
    V1 = RotateLeft(V1, 32)
    SipCompress(V0, V1)
    V1 = RotateLeft(V1, 32)
    HashCode = IEOR(V0, V1)

    RETURN

#undef SipCompress

END FUNCTION TSip_Hash64

!******************************************************************************

END SUBMODULE SubBase_SipHash_Ref

!******************************************************************************

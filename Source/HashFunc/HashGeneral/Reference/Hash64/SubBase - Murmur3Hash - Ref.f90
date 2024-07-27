
SUBMODULE (ModBase_ReferenceHash64) SubBase_Murmur3Hash_Ref

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains a reference implementation for the Murmur3 hash algorithm
!   for 64-bit-integer (and optionally 128-bit) output by Austin Appleby. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    USE Class_ByteConverter
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tLong, PARAMETER    :: C1 = ToLong(Z'87C37B91114253D5')
    tLong, PARAMETER    :: C2 = ToLong(Z'4CF5AD432745937F')

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE FUNCTION Murmur3Hash_I128(Input, InpSize, StartHash, RemoveSign, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the Murmur3 hash algorithm by Austin Appleby.
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
    HashCode = Murmur3_Hash128(InpPtr, Seed, ByteConv, HashPair)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION Murmur3Hash_I128

!******************************************************************************

FUNCTION Murmur3_Hash128(Input, Seed, BC, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the Murmur3 hash algorithm by Austin Appleby.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN)     :: Input(0:)    !! input bytes
    tLong,               INTENT(IN)     :: Seed         !! seed
    TYPE(ByteConverter), INTENT(IN)     :: BC           !! byte converter
    tLong, OPTIONAL,     INTENT(OUT)    :: HashPair(2)  !! dual (128-bit) hash codes
    tLong                               :: HashCode     !! single (64-bit) hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: H1, H2, K1, K2
    tIndex      :: Length, Remaining, Offset

!** FLOW

    ! initialize
    H1 = Seed
    H2 = Seed
    Length = SIZE(Input)
    Remaining = Length
    Offset = 0

    ! perform hashing by mixing 16 bytes at a time into the hash
    DO WHILE (Remaining >= 16)

        K1 = BC%Pack_I64(Input, Offset)
        K2 = BC%Pack_I64(Input, Offset+8)
        Offset    = Offset + 16
        Remaining = Remaining - 16

        CALL K1_Mixing(K1)
        H1 = IEOR(H1, K1)
        H1 = RotateLeft(H1, 27)
        H1 = H1 + H2
        H1 = H1*5_kLong + ToLong(Z'0000000052DCE729')

        CALL K2_Mixing(K2)
        H2 = IEOR(H2, K2)
        H2 = RotateLeft(H2, 31)
        H2 = H2 + H1
        H2 = H2*5_kLong + ToLong(Z'0000000038495AB5')

    END DO

    IF (Remaining > 0) THEN
        SELECT CASE (Remaining)
        CASE (8:15)
            K1 = BC%Pack_I64(Input, Offset)
            IF (Remaining > 8) THEN
                K2 = BC%Pack_I64_Partial(Input, Offset+8, Remaining-8)
            ELSE
                K2 = 0_kLong
            END IF
        CASE (1:7)
            K1 = BC%Pack_I64_Partial(Input, Offset, Remaining)
            K2 = 0_kLong
        END SELECT
        CALL K1_Mixing(K1)
        H1 = IEOR(H1, K1)
        CALL K2_Mixing(K2)
        H2 = IEOR(H2, K2)
    END IF

    ! Do a few final mixes of the hash to ensure the last few bytes are well-incorporated
    H1 = IEOR(H1, ToLong(Length))
    H2 = IEOR(H2, ToLong(Length))
    H1 = H1 + H2
    H2 = H2 + H1
    CALL FinalMixing(H1)
    CALL FinalMixing(H2)

    IF (PRESENT(HashPair)) THEN
        H1 = H1 + H2
        HashPair(1) = H1
        HashPair(2) = H1 + H2
        HashCode = H1
    ELSE
        HashCode = H1 + H2
    END IF

    RETURN

END FUNCTION Murmur3_Hash128

!******************************************************************************

SUBROUTINE K1_Mixing(K)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform mixing of K1.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(INOUT) :: K

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    K = K*C1
    K = RotateLeft(K, 31)
    K = K*C2

    RETURN

END SUBROUTINE K1_Mixing

!******************************************************************************

SUBROUTINE K2_Mixing(K)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform mixing of K2.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(INOUT) :: K

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    K = K*C2
    K = RotateLeft(K, 33)
    K = K*C1

    RETURN

END SUBROUTINE K2_Mixing

!******************************************************************************

SUBROUTINE FinalMixing(H)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform final mixing.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(INOUT) :: H

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    H = IEOR(H, SHIFTR(H, 33))
    H = H*ToLong(Z'FF51AFD7ED558CCD')
    H = IEOR(H, SHIFTR(H, 33))
    H = H*ToLong(Z'C4CEB9FE1A85EC53')
    H = IEOR(H, SHIFTR(H, 33))

    RETURN

END SUBROUTINE FinalMixing

!******************************************************************************

END SUBMODULE SubBase_Murmur3Hash_Ref

!******************************************************************************


SUBMODULE (ModBase_ExperimentalHash64) SubBase_Murmur3Hash_Exp

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an experimental implementation for the Murmur3 hash algorithm
!   for 64-bit-integer (and optionally 128-bit) output by Austin Appleby. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define MaskI8(X)       IAND(ToLong(X), ToLong(Z'00000000000000FF'))

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

MODULE FUNCTION Murmur3_Hash128_Exp(Input, InpSize, Algo, StartHash, &
                                    RemoveSign, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the Murmur3 hash algorithm by Austin Appleby.
    !  This is an interface routine that calls the working routine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), DIMENSION(..), INTENT(IN)  :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
    tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-14)
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

!** FLOW

    ! initialize
    SET_OPTION(Seed, 0_kInteger, StartHash)
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)

    ! perform hashing
    SELECT CASE (Algo)
    CASE (1)
        HashCode = Murmur3_Hash128(InpPtr, Seed, Pack_I64_A1, HashPair)
    CASE (2)
        HashCode = Murmur3_Hash128(InpPtr, Seed, Pack_I64_A2, HashPair)
    CASE (3)
        HashCode = Murmur3_Hash128(InpPtr, Seed, Pack_I64_A3, HashPair)
    CASE (4)
        HashCode = Murmur3_Hash128(InpPtr, Seed, Pack_I64_A4, HashPair)
    CASE (5)
        HashCode = Murmur3_Hash128(InpPtr, Seed, Pack_I64_A5, HashPair)
    CASE (6)
        HashCode = Murmur3_Hash128(InpPtr, Seed, Pack_I64_A6, HashPair)
    CASE (7)
        HashCode = Murmur3_Hash128(InpPtr, Seed, Pack_I64_A7, HashPair)
    CASE (8)
        HashCode = Murmur3_Hash128_08(InpPtr, Seed, HashPair)
    CASE (9)
        HashCode = Murmur3_Hash128_09(InpPtr, Seed, HashPair)
    CASE (10)
        HashCode = Murmur3_Hash128_10(InpPtr, Seed, HashPair)
    CASE (11)
        HashCode = Murmur3_Hash128_11(InpPtr, Seed, HashPair)
    CASE (12)
        HashCode = Murmur3_Hash128_12(InpPtr, Seed, HashPair)
    CASE (13)
        HashCode = Murmur3_Hash128_13(InpPtr, Seed, HashPair)
    CASE (14)
        HashCode = Murmur3_Hash128_14(InpPtr, Seed, HashPair)
    END SELECT

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION Murmur3_Hash128_Exp

!******************************************************************************

FUNCTION Murmur3_Hash128(Input, Seed, PackFull, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the Murmur3 hash algorithm by Austin Appleby.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,           INTENT(IN)     :: Input(0:)    !! input bytes
    tLong,           INTENT(IN)     :: Seed         !! seed
    PROCEDURE(Pack_I64)             :: PackFull     !! procedure to convert a byte array to 64-bit integer
    tLong, OPTIONAL, INTENT(OUT)    :: HashPair(2)  !! dual (128-bit) hash codes
    tLong                           :: HashCode     !! single (64-bit) hash code

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

        K1 = PackFull(Input, Offset)
        K2 = PackFull(Input, Offset+8)
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
            K1 = PackFull(Input, Offset)
            IF (Remaining > 8) THEN
                K2 = PackPartial(Input, Offset+8, Remaining-8, PackFull)
            ELSE
                K2 = 0_kLong
            END IF
        CASE (1:7)
            K1 = PackPartial(Input, Offset, Remaining, PackFull)
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

FUNCTION Murmur3_Hash128_08(Input, Seed, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the MurmurHash3 hash algorithm by Austin Appleby.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, TARGET,   INTENT(IN)     :: Input(0:)    ! input bytes
    tLong,           INTENT(IN)     :: Seed         ! seed
    tLong, OPTIONAL, INTENT(OUT)    :: HashPair(2)  ! dual hash code
    tLong                           :: HashCode     ! single hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong, POINTER  :: LongVal(:) => NULL()
    tLong           :: H1, H2, K1, K2
    tIndex          :: Length, Remaining, Offset, Index
    TYPE(C_PTR)     :: CPTR

!** FLOW

    ! initialize
    H1 = Seed
    H2 = Seed
    Length = SIZE(Input)
    Remaining = Length
    Index = 1
    CPTR = C_LOC(Input(0))
    CALL C_F_POINTER(CPTR, LongVal, SHAPE=[Length/8])

    ! perform hashing by mixing 16 bytes at a time into the hash
    DO WHILE (Remaining >= 16)

        K1 = LongVal(Index)
        K2 = LongVal(Index+1)
        Index = Index + 2
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

    ! compute offset
    Offset = (Index-1)*8

    IF (Remaining > 0) THEN
        K1 = 0_kLong
        K2 = 0_kLong
        SELECT CASE (Remaining)
        CASE (8:15)
            K1 = LongVal(Index)
            K2 = Pack_Partial(Input, Offset+8, Remaining-8)
        CASE (1:7)
            K1 = Pack_Partial(Input, Offset, Remaining)
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

    NULLIFY(LongVal)

    RETURN

CONTAINS

    FUNCTION Pack_Full(Buf, Off) RESULT(Res)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To pack the array 'Buf' at offset 'Off' into the 64-bit word 'Res',
        ! in little-endian convention (least significant byte first).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,  TARGET, INTENT(IN)  :: Buf(0:)  ! buffer
        tIndex,         INTENT(IN)  :: Off      ! offset
        tLong                       :: Res      ! result

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong, POINTER  :: Val => NULL()
        TYPE(C_PTR)     :: CPTR

    ! FLOW

        ! implementation algorithm #6
        CPTR = C_LOC(Buf(Off))
        CALL C_F_POINTER(CPTR, Val)
        Res = Val
        NULLIFY(Val)

        RETURN

    END FUNCTION Pack_Full

    !**************************************************************************

    FUNCTION Pack_Partial(Buf, Off, Length) RESULT(Res)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To pack seven or fewer bytes of the array 'Buf' at offset 'Off'
        ! into the 64-bit word 'Res', in little-endian convention
        ! (least significant byte first).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,  INTENT(IN)  :: Buf(0:)  ! buffer
        tIndex, INTENT(IN)  :: Off      ! offset
        tIndex, INTENT(IN)  :: Length   ! the number of bytes to pack (between 1 to 7)
        tLong               :: Res      ! result

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tByte       :: Wrk(0:7)
        tIndex      :: I

    ! FLOW

        ! initialize
        Wrk = 0_kByte

        ! gather available bytes
        DO I = 0, Length-1
            Wrk(I) = Buf(Off+I)
        END DO

        ! pack bytes into word
        Res = Pack_Full(Wrk, 0)

        RETURN

    END FUNCTION Pack_Partial

    !**************************************************************************

END FUNCTION Murmur3_Hash128_08

!******************************************************************************

FUNCTION Murmur3_Hash128_09(Input, Seed, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the MurmurHash3 hash algorithm by Austin Appleby.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,           INTENT(IN)     :: Input(0:)    ! input bytes
    tLong,           INTENT(IN)     :: Seed         ! seed
    tLong, OPTIONAL, INTENT(OUT)    :: HashPair(2)  ! dual hash code
    tLong                           :: HashCode     ! single hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong           :: H1, H2, K1, K2
    tIndex          :: Length, Remaining, Offset
    tByte           :: ByteVal(0:15)
    tLong           :: LongVal(1:2)
    EQUIVALENCE(LongVal, ByteVal)

!** FLOW

    ! initialize
    H1 = Seed
    H2 = Seed
    Length = SIZE(Input)
    Remaining = Length
    Offset = 0

    ! perform hashing by mixing 16 bytes at a time into the hash
    DO WHILE (Remaining >= 16)

        ByteVal(0:15) = Input(Offset:Offset+15)
        K1 = LongVal(1)
        K2 = LongVal(2)
        Offset = Offset + 16
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
            ByteVal(0:Remaining-1) = Input(Offset:Offset+Remaining-1)
            ByteVal(Remaining:15)  = 0_kByte
            K1 = LongVal(1)
            K2 = LongVal(2)
        CASE (1:7)
            ByteVal(0:Remaining-1) = Input(Offset:Offset+Remaining-1)
            ByteVal(Remaining:7)   = 0_kByte
            K1 = LongVal(1)
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

END FUNCTION Murmur3_Hash128_09

!******************************************************************************

FUNCTION Murmur3_Hash128_10(Input, Seed, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the MurmurHash3 hash algorithm by Austin Appleby.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, TARGET,   INTENT(IN)     :: Input(0:)    ! input bytes
    tLong,           INTENT(IN)     :: Seed         ! seed
    tLong, OPTIONAL, INTENT(OUT)    :: HashPair(2)  ! dual hash code
    tLong                           :: HashCode     ! single hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong, POINTER  :: LongVal(:) => NULL()
    tLong           :: H1, H2, K1, K2
    tIndex          :: Length, Remaining, Offset, Index, NBlocks
    tByte, TARGET   :: LastBytes(0:15)
    TYPE(C_PTR)     :: CPTR

!** FLOW

    ! initialize
    H1 = Seed
    H2 = Seed
    Length = SIZE(Input)
    Remaining = Length
    Index = 1
    NBlocks = Length/16
    CPTR = C_LOC(Input(0))
    CALL C_F_POINTER(CPTR, LongVal, SHAPE=[2*NBlocks])

    ! perform hashing by mixing 16 bytes at a time into the hash
    DO WHILE (Remaining >= 16)

        K1 = LongVal(Index)
        K2 = LongVal(Index+1)
        Index = Index + 2
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
        ! compute offset
        Offset = (Index-1)*8
        LastBytes(0:Remaining-1) = Input(Offset:Offset+Remaining-1)
        LastBytes(Remaining:15)  = 0_kByte
        CPTR = C_LOC(LastBytes(0))
        CALL C_F_POINTER(CPTR, LongVal, SHAPE=[2])
        K1 = LongVal(1)
        K2 = LongVal(2)
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

    NULLIFY(LongVal)

    RETURN

END FUNCTION Murmur3_Hash128_10

!******************************************************************************

FUNCTION Murmur3_Hash128_11(Input, Seed, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the MurmurHash3 hash algorithm by Austin Appleby.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,           INTENT(IN)     :: Input(0:)    ! input bytes
    tLong,           INTENT(IN)     :: Seed         ! seed
    tLong, OPTIONAL, INTENT(OUT)    :: HashPair(2)  ! dual hash code
    tLong                           :: HashCode     ! single hash code

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

        K1 = PackFull(Input, Offset)
        K2 = PackFull(Input, Offset+8)
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
        K1 = 0_kLong
        K2 = 0_kLong

        IF (Remaining == 15) K2 = IEOR(K2, SHIFTL(MaskI8(Input(Offset+14)), 48))
        IF (Remaining >= 14) K2 = IEOR(K2, SHIFTL(MaskI8(Input(Offset+13)), 40))
        IF (Remaining >= 13) K2 = IEOR(K2, SHIFTL(MaskI8(Input(Offset+12)), 32))
        IF (Remaining >= 12) K2 = IEOR(K2, SHIFTL(MaskI8(Input(Offset+11)), 24))
        IF (Remaining >= 11) K2 = IEOR(K2, SHIFTL(MaskI8(Input(Offset+10)), 16))
        IF (Remaining >= 10) K2 = IEOR(K2, SHIFTL(MaskI8(Input(Offset+ 9)),  8))
        IF (Remaining >=  9) THEN
            K2 = IEOR(K2, MaskI8(Input(Offset+8)))
            CALL K2_Mixing(K2)
            H2 = IEOR(H2, K2)
        END IF
        IF (Remaining >= 8) K1 = IEOR(K1, SHIFTL(MaskI8(Input(Offset+ 7)),  56))
        IF (Remaining >= 7) K1 = IEOR(K1, SHIFTL(MaskI8(Input(Offset+ 6)),  48))
        IF (Remaining >= 6) K1 = IEOR(K1, SHIFTL(MaskI8(Input(Offset+ 5)),  40))
        IF (Remaining >= 5) K1 = IEOR(K1, SHIFTL(MaskI8(Input(Offset+ 4)),  32))
        IF (Remaining >= 4) K1 = IEOR(K1, SHIFTL(MaskI8(Input(Offset+ 3)),  24))
        IF (Remaining >= 3) K1 = IEOR(K1, SHIFTL(MaskI8(Input(Offset+ 2)),  16))
        IF (Remaining >= 2) K1 = IEOR(K1, SHIFTL(MaskI8(Input(Offset+ 1)),   8))
        IF (Remaining >= 1) THEN
            K1 = IEOR(K1, MaskI8(Input(Offset)))
            CALL K1_Mixing(K1)
            H1 = IEOR(H1, K1)
        END IF
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

CONTAINS

    FUNCTION PackFull(Buf, Off) RESULT(Res)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To pack the array 'Buf' at offset 'Off' into the 64-bit word 'Res',
        ! in little-endian convention (least significant byte first).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,  TARGET, INTENT(IN)  :: Buf(0:)  ! buffer
        tIndex,         INTENT(IN)  :: Off      ! offset
        tLong                       :: Res      ! result

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong, POINTER  :: Val => NULL()
        TYPE(C_PTR)     :: CPTR

    ! FLOW

        ! implementation algorithm #6
        CPTR = C_LOC(Buf(Off))
        CALL C_F_POINTER(CPTR, Val)
        Res = Val
        NULLIFY(Val)

        RETURN

    END FUNCTION PackFull

    !**************************************************************************

END FUNCTION Murmur3_Hash128_11

!******************************************************************************

FUNCTION Murmur3_Hash128_12(Input, Seed, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the MurmurHash3 hash algorithm by Austin Appleby.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,           INTENT(IN)     :: Input(0:)    ! input bytes
    tLong,           INTENT(IN)     :: Seed         ! seed
    tLong, OPTIONAL, INTENT(OUT)    :: HashPair(2)  ! dual hash code
    tLong                           :: HashCode     ! single hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong           :: H1, H2
    tIndex          :: Length, Remaining, Offset
    tByte           :: ByteVal(0:15)
    tLong           :: KVal(1:2)
    EQUIVALENCE(KVal, ByteVal)

!** FLOW

    ! initialize
    H1 = Seed
    H2 = Seed
    Length = SIZE(Input)
    Remaining = Length
    Offset = 0

    ! perform hashing by mixing 16 bytes at a time into the hash
    DO WHILE (Remaining >= 16)

        ByteVal(0:15) = Input(Offset:Offset+15)
        Offset = Offset + 16
        Remaining = Remaining - 16

        CALL K1_Mixing(KVal(1))
        H1 = IEOR(H1, KVal(1))
        H1 = RotateLeft(H1, 27)
        H1 = H1 + H2
        H1 = H1*5_kLong + ToLong(Z'0000000052DCE729')

        CALL K2_Mixing(KVal(2))
        H2 = IEOR(H2, KVal(2))
        H2 = RotateLeft(H2, 31)
        H2 = H2 + H1
        H2 = H2*5_kLong + ToLong(Z'0000000038495AB5')

    END DO

    IF (Remaining > 0) THEN
        KVal(1) = 0_kLong
        KVal(2) = 0_kLong
        ByteVal(0:Remaining-1) = Input(Offset:Offset+Remaining-1)
        CALL K1_Mixing(KVal(1))
        H1 = IEOR(H1, KVal(1))
        CALL K2_Mixing(KVal(2))
        H2 = IEOR(H2, KVal(2))
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

END FUNCTION Murmur3_Hash128_12

!******************************************************************************

FUNCTION Murmur3_Hash128_13(Input, Seed, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the MurmurHash3 hash algorithm by Austin Appleby.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, TARGET,   INTENT(IN)     :: Input(0:)    ! input bytes
    tLong,           INTENT(IN)     :: Seed         ! seed
    tLong, OPTIONAL, INTENT(OUT)    :: HashPair(2)  ! dual hash code
    tLong                           :: HashCode     ! single hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong           :: H1, H2, K1, K2
    tIndex          :: Length, Remaining, Offset
    tLong, POINTER  :: LongVal => NULL()
    TYPE(C_PTR)     :: CPTR

!** FLOW

    ! initialize
    H1 = Seed
    H2 = Seed
    Length = SIZE(Input)
    Remaining = Length
    Offset = 0

    ! perform hashing by mixing 16 bytes at a time into the hash
    DO WHILE (Remaining >= 16)
        CPTR = C_LOC(Input(Offset))
        CALL C_F_POINTER(CPTR, LongVal)
        K1 = LongVal
        CPTR = C_LOC(Input(Offset+8))
        CALL C_F_POINTER(CPTR, LongVal)
        K2 = LongVal
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
        K1 = 0_kLong
        K2 = 0_kLong

        IF (Remaining == 15) K2 = IEOR(K2, SHIFTL(MaskI8(Input(Offset+14)), 48))
        IF (Remaining >= 14) K2 = IEOR(K2, SHIFTL(MaskI8(Input(Offset+13)), 40))
        IF (Remaining >= 13) K2 = IEOR(K2, SHIFTL(MaskI8(Input(Offset+12)), 32))
        IF (Remaining >= 12) K2 = IEOR(K2, SHIFTL(MaskI8(Input(Offset+11)), 24))
        IF (Remaining >= 11) K2 = IEOR(K2, SHIFTL(MaskI8(Input(Offset+10)), 16))
        IF (Remaining >= 10) K2 = IEOR(K2, SHIFTL(MaskI8(Input(Offset+ 9)),  8))
        IF (Remaining >=  9) THEN
            K2 = IEOR(K2, MaskI8(Input(Offset+8)))
            CALL K2_Mixing(K2)
            H2 = IEOR(H2, K2)
        END IF
        IF (Remaining >= 8) K1 = IEOR(K1, SHIFTL(MaskI8(Input(Offset+ 7)),  56))
        IF (Remaining >= 7) K1 = IEOR(K1, SHIFTL(MaskI8(Input(Offset+ 6)),  48))
        IF (Remaining >= 6) K1 = IEOR(K1, SHIFTL(MaskI8(Input(Offset+ 5)),  40))
        IF (Remaining >= 5) K1 = IEOR(K1, SHIFTL(MaskI8(Input(Offset+ 4)),  32))
        IF (Remaining >= 4) K1 = IEOR(K1, SHIFTL(MaskI8(Input(Offset+ 3)),  24))
        IF (Remaining >= 3) K1 = IEOR(K1, SHIFTL(MaskI8(Input(Offset+ 2)),  16))
        IF (Remaining >= 2) K1 = IEOR(K1, SHIFTL(MaskI8(Input(Offset+ 1)),   8))
        IF (Remaining >= 1) THEN
            K1 = IEOR(K1, MaskI8(Input(Offset)))
            CALL K1_Mixing(K1)
            H1 = IEOR(H1, K1)
        END IF
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

END FUNCTION Murmur3_Hash128_13

!******************************************************************************

FUNCTION Murmur3_Hash128_14(Input, Seed, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the MurmurHash3 hash algorithm by Austin Appleby.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, TARGET,   INTENT(IN)     :: Input(0:)    ! input bytes
    tLong,           INTENT(IN)     :: Seed         ! seed
    tLong, OPTIONAL, INTENT(OUT)    :: HashPair(2)  ! dual hash code
    tLong                           :: HashCode     ! single hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong           :: H1, H2, K1, K2
    tIndex          :: Length, Remaining, Offset
    tByte, TARGET   :: LastBytes(0:15)
    tLong, POINTER  :: LongVal => NULL()
    TYPE(C_PTR)     :: CPTR

!** FLOW

    ! initialize
    H1 = Seed
    H2 = Seed
    Length = SIZE(Input)
    Remaining = Length
    Offset = 0

    ! perform hashing by mixing 16 bytes at a time into the hash
    DO WHILE (Remaining >= 16)
        CPTR = C_LOC(Input(Offset))
        CALL C_F_POINTER(CPTR, LongVal)
        K1 = LongVal
        CPTR = C_LOC(Input(Offset+8))
        CALL C_F_POINTER(CPTR, LongVal)
        K2 = LongVal
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
        LastBytes = 0_kByte

        IF (Remaining == 15) LastBytes(14) = Input(Offset+14)
        IF (Remaining >= 14) LastBytes(13) = Input(Offset+13)
        IF (Remaining >= 13) LastBytes(12) = Input(Offset+12)
        IF (Remaining >= 12) LastBytes(11) = Input(Offset+11)
        IF (Remaining >= 11) LastBytes(10) = Input(Offset+10)
        IF (Remaining >= 10) LastBytes( 9) = Input(Offset+ 9)
        IF (Remaining >=  9) THEN
            LastBytes(8) = Input(Offset+8)
            CPTR = C_LOC(LastBytes(8))
            CALL C_F_POINTER(CPTR, LongVal)
            K2 = LongVal
            CALL K2_Mixing(K2)
            H2 = IEOR(H2, K2)
        END IF
        IF (Remaining >= 8) LastBytes(7) = Input(Offset+7)
        IF (Remaining >= 7) LastBytes(6) = Input(Offset+6)
        IF (Remaining >= 6) LastBytes(5) = Input(Offset+5)
        IF (Remaining >= 5) LastBytes(4) = Input(Offset+4)
        IF (Remaining >= 4) LastBytes(3) = Input(Offset+3)
        IF (Remaining >= 3) LastBytes(2) = Input(Offset+2)
        IF (Remaining >= 2) LastBytes(1) = Input(Offset+1)
        IF (Remaining >= 1) THEN
            LastBytes(0) = Input(Offset)
            CPTR = C_LOC(LastBytes(0))
            CALL C_F_POINTER(CPTR, LongVal)
            K1 = LongVal
            CALL K1_Mixing(K1)
            H1 = IEOR(H1, K1)
        END IF
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

END FUNCTION Murmur3_Hash128_14

!******************************************************************************

END SUBMODULE SubBase_Murmur3Hash_Exp

!******************************************************************************

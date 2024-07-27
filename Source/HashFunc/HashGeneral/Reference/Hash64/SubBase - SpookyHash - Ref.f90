
SUBMODULE (ModBase_ReferenceHash64) SubBase_SpookyHash_Ref

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains a reference implementation for version 2 of the SpookyHash
!   hash algorithm by Bob Jenkins. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    USE Class_ByteConverter
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tLong,  PARAMETER   :: GOLDEN_RATIO_64 = ToLong(Z'9E3779B97F4A7C15')
    tLong,  PARAMETER   :: SC = ToLong(Z'DEADBEEFDEADBEEF')
    tIndex, PARAMETER   :: StateSize  = 12
    tIndex, PARAMETER   :: BlockSize  = StateSize*8
    tIndex, PARAMETER   :: SmallLimit = BlockSize*2

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE FUNCTION SpookyHash_I128(Input, InpSize, StartHash, RemoveSign, &
                                Seed, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the SpookyHash hash algorithm by Bob Jenkins.
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
    tLong,    OPTIONAL,     INTENT(IN)  :: Seed         !! optional seed
    tLong,    OPTIONAL,     INTENT(OUT) :: HashPair(2)  !! dual (128-bit) hash codes
    tLong                               :: HashCode     !! single (64-bit) hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER      :: InpPtr(:)
    tLong               :: Seed1
    TYPE(ByteConverter) :: ByteConv

!** FLOW

    ! initialize
    SET_OPTION(Seed1, 0_kInteger, StartHash)
    CALL ByteConv%Initialize()
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)

    ! perform hashing
    HashCode = Spooky_Hash128(InpPtr, Seed1, ByteConv, Seed, HashPair)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION SpookyHash_I128

!******************************************************************************

FUNCTION Spooky_Hash128(Input, Seed1, BC, Seed2, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the SpookyHash hash algorithm by Bob Jenkins.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN)     :: Input(0:)    !! input bytes
    tLong,               INTENT(IN)     :: Seed1        !! seed
    TYPE(ByteConverter), INTENT(IN)     :: BC           !! byte converter
    tLong, OPTIONAL,     INTENT(IN)     :: Seed2        !! optional (additional) seed
    tLong, OPTIONAL,     INTENT(OUT)    :: HashPair(2)  !! dual (128-bit) hash codes
    tLong                               :: HashCode     !! single (64-bit) hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: H0, H1, H2, H3, H4, H5
    tLong       :: H6, H7, H8, H9, H10, H11
    tLong       :: Seed0, Partial
    tIndex      :: Length, Remaining, Offset
    tIndex      :: PartialSize, WholeWords, I

!** FLOW

    ! check length and call SmallHash if length is short (< 192)
    Length = SIZE(Input)
    IF (Length < SmallLimit) THEN
        HashCode = SmallHash(Input, Length, Seed1, BC, Seed2, HashPair)
        RETURN
    END IF

    ! initialize
    SET_OPTION(Seed0, Seed1+GOLDEN_RATIO_64, Seed2)
    H0 = Seed1
    H3 = Seed1
    H6 = Seed1
    H9 = Seed1
    H1 = Seed0
    H4 = Seed0
    H7 = Seed0
    H10 = Seed0
    H2 = SC
    H5 = SC
    H8 = SC
    H11 = SC

    Remaining = Length
    Offset = 0
    DO WHILE (Remaining >= BlockSize)
        ! mixing internal states and data
        CALL MixData(H0,  H2,  H10, H11, H1,  11, Input, Offset)
        CALL MixData(H1,  H3,  H11, H0,  H2,  32, Input, Offset+8)
        CALL MixData(H2,  H4,  H0,  H1,  H3,  43, Input, Offset+16)
        CALL MixData(H3,  H5,  H1,  H2,  H4,  31, Input, Offset+24)
        CALL MixData(H4,  H6,  H2,  H3,  H5,  17, Input, Offset+32)
        CALL MixData(H5,  H7,  H3,  H4,  H6,  28, Input, Offset+40)
        CALL MixData(H6,  H8,  H4,  H5,  H7,  39, Input, Offset+48)
        CALL MixData(H7,  H9,  H5,  H6,  H8,  57, Input, Offset+56)
        CALL MixData(H8,  H10, H6,  H7,  H9,  55, Input, Offset+64)
        CALL MixData(H9,  H11, H7,  H8,  H10, 54, Input, Offset+72)
        CALL MixData(H10, H0,  H8,  H9,  H11, 22, Input, Offset+80)
        CALL MixData(H11, H1,  H9,  H10, H0,  46, Input, Offset+88)
        ! update indices
        Remaining = Remaining - BlockSize
        Offset = Offset + BlockSize
    END DO

    PartialSize = IAND(Remaining, 7)
    WholeWords = SHIFTR(Remaining, 3)
    IF (PartialSize > 0) THEN
        Partial = BC%Pack_I64_Partial(Input, Offset + SHIFTL(WholeWords, 3), PartialSize)
        SELECT CASE (WholeWords)
        CASE (0)
            H0 = H0 + Partial
        CASE (1)
            H1 = H1 + Partial
        CASE (2)
            H2 = H2 + Partial
        CASE (3)
            H3 = H3 + Partial
        CASE (4)
            H4 = H4 + Partial
        CASE (5)
            H5 = H5 + Partial
        CASE (6)
            H6 = H6 + Partial
        CASE (7)
            H7 = H7 + Partial
        CASE (8)
            H8 = H8 + Partial
        CASE (9)
            H9 = H9 + Partial
        CASE (10)
            H10 = H10 + Partial
        CASE (11)
            H11 = H11 + Partial
        END SELECT
    END IF

    ! fall-through is intentional
    IF (WholeWords  >= 11) H10 = H10 + BC%Pack_I64(Input, Offset+80)
    IF (WholeWords  >= 10) H9  = H9  + BC%Pack_I64(Input, Offset+72)
    IF (WholeWords  >=  9) H8  = H8  + BC%Pack_I64(Input, Offset+64)
    IF (WholeWords  >=  8) H7  = H7  + BC%Pack_I64(Input, Offset+56)
    IF (WholeWords  >=  7) H6  = H6  + BC%Pack_I64(Input, Offset+48)
    IF (WholeWords  >=  6) H5  = H5  + BC%Pack_I64(Input, Offset+40)
    IF (WholeWords  >=  5) H4  = H4  + BC%Pack_I64(Input, Offset+32)
    IF (WholeWords  >=  4) H3  = H3  + BC%Pack_I64(Input, Offset+24)
    IF (WholeWords  >=  3) H2  = H2  + BC%Pack_I64(Input, Offset+16)
    IF (WholeWords  >=  2) H1  = H1  + BC%Pack_I64(Input, Offset+8)
    IF (WholeWords  >=  1) H0  = H0  + BC%Pack_I64(Input, Offset)

    H11 = H11 + SHIFTL(ToLong(Remaining), 56)

    ! end mixing with 3 iterations
    DO I = 1, 3
        CALL MixH(H11, H1,  H2,  44)
        CALL MixH(H0,  H2,  H3,  15)
        CALL MixH(H1,  H3,  H4,  34)
        CALL MixH(H2,  H4,  H5,  21)
        CALL MixH(H3,  H5,  H6,  38)
        CALL MixH(H4,  H6,  H7,  33)
        CALL MixH(H5,  H7,  H8,  10)
        CALL MixH(H6,  H8,  H9,  13)
        CALL MixH(H7,  H9,  H10, 38)
        CALL MixH(H8,  H10, H11, 53)
        CALL MixH(H9,  H11, H0,  42)
        CALL MixH(H10, H0,  H1,  54)
    END DO

    ! set output
    HashCode = H0
    IF (PRESENT(HashPair)) THEN
        HashPair(1) = H0
        HashPair(2) = H1
    END IF

    RETURN

    CONTAINS

    SUBROUTINE MixData(A, B, C, D, E, Pos, Inp, Off)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform mixing of internal states and data.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong,    INTENT(INOUT) :: A, B, C, D, E
        tInteger, INTENT(IN)    :: Pos
        tByte,    INTENT(IN)    :: Inp(0:)
        tIndex,   INTENT(IN)    :: Off

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        A = A + BC%Pack_I64(Inp, Off)
        B = IEOR(B, C)
        D = IEOR(D, A)
        A = RotateLeft(A, Pos)
        D = D + E

        RETURN

    END SUBROUTINE MixData

    !**************************************************************************

    SUBROUTINE MixH(A, B, C, Pos)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform mixing of internal states.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong,    INTENT(INOUT) :: A, B, C
        tInteger, INTENT(IN)    :: Pos

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW
        
        A = A + B
        C = IEOR(C, A)
        B = RotateLeft(B, Pos)

        RETURN

    END SUBROUTINE MixH

    !**************************************************************************

END FUNCTION Spooky_Hash128

!******************************************************************************

FUNCTION SmallHash(Input, Length, Seed1, BC, Seed2, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code for small input using the SpookyHash (version 2)
    ! hash algorithm by Bob Jenkins.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,                INTENT(IN)    :: Input(0:)    ! input bytes
    tIndex,               INTENT(IN)    :: Length       ! size of input bytes
    tLong,                INTENT(IN)    :: Seed1        ! seed
    CLASS(ByteConverter), INTENT(IN)    :: BC           ! byte converter
    tLong, OPTIONAL,      INTENT(IN)    :: Seed2        ! seed
    tLong, OPTIONAL,      INTENT(OUT)   :: HashPair(2)  ! dual hash codes
    tLong                               :: HashCode     ! single hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: H0, H1, H2, H3
    tIndex      :: Remaining, Offset

!** FLOW

    ! initialize
    H0 = Seed1
    H1 = Seed1+GOLDEN_RATIO_64
    IF (PRESENT(Seed2)) H1 = Seed2
    H2 = SC
    H3 = SC
    Remaining = Length
    Offset = 0

    DO WHILE (Remaining >= 32)
        H2 = H2 + BC%Pack_I64(Input, Offset)
        H3 = H3 + BC%Pack_I64(Input, Offset+8)
        CALL ShortMix(H0, H1, H2, H3)
        H0 = H0 + BC%Pack_I64(Input, Offset+16)
        H1 = H1 + BC%Pack_I64(Input, Offset+24)
        ! update indices
        Offset = Offset + 32
        Remaining = Remaining - 32
    END DO

    IF (Remaining >= 16) THEN
        H2 = H2 + BC%Pack_I64(Input, Offset)
        H3 = H3 + BC%Pack_I64(Input, Offset+8)
        CALL ShortMix(H0, H1, H2, H3)
        ! update indices
        Offset = Offset + 16
        Remaining = Remaining - 16
    END IF

    H3 = H3 + SHIFTL(ToLong(Length), 56)

    IF (Remaining >= 8) THEN
        H2 = H2 + BC%Pack_I64(Input, Offset)
        ! update indices
        Offset = Offset + 8
        Remaining = Remaining - 8
        IF (Remaining > 0) H3 = H3 + BC%Pack_I64_Partial(Input, Offset, Remaining)
    ELSEIF (Remaining > 0) THEN
        H2 = H2 + BC%Pack_I64_Partial(Input, Offset, Remaining)
    ELSE
        H2 = H2 + SC
        H3 = H3 + SC
    END IF

    ! ShortEnd
    CALL MixH2(H3, H2, 15)
    CALL MixH2(H0, H3, 52)
    CALL MixH2(H1, H0, 26)
    CALL MixH2(H2, H1, 51)
    CALL MixH2(H3, H2, 28)
    CALL MixH2(H0, H3,  9)
    CALL MixH2(H1, H0, 47)
    CALL MixH2(H2, H1, 54)
    CALL MixH2(H3, H2, 32)
    CALL MixH2(H0, H3, 25)
    CALL MixH2(H1, H0, 63)

    ! set output
    HashCode = H0
    IF (PRESENT(HashPair)) THEN
        HashPair(1) = H0
        HashPair(2) = H1
    END IF

    RETURN

    CONTAINS

    SUBROUTINE MixH1(A, B, C, Pos)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform mixing of internal states.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong,    INTENT(INOUT) :: A, C
        tLong,    INTENT(IN)    :: B
        tInteger, INTENT(IN)    :: Pos

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW
        
        A = RotateLeft(A, Pos)
        A = A + B
        C = IEOR(C, A)

        RETURN

    END SUBROUTINE MixH1

    !**************************************************************************

    SUBROUTINE MixH2(A, B, Pos)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform mixing of internal states.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong,    INTENT(INOUT) :: A, B
        tInteger, INTENT(IN)    :: Pos

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW
        
        A = IEOR(A, B)
        B = RotateLeft(B, Pos)
        A = A + B

        RETURN

    END SUBROUTINE MixH2

    !**************************************************************************

    SUBROUTINE ShortMix(A, B, C, D)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform mixing of internal states.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong,    INTENT(INOUT) :: A, B, C, D

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        CALL MixH1(C, D, A, 50)
        CALL MixH1(D, A, B, 52)
        CALL MixH1(A, B, C, 30)
        CALL MixH1(B, C, D, 41)
        CALL MixH1(C, D, A, 54)
        CALL MixH1(D, A, B, 48)
        CALL MixH1(A, B, C, 38)
        CALL MixH1(B, C, D, 37)
        CALL MixH1(C, D, A, 62)
        CALL MixH1(D, A, B, 34)
        CALL MixH1(A, B, C,  5)
        CALL MixH1(B, C, D, 36)

        RETURN

    END SUBROUTINE ShortMix

    !**************************************************************************

END FUNCTION SmallHash

!******************************************************************************

END SUBMODULE SubBase_SpookyHash_Ref

!******************************************************************************

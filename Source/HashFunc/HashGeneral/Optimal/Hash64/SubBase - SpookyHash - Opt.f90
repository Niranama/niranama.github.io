
SUBMODULE (ModBase_OptimalHash64) SubBase_SpookyHash_Opt

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an optimal implementation for version 2 of the SpookyHash
!   hash algorithm by Bob Jenkins. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define MixData(A, B, C, D, E, Pos, Inp, Off) \
    A = A + PackFull(Inp, Off); \
    B = IEOR(B, C); \
    D = IEOR(D, A); \
    A = RotateLeft(A, Pos); \
    D = D + E;
#define MixH(A, B, C, Pos) \
    A = A + B; \
    C = IEOR(C, A); \
    B = RotateLeft(B, Pos);
#define MixH1(A, B, C, P) \
    A = RotateLeft(A, P); \
    A = A + B; \
    C = IEOR(C, A);
#define MixH2(A, B, P) \
    A = IEOR(A, B); \
    B = RotateLeft(B, P); \
    A = A + B;
#define ShortMix(A, B, C, D) \
    MixH1(C, D, A, 50); \
    MixH1(D, A, B, 52); \
    MixH1(A, B, C, 30); \
    MixH1(B, C, D, 41); \
    MixH1(C, D, A, 54); \
    MixH1(D, A, B, 48); \
    MixH1(A, B, C, 38); \
    MixH1(B, C, D, 37); \
    MixH1(C, D, A, 62); \
    MixH1(D, A, B, 34); \
    MixH1(A, B, C,  5); \
    MixH1(B, C, D, 36);

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

MODULE FUNCTION Spooky_Hash128_Opt(Input, InpSize, StartHash, RemoveSign, &
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

!** FLOW

    ! initialize
    SET_OPTION(Seed1, 0_kInteger, StartHash)
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)

    ! perform hashing
    HashCode = Spooky_Hash128(InpPtr, Seed1, Seed, HashPair)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION Spooky_Hash128_Opt

!******************************************************************************

FUNCTION Spooky_Hash128(Input, Seed1, Seed2, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the SpookyHash hash algorithm by Bob Jenkins.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,           INTENT(IN)     :: Input(0:)    !! input bytes
    tLong,           INTENT(IN)     :: Seed1        !! seed
    tLong, OPTIONAL, INTENT(IN)     :: Seed2        !! optional (additional) seed
    tLong, OPTIONAL, INTENT(OUT)    :: HashPair(2)  !! dual (128-bit) hash codes
    tLong                           :: HashCode     !! single (64-bit) hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: H0, H1, H2, H3, H4, H5
    tLong       :: H6, H7, H8, H9, H10, H11
    tLong       :: Seed0, Partial
    tIndex      :: Length, Remaining, Offset
    tIndex      :: PartialSize, WholeWords, I

!** FLOW

    ! check length and SmallHash if length is short (< 192)
    Length = SIZE(Input)
    IF (Length < SmallLimit) THEN
        HashCode = SmallHash(Input, Length, Seed1, Seed2, HashPair)
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
        MixData(H0,  H2,  H10, H11, H1,  11, Input, Offset)
        MixData(H1,  H3,  H11, H0,  H2,  32, Input, Offset+8)
        MixData(H2,  H4,  H0,  H1,  H3,  43, Input, Offset+16)
        MixData(H3,  H5,  H1,  H2,  H4,  31, Input, Offset+24)
        MixData(H4,  H6,  H2,  H3,  H5,  17, Input, Offset+32)
        MixData(H5,  H7,  H3,  H4,  H6,  28, Input, Offset+40)
        MixData(H6,  H8,  H4,  H5,  H7,  39, Input, Offset+48)
        MixData(H7,  H9,  H5,  H6,  H8,  57, Input, Offset+56)
        MixData(H8,  H10, H6,  H7,  H9,  55, Input, Offset+64)
        MixData(H9,  H11, H7,  H8,  H10, 54, Input, Offset+72)
        MixData(H10, H0,  H8,  H9,  H11, 22, Input, Offset+80)
        MixData(H11, H1,  H9,  H10, H0,  46, Input, Offset+88)
        ! update indices
        Remaining = Remaining - BlockSize
        Offset = Offset + BlockSize
    END DO

    PartialSize = IAND(Remaining, 7)
    WholeWords = SHIFTR(Remaining, 3)
    IF (PartialSize > 0) THEN
        Partial = PackPartial(Input, Offset + SHIFTL(WholeWords, 3), &
                              PartialSize)
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
    IF (WholeWords  >= 11) H10 = H10 + PackFull(Input, Offset+80)
    IF (WholeWords  >= 10) H9  = H9  + PackFull(Input, Offset+72)
    IF (WholeWords  >=  9) H8  = H8  + PackFull(Input, Offset+64)
    IF (WholeWords  >=  8) H7  = H7  + PackFull(Input, Offset+56)
    IF (WholeWords  >=  7) H6  = H6  + PackFull(Input, Offset+48)
    IF (WholeWords  >=  6) H5  = H5  + PackFull(Input, Offset+40)
    IF (WholeWords  >=  5) H4  = H4  + PackFull(Input, Offset+32)
    IF (WholeWords  >=  4) H3  = H3  + PackFull(Input, Offset+24)
    IF (WholeWords  >=  3) H2  = H2  + PackFull(Input, Offset+16)
    IF (WholeWords  >=  2) H1  = H1  + PackFull(Input, Offset+8)
    IF (WholeWords  >=  1) H0  = H0  + PackFull(Input, Offset)

    H11 = H11 + SHIFTL(ToLong(Remaining), 56)

    ! end mixing with 3 iterations
    DO I = 1, 3
        MixH(H11, H1,  H2,  44)
        MixH(H0,  H2,  H3,  15)
        MixH(H1,  H3,  H4,  34)
        MixH(H2,  H4,  H5,  21)
        MixH(H3,  H5,  H6,  38)
        MixH(H4,  H6,  H7,  33)
        MixH(H5,  H7,  H8,  10)
        MixH(H6,  H8,  H9,  13)
        MixH(H7,  H9,  H10, 38)
        MixH(H8,  H10, H11, 53)
        MixH(H9,  H11, H0,  42)
        MixH(H10, H0,  H1,  54)
    END DO

    ! set output
    HashCode = H0
    IF (PRESENT(HashPair)) THEN
        HashPair(1) = H0
        HashPair(2) = H1
    END IF

    RETURN

END FUNCTION Spooky_Hash128

!******************************************************************************

FUNCTION SmallHash(Input, Length, Seed1, Seed2, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code for small input using the SpookyHash (version 2)
    ! hash algorithm by Bob Jenkins.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,           INTENT(IN)     :: Input(0:)    ! input bytes
    tIndex,          INTENT(IN)     :: Length       ! size of input bytes
    tLong,           INTENT(IN)     :: Seed1        ! seed
    tLong, OPTIONAL, INTENT(IN)     :: Seed2        ! seed
    tLong, OPTIONAL, INTENT(OUT)    :: HashPair(2)  ! dual hash codes
    tLong                           :: HashCode     ! single hash code

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
        H2 = H2 + PackFull(Input, Offset)
        H3 = H3 + PackFull(Input, Offset+8)
        ShortMix(H0, H1, H2, H3)
        H0 = H0 + PackFull(Input, Offset+16)
        H1 = H1 + PackFull(Input, Offset+24)
        ! update indices
        Offset = Offset + 32
        Remaining = Remaining - 32
    END DO

    IF (Remaining >= 16) THEN
        H2 = H2 + PackFull(Input, Offset)
        H3 = H3 + PackFull(Input, Offset+8)
        ShortMix(H0, H1, H2, H3)
        ! update indices
        Offset = Offset + 16
        Remaining = Remaining - 16
    END IF

    H3 = H3 + SHIFTL(ToLong(Length), 56)

    IF (Remaining >= 8) THEN
        H2 = H2 + PackFull(Input, Offset)
        ! update indices
        Offset = Offset + 8
        Remaining = Remaining - 8
        IF (Remaining > 0) H3 = H3 + PackPartial(Input, Offset, Remaining)
    ELSEIF (Remaining > 0) THEN
        H2 = H2 + PackPartial(Input, Offset, Remaining)
    ELSE
        H2 = H2 + SC
        H3 = H3 + SC
    END IF

    ! ShortEnd
    MixH2(H3, H2, 15)
    MixH2(H0, H3, 52)
    MixH2(H1, H0, 26)
    MixH2(H2, H1, 51)
    MixH2(H3, H2, 28)
    MixH2(H0, H3,  9)
    MixH2(H1, H0, 47)
    MixH2(H2, H1, 54)
    MixH2(H3, H2, 32)
    MixH2(H0, H3, 25)
    MixH2(H1, H0, 63)

    ! set output
    HashCode = H0
    IF (PRESENT(HashPair)) THEN
        HashPair(1) = H0
        HashPair(2) = H1
    END IF

    RETURN

END FUNCTION SmallHash

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                             +
!                   OPTIMAL VERSIONS OF AUXILIARY ROUTINES                    +
!                                                                             +
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PURE FUNCTION PackFull(Buf, Off) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To pack the array 'Buf' at offset 'Off' into the 64-bit word 'Res',
    ! in little-endian convention (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,  INTENT(IN)  :: Buf(0:)  ! buffer
    tIndex, INTENT(IN)  :: Off      ! offset
    tLong               :: Res      ! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte       :: Input(0:7)
    tLong       :: Output
    EQUIVALENCE (Input, Output)

! FLOW

    ! implementation algorithm #7
    Input(0:7) = Buf(Off:Off+7)
    Res = Output

    RETURN

END FUNCTION PackFull

!******************************************************************************

PURE FUNCTION PackPartial(Buf, Off, Length) RESULT(Res)

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
    Wrk(0:Length-1) = Buf(Off:Off+Length-1)

    ! pack bytes into word
    Res = PackFull(Wrk, 0)

    RETURN

END FUNCTION PackPartial

!******************************************************************************

END SUBMODULE SubBase_SpookyHash_Opt

!******************************************************************************

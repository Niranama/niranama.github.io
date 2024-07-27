
SUBMODULE (ModBase_OptimalHash64) SubBase_PengyHash_Opt

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an optimal implementation for the PengyHash hash algorithm
!   by Alberto Fajardo. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr

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

MODULE FUNCTION PengyV02_Hash64_Opt(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the PengyHash hash algorithm by Alberto Fajardo.
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
    HashCode = Pengy_Hash64_V02(InpPtr, Seed)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION PengyV02_Hash64_Opt

!******************************************************************************

MODULE FUNCTION PengyV03_Hash64_Opt(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the PengyHash hash algorithm by Alberto Fajardo.
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
    HashCode = Pengy_Hash64_V03(InpPtr, Seed)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION PengyV03_Hash64_Opt

!******************************************************************************

FUNCTION Pengy_Hash64_V02(Input, Seed) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the PengyHash hash algorithm by Alberto Fajardo.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, INTENT(IN)       :: Input(0:)    !! input bytes
    tLong, INTENT(IN)       :: Seed         !! seed
    tLong                   :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: B(0:3), S(0:3)
    tByte       :: Partial(0:7)
    tIndex      :: Length, Remaining, Offset, I

!** FLOW

    ! initialize
    Length = SIZE(Input)
    B = 0_kLong
    S = 0_kLong
    S(3) = ToLong(Length)
    Remaining = Length
    Offset = 0

    ! perform hashing
    DO WHILE (Remaining >= 32)
        B(0) = PackLong(Input, Offset)
        B(1) = PackLong(Input, Offset+8)
        B(2) = PackLong(Input, Offset+16)
        B(3) = PackLong(Input, Offset+24)

        S(0) = S(0) + S(1) + B(3)
        S(1) = S(0) + RotateLeft(S(1), 14)
        S(2) = S(2) + S(3) + B(2)
        S(3) = S(2) + RotateLeft(S(3), 23)
        S(0) = S(0) + S(3) + B(1)
        S(3) = IEOR(S(0), RotateLeft(S(3), 16))
        S(2) = S(2) + S(1) + B(0)
        S(1) = IEOR(S(2), RotateLeft(S(1), 40))

        Offset = Offset + 32
        Remaining = Remaining - 32
    END DO

    SELECT CASE (Remaining)
    CASE (24:31)
        B(0) = PackLong(Input, Offset)
        B(1) = PackLong(Input, Offset+8)
        B(2) = PackLong(Input, Offset+16)
        CALL UnpackLong(B(3), Partial, 0)
        Partial(0:Remaining-24-1) = Input(Offset+24:Offset+Remaining-1)
        B(3) = PackLong(Partial, 0)
    CASE (16:23)
        B(0) = PackLong(Input, Offset)
        B(1) = PackLong(Input, Offset+8)
        CALL UnpackLong(B(2), Partial, 0)
        Partial(0:Remaining-16-1) = Input(Offset+16:Offset+Remaining-1)
        B(2) = PackLong(Partial, 0)
    CASE (8:15)
        B(0) = PackLong(Input, Offset)
        CALL UnpackLong(B(1), Partial, 0)
        Partial(0:Remaining-8-1) = Input(Offset+8:Offset+Remaining-1)
        B(1) = PackLong(Partial, 0)
    CASE (1:7)
        CALL UnpackLong(B(0), Partial, 0)
        Partial(0:Remaining-1) = Input(Offset:Offset+Remaining-1)
        B(0) = PackLong(Partial, 0)
    END SELECT

    DO I = 1, 6
        S(0) = S(0) + S(1) + B(3)
        S(1) = S(0) + RotateLeft(S(1), 14) + Seed
        S(2) = S(2) + S(3) + B(2)
        S(3) = S(2) + RotateLeft(S(3), 23)
        S(0) = S(0) + S(3) + B(1)
        S(3) = IEOR(S(0), RotateLeft(S(3), 16))
        S(2) = S(2) + S(1) + B(0)
        S(1) = IEOR(S(2), RotateLeft(S(1), 40))
    END DO

    HashCode = S(0) + S(1) + S(2) + S(3)

    RETURN

END FUNCTION Pengy_Hash64_V02

!******************************************************************************

FUNCTION Pengy_Hash64_V03(Input, Seed) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the PengyHash hash algorithm by Alberto Fajardo.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, INTENT(IN)   :: Input(0:)    !! input bytes
    tLong, INTENT(IN)   :: Seed         !! seed
    tLong               :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: B(0:3), S(0:3)
    tIndex      :: Length, Remaining, Offset, I

!** FLOW

    ! initialize
    Length = SIZE(Input)
    B = 0_kLong
    S = 0_kLong
    S(3) = ToLong(Length)
    Remaining = Length
    Offset = 0

    ! perform hashing
    DO WHILE (Remaining >= 32)

        B(0) = PackLong(Input, Offset)
        B(1) = PackLong(Input, Offset+8)
        B(2) = PackLong(Input, Offset+16)
        B(3) = PackLong(Input, Offset+24)

        S(1) = S(1) + B(1)
        S(0) = S(0) + S(1) + B(0)
        S(1) = IEOR(S(0), RotateLeft(S(1), 14))

        S(3) = S(3) + B(3)
        S(2) = S(2) + S(3) + B(2)
        S(3) = IEOR(S(2), RotateLeft(S(3), 23))

        S(3) = S(3) + B(3)
        S(0) = S(0) + S(3) + B(0)
        S(3) = IEOR(S(0), RotateLeft(S(3), 11))

        S(1) = S(1) + B(1)
        S(2) = S(2) + S(1) + B(2)
        S(1) = IEOR(S(2), RotateLeft(S(1), 40))

        Offset = Offset + 32
        Remaining = Remaining - 32
    END DO

    I = 0
    B = 0_kLong
    DO WHILE (Remaining >= 8)
        B(I) = PackLong(Input, Offset)
        Offset = Offset + 8
        Remaining = Remaining - 8
        I = I + 1
    END DO
    IF (Remaining > 0) B(I) = PackPartial(Input, Offset, Remaining)

    DO I = 1, 6
        S(1) = S(1) + B(1) + Seed
        S(0) = S(0) + S(1) + B(0)
        S(1) = IEOR(S(0), RotateLeft(S(1), 14))

        S(3) = S(3) + B(3)
        S(2) = S(2) + S(3) + B(2)
        S(3) = IEOR(S(2), RotateLeft(S(3), 23))

        S(3) = S(3) + B(3)
        S(0) = S(0) + S(3) + B(0)
        S(3) = IEOR(S(0), RotateLeft(S(3), 9))

        S(1) = S(1) + B(1)
        S(2) = S(2) + S(1) + B(2)
        S(1) = IEOR(S(2), RotateLeft(S(1), 40))
    END DO

    HashCode = S(0) + S(1) + S(2) + S(3)

    RETURN

END FUNCTION Pengy_Hash64_V03

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                             +
!                   OPTIMAL VERSIONS OF AUXILIARY ROUTINES                    +
!                                                                             +
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PURE FUNCTION PackLong(Buf, Off) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To pack the array 'Buf' at Offset 'Off' into the 64-bit word 'Res',
    ! in little-endian convention (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,  INTENT(IN)  :: Buf(0:)  ! buffer
    tIndex, INTENT(IN)  :: Off      ! Offset
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

END FUNCTION PackLong

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
    Res = PackLong(Wrk, 0)

    RETURN

END FUNCTION PackPartial

!******************************************************************************

PURE SUBROUTINE UnPackLong(Val, Buf, Off)

!** PURPOSE OF THIS SUBROUTINE:
    ! To unpack the 64-bit word 'Val' into the array 'Buf' at offset 'Off',
    ! in little-endian convention (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,  INTENT(IN)      :: Val      ! the value to convert
    tByte,  INTENT(INOUT)   :: Buf(0:)  ! the destination buffer
    tIndex, INTENT(IN)      :: Off      ! the destination offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte       :: Output(0:7)
    tLong       :: Input
    EQUIVALENCE (Output, Input)

! FLOW

    ! implementation algorithm #7
    Input = Val
    Buf(Off:Off+7) = Output(0:7)

    RETURN

END SUBROUTINE UnPackLong

!******************************************************************************

END SUBMODULE SubBase_PengyHash_Opt

!******************************************************************************

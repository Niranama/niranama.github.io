
SUBMODULE (ModBase_ExperimentalHash64) SubBase_PengyHash_Exp

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an experimental implementation for the PengyHash hash algorithm
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

MODULE FUNCTION PengyV02_Hash64_Exp(Input, InpSize, Algo, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the PengyHash hash algorithm by Alberto Fajardo.
    !  This is an interface routine that calls the working routine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), DIMENSION(..), INTENT(IN)  :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
    tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-7)
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
    SELECT CASE (Algo)
    CASE (1)
        HashCode = Pengy_Hash64_V02(InpPtr, Seed, Pack_I64_A1, Unpack_I64_A1)
    CASE (2)
        HashCode = Pengy_Hash64_V02(InpPtr, Seed, Pack_I64_A2, Unpack_I64_A1)
    CASE (3)
        HashCode = Pengy_Hash64_V02(InpPtr, Seed, Pack_I64_A3, Unpack_I64_A1)
    CASE (4)
        HashCode = Pengy_Hash64_V02(InpPtr, Seed, Pack_I64_A4, Unpack_I64_A2)
    CASE (5)
        HashCode = Pengy_Hash64_V02(InpPtr, Seed, Pack_I64_A5, Unpack_I64_A3)
    CASE (6)
        HashCode = Pengy_Hash64_V02(InpPtr, Seed, Pack_I64_A6, Unpack_I64_A4)
    CASE (7)
        HashCode = Pengy_Hash64_V02(InpPtr, Seed, Pack_I64_A7, Unpack_I64_A5)
    END SELECT

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION PengyV02_Hash64_Exp

!******************************************************************************

MODULE FUNCTION PengyV03_Hash64_Exp(Input, InpSize, Algo, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the PengyHash hash algorithm by Alberto Fajardo.
    !  This is an interface routine that calls the working routine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), DIMENSION(..), INTENT(IN)  :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
    tLong,    OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
    tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-7)
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
    SELECT CASE (Algo)
    CASE (1)
        HashCode = Pengy_Hash64_V03(InpPtr, Seed, Pack_I64_A1)
    CASE (2)
        HashCode = Pengy_Hash64_V03(InpPtr, Seed, Pack_I64_A2)
    CASE (3)
        HashCode = Pengy_Hash64_V03(InpPtr, Seed, Pack_I64_A3)
    CASE (4)
        HashCode = Pengy_Hash64_V03(InpPtr, Seed, Pack_I64_A4)
    CASE (5)
        HashCode = Pengy_Hash64_V03(InpPtr, Seed, Pack_I64_A5)
    CASE (6)
        HashCode = Pengy_Hash64_V03(InpPtr, Seed, Pack_I64_A6)
    CASE (7)
        HashCode = Pengy_Hash64_V03(InpPtr, Seed, Pack_I64_A7)
    END SELECT

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION PengyV03_Hash64_Exp

!******************************************************************************

FUNCTION Pengy_Hash64_V02(Input, Seed, PackLong, UnpackLong) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the PengyHash hash algorithm by Alberto Fajardo.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, INTENT(IN)       :: Input(0:)    !! input bytes
    tLong, INTENT(IN)       :: Seed         !! seed
    PROCEDURE(Pack_I64)     :: PackLong     !! procedure to convert a byte array to 64-bit integer
    PROCEDURE(Unpack_I64)   :: UnpackLong   !! procedure to convert 64-bit integer to a byte array
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

FUNCTION Pengy_Hash64_V03(Input, Seed, PackFull) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the PengyHash hash algorithm by Alberto Fajardo.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, INTENT(IN)   :: Input(0:)    !! input bytes
    tLong, INTENT(IN)   :: Seed         !! seed
    PROCEDURE(Pack_I64) :: PackFull     !! procedure to convert a byte array to 64-bit integer
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
        
        B(0) = PackFull(Input, Offset)
        B(1) = PackFull(Input, Offset+8)
        B(2) = PackFull(Input, Offset+16)
        B(3) = PackFull(Input, Offset+24)
        
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
        B(I) = PackFull(Input, Offset)
        Offset = Offset + 8
        Remaining = Remaining - 8
        I = I + 1
    END DO
    IF (Remaining > 0) B(I) = PackPartial(Input, Offset, Remaining, PackFull)
    
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

!******************************************************************************

END SUBMODULE SubBase_PengyHash_Exp

!******************************************************************************


SUBMODULE (ModBase_ReferenceHash32) SubBase_Lookup3Hash_Ref

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains a reference implementation for the Lookup3 hash algorithm
!   by Bob Jenkins. <br>

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

MODULE FUNCTION Lookup3Hash_I32(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the Lookup3 hash algorithm by Bob Jenkins.
    !  This is an interface routine that calls the working routine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), DIMENSION(..), INTENT(IN)  :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
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
    HashCode = Lookup3_Hash32(InpPtr, Seed, ByteConv)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION Lookup3Hash_I32

!******************************************************************************

FUNCTION Lookup3_Hash32(Input, Seed, BC) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the Lookup3 hash algorithm by Bob Jenkins.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN) :: Input(0:)    !! input bytes
    tInteger,            INTENT(IN) :: Seed         !! seed
    TYPE(ByteConverter), INTENT(IN) :: BC           !! byte converter
    tInteger                        :: HashCode     !! hash code
    
!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: InitParam = ToInteger(Z'DEADBEEF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: A, B, C
    tIndex      :: Length, Remaining, Offset

!** FLOW

    ! initialize
    Length = SIZE(Input, KIND=kIndex)
    Remaining = Length
    Offset = 0_kIndex
    A = InitParam + ToInteger(Length) + Seed
    B = A
    C = A

    ! perform hashing
    DO WHILE (Remaining > 12_kIndex)
        ! add input to internal states
        A = A + BC%Pack_I32(Input, Offset)
        B = B + BC%Pack_I32(Input, Offset+4_kIndex)
        C = C + BC%Pack_I32(Input, Offset+8_kIndex)
        ! mix internal states
        CALL IntermediateMix(A, B, C)
        ! update indices
        Remaining = Remaining - 12_kIndex
        Offset = Offset + 12_kIndex
    END DO

    SELECT CASE (Remaining)
    CASE (9_kIndex:12_kIndex)
        A = A + BC%Pack_I32(Input, Offset)
        B = B + BC%Pack_I32(Input, Offset+4_kIndex)
        IF (Remaining == 12_kIndex) THEN
            C = C + BC%Pack_I32(Input, Offset+8_kIndex)
        ELSE
            C = C + BC%Pack_I32_Partial(Input, Offset+8_kIndex, Remaining-8_kIndex)
        END IF
    CASE (5_kIndex:8_kIndex)
        A = A + BC%Pack_I32(Input, Offset)
        IF (Remaining == 8_kIndex) THEN
            B = B + BC%Pack_I32(Input, Offset+4_kIndex)
        ELSE
            B = B + BC%Pack_I32_Partial(Input, Offset+4_kIndex, Remaining-4_kIndex)
        END IF
    CASE (1_kIndex:4_kIndex)
        IF (Remaining == 4_kIndex) THEN
            A = A + BC%Pack_I32(Input, Offset)
        ELSE
            A = A + BC%Pack_I32_Partial(Input, Offset, Remaining)
        END IF
    CASE (0)
        ! return the hash for zero-length input (that requires no mixing)
        HashCode = C
        RETURN
    END SELECT

    ! final mixing of internal states
    CALL FinalMix(A, B, C)

    ! return the hash
    HashCode = C

    RETURN

END FUNCTION Lookup3_Hash32

!******************************************************************************

SUBROUTINE IntermediateMix(A, B, C)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform intermediate mixing of working variables.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(INOUT) :: A, B, C

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    A = A - C
    A = IEOR(A, RotateLeft(C, 4))
    C = C + B
    B = B - A
    B = IEOR(B, RotateLeft(A, 6))
    A = A + C
    C = C - B
    C = IEOR(C, RotateLeft(B, 8))
    B = B + A
    A = A - C
    A = IEOR(A, RotateLeft(C,16))
    C = C + B
    B = B - A
    B = IEOR(B, RotateLeft(A,19))
    A = A + C
    C = C - B
    C = IEOR(C, RotateLeft(B, 4))
    B = B + A

    RETURN

END SUBROUTINE IntermediateMix

!******************************************************************************

SUBROUTINE FinalMix(A, B, C)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform final mixing of working variables.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(INOUT) :: A, B, C

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    C = IEOR(C, B)
    C = C - RotateLeft(B,14)
    A = IEOR(A, C)
    A = A - RotateLeft(C,11)
    B = IEOR(B, A)
    B = B - RotateLeft(A,25)
    C = IEOR(C, B)
    C = C - RotateLeft(B,16)
    A = IEOR(A, C)
    A = A - RotateLeft(C,4)
    B = IEOR(B, A)
    B = B - RotateLeft(A,14)
    C = IEOR(C, B)
    C = C - RotateLeft(B,24)

    RETURN

END SUBROUTINE FinalMix

!******************************************************************************

END SUBMODULE SubBase_Lookup3Hash_Ref

!******************************************************************************

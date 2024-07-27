
SUBMODULE (ModBase_OptimalHash32) SubBase_Lookup3Hash_Opt

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an optimal implementation for the Lookup3 hash algorithm
!   by Bob Jenkins. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define IntermediateMix(A, B, C) \
    A = A - C; \
    A = IEOR(A, RotateLeft(C, 4)); \
    C = C + B; \
    B = B - A; \
    B = IEOR(B, RotateLeft(A, 6)); \
    A = A + C; \
    C = C - B; \
    C = IEOR(C, RotateLeft(B, 8)); \
    B = B + A; \
    A = A - C; \
    A = IEOR(A, RotateLeft(C,16)); \
    C = C + B; \
    B = B - A; \
    B = IEOR(B, RotateLeft(A,19)); \
    A = A + C; \
    C = C - B; \
    C = IEOR(C, RotateLeft(B, 4)); \
    B = B + A;
#define FinalMix(A, B, C) \
    C = IEOR(C, B); \
    C = C - RotateLeft(B,14); \
    A = IEOR(A, C); \
    A = A - RotateLeft(C,11); \
    B = IEOR(B, A); \
    B = B - RotateLeft(A,25); \
    C = IEOR(C, B); \
    C = C - RotateLeft(B,16); \
    A = IEOR(A, C); \
    A = A - RotateLeft(C,4); \
    B = IEOR(B, A); \
    B = B - RotateLeft(A,14); \
    C = IEOR(C, B); \
    C = C - RotateLeft(B,24);
#define MaskInteger(X)  IAND(ToInteger(X), Z'000000FF')

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

MODULE FUNCTION Lookup3_Hash32_Opt(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

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

!** FLOW

    ! initialize
    SET_OPTION(Seed, 0_kInteger, StartHash)
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)

    ! perform hashing
    HashCode = Lookup3_Hash32(InpPtr, Seed)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION Lookup3_Hash32_Opt

!******************************************************************************

FUNCTION Lookup3_Hash32(Input, Seed) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the Lookup3 hash algorithm by Bob Jenkins.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)    :: Input(0:)    !! input bytes
    tInteger, INTENT(IN)    :: Seed         !! seed
    tInteger                :: HashCode     !! hash code
    
!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: InitParam = ToInteger(Z'DEADBEEF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: A, B, C
    tIndex      :: Length, Remaining, Offset

!** FLOW
    
    ! initialize
    Length = SIZE(Input)
    Remaining = Length
    Offset = 0
    A = InitParam + ToInteger(Length) + Seed
    B = A
    C = A

    ! perform hashing
    DO WHILE (Remaining > 12)
        ! add input to internal states
        A = A + PackFull(Input, Offset)
        B = B + PackFull(Input, Offset+4)
        C = C + PackFull(Input, Offset+8)
        ! mix internal states
        IntermediateMix(A, B, C)
        ! update indices
        Remaining = Remaining - 12
        Offset = Offset + 12
    END DO
        
    SELECT CASE (Remaining)
    CASE (9:12)
        A = A + PackFull(Input, Offset)
        B = B + PackFull(Input, Offset+4)
        IF (Remaining == 12) THEN
            C = C + PackFull(Input, Offset+8)
        ELSE
            C = C + PackPartial(Input, Offset+8, Remaining-8, PackFull)
        END IF
    CASE (5:8)
        A = A + PackFull(Input, Offset)
        IF (Remaining == 8) THEN
            B = B + PackFull(Input, Offset+4)
        ELSE
            B = B + PackPartial(Input, Offset+4, Remaining-4, PackFull)
        END IF
    CASE (1:4)
        IF (Remaining == 4) THEN
            A = A + PackFull(Input, Offset)
        ELSE
            A = A + PackPartial(Input, Offset, Remaining, PackFull)
        END IF
    CASE (0)
        ! return the hash for zero-length input (that requires no mixing)
        HashCode = C
        RETURN
    END SELECT
        
    ! final mixing of internal states
    FinalMix(A, B, C)
        
    ! return the hash
    HashCode = C

    RETURN

CONTAINS

    FUNCTION PackFull(ByteArr, Offset) RESULT(Res)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To convert an array of 8-bit integers starting at the offset to
        !  a 32-bit integer value.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte, TARGET, INTENT(IN)   :: ByteArr(0:)  !! byte array
        tIndex,        INTENT(IN)   :: Offset       !! offset
        tInteger                    :: Res          !! result

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW

        ! implementation algorithm #4
        Res = 0
        CALL MVBITS(MaskInteger(ByteArr(Offset)),   0, 8, Res,  0)
        CALL MVBITS(MaskInteger(ByteArr(Offset+1)), 0, 8, Res,  8)
        CALL MVBITS(MaskInteger(ByteArr(Offset+2)), 0, 8, Res, 16)
        CALL MVBITS(MaskInteger(ByteArr(Offset+3)), 0, 8, Res, 24)

        RETURN

    END FUNCTION PackFull

    !**************************************************************************

END FUNCTION Lookup3_Hash32

!******************************************************************************

END SUBMODULE SubBase_Lookup3Hash_Opt

!******************************************************************************


SUBMODULE (ModBase_ReferenceHash32) SubBase_FastHash_Ref

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains a reference implementation for the Fast hash algorithm
!   by Zilong Tan. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    USE Class_ByteConverter
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tLong, PARAMETER    :: C = ToLong(Z'2127599BF4325C37')
    tLong, PARAMETER    :: M = ToLong(Z'880355F21E6D1965')
    tLong, PARAMETER    :: LONG_LO_MASK = ToLong(Z'00000000FFFFFFFF')

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE FUNCTION FastHash_I32(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the Fast hash algorithm by Zilong Tan.
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
    HashCode = Fast_Hash32(InpPtr, Seed, ByteConv)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION FastHash_I32

!******************************************************************************

FUNCTION Fast_Hash32(Input, Seed, BC) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the Fast hash algorithm by Zilong Tan.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN) :: Input(0:)    !! input bytes
    tInteger,            INTENT(IN) :: Seed         !! seed
    TYPE(ByteConverter), INTENT(IN) :: BC           !! byte converter
    tInteger                        :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: H, V
    tIndex      :: Length, Remaining, Offset

!** FLOW

    ! initialize
    Length = SIZE(Input)
    Remaining = Length
    Offset = 0
    H = IEOR(ToLong(Seed), ToLong(Length)*M)

    DO WHILE (Remaining >= 8)
        ! get input
        V = BC%Pack_I64(Input, Offset)
        ! perform mixing
        CALL MixAC(V)
        H = IEOR(H, V)
        H = H*M
        ! update indices
        Offset = Offset + 8
        Remaining = Remaining - 8
    END DO

    IF (Remaining > 0) THEN
        ! get input
        V = BC%Pack_I64_Partial(Input, Offset, Remaining)
        ! perform mixing
        CALL MixAC(V)
        H = IEOR(H, V)
        H = H*M
    END IF

    ! perform final mixing
    CALL MixAC(H)

    ! return 32-bit output
    ! the following trick converts the 64-bit hash code to Fermat
    ! residue, which shall retain information from both the higher
    ! and lower parts of hash code.
    HashCode = ToInteger(IAND(H - SHIFTR(H, 32), LONG_LO_MASK))

    RETURN

END FUNCTION Fast_Hash32

!******************************************************************************

SUBROUTINE MixAC(A)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform mixing

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(INOUT)    :: A

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    A = IEOR(A, SHIFTR(A, 23))
    A = A*C
    A = IEOR(A, SHIFTR(A, 47))

    RETURN

END SUBROUTINE MixAC

!******************************************************************************

END SUBMODULE SubBase_FastHash_Ref

!******************************************************************************

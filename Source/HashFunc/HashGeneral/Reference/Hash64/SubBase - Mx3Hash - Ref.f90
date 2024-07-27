
SUBMODULE (ModBase_ReferenceHash64) SubBase_Mx3Hash_Ref

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains a reference implementation for the Mx3Hash hash algorithm
!   by Jon Maiga. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    USE Class_ByteConverter
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tLong, PARAMETER    :: C = ToLong(Z'BEA225F9EB34556D')

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE FUNCTION Mx3Hash_I64(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the Mx3Hash hash algorithm by Jon Maiga.
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
    TYPE(ByteConverter) :: ByteConv

!** FLOW

    ! initialize
    SET_OPTION(Seed, 0_kInteger, StartHash)
    CALL ByteConv%Initialize()
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)

    ! perform hashing
    HashCode = Mx3_Hash64(InpPtr, Seed, ByteConv)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION Mx3Hash_I64

!******************************************************************************

FUNCTION Mx3_Hash64(Input, Seed, BC) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the Mx3Hash hash algorithm by Jon Maiga.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN) :: Input(0:)    !! input bytes
    tLong,               INTENT(IN) :: Seed         !! seed
    TYPE(ByteConverter), INTENT(IN) :: BC           !! byte converter
    tLong                           :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: H, X0
    tIndex      :: Length, Remaining, Offset

!** FLOW

    ! initialize
    Length = SIZE(Input)
    Remaining = Length
    Offset = 0
    H  = IEOR(Seed, ToLong(Length))

    ! perform hashing
    DO WHILE (Remaining >= 32)
        ! get input and mix them with constant
        X0 = BC%Pack_I64(Input, Offset)
        CALL MixStream(H, X0)
        X0 = BC%Pack_I64(Input, Offset + 8)
        CALL MixStream(H, X0)
        X0 = BC%Pack_I64(Input, Offset + 16)
        CALL MixStream(H, X0)
        X0 = BC%Pack_I64(Input, Offset + 24)
        CALL MixStream(H, X0)
        ! update indices
        Offset = Offset + 32
        Remaining = Remaining - 32
    END DO

    DO WHILE (Remaining >= 8)
        ! get input and  mix it with constant
        X0 = BC%Pack_I64(Input, Offset)
        CALL MixStream(H, X0)
        ! update indices
        Offset = Offset + 8
        Remaining = Remaining - 8
    END DO

    ! get input (remaining bytes) and mix it with constant
    IF (Remaining > 0) THEN
        X0 = BC%Pack_I64_Partial(Input, Offset, Remaining)
        CALL MixStream(H, X0)
    END IF

    ! finish
    CALL FinalMix(H)
    HashCode = H

    RETURN

END FUNCTION Mx3_Hash64

!******************************************************************************

SUBROUTINE MixStream(H, X)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform stream mixing.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(INOUT) :: H, X

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    X = X*C
    X = IEOR(X, IEOR(SHIFTR(X, 57), SHIFTR(X, 33)))
    X = X*C
    H = H + X
    H = H*C

    RETURN

END SUBROUTINE MixStream

!******************************************************************************

SUBROUTINE FinalMix(X)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform final mixing.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(INOUT) :: X

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    X = X*C
    X = IEOR(X, SHIFTR(X, 33))
    X = X*C
    X = IEOR(X, SHIFTR(X, 29))
    X = X*C
    X = IEOR(X, SHIFTR(X, 39))

    RETURN

END SUBROUTINE FinalMix

!******************************************************************************

END SUBMODULE SubBase_Mx3Hash_Ref

!******************************************************************************

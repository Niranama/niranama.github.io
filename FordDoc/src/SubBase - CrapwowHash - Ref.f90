
SUBMODULE (ModBase_ReferenceHash32) SubBase_CrapWowHash_Ref

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains a reference implementation for the CrapWow hash algorithm. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    USE Class_ByteConverter
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tInteger, PARAMETER     :: CWOW_32_M = ToInteger(Z'57559429')
    tInteger, PARAMETER     :: CWOW_32_N = ToInteger(Z'5052ACDB')
    tLong,    PARAMETER     :: LONG_LO_MASK = ToLong(Z'00000000FFFFFFFF')

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE FUNCTION CrapWowHash_I32(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the CrapWow hash algorithm.
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
    HashCode = CrapWow_Hash32(InpPtr, Seed, ByteConv)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION CrapWowHash_I32

!******************************************************************************

FUNCTION CrapWow_Hash32(Input, Seed, BC) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the CrapWow hash algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN) :: Input(0:)    !! input bytes
    tInteger,            INTENT(IN) :: Seed         !! seed
    TYPE(ByteConverter), INTENT(IN) :: BC           !! byte converter
    tInteger                        :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: H, K, Inp
    tIndex      :: Length, Remaining, Offset

!** FLOW

    ! initialize
    Length = SIZE(Input)
    Remaining = Length
    Offset = 0
    H = ToInteger(Length)
    K = H + Seed + CWOW_32_N

    DO WHILE (Remaining >= 8)
        ! CWMixB
        CALL CWFold(BC%Pack_I32(Input, Offset), CWOW_32_N, H, K)
        ! CWMixA
        CALL CWFold(BC%Pack_I32(Input, Offset + 4), CWOW_32_M, K, H)
        ! update indices
        Offset = Offset + 8
        Remaining = Remaining - 8
    END DO

    DO WHILE (Remaining >= 4)
        ! CWMixB
        CALL CWFold(BC%Pack_I32(Input, Offset), CWOW_32_N, H, K)
        ! update indices
        Offset = Offset + 4
        Remaining = Remaining - 4
    END DO

    IF (Remaining > 0) THEN
        ! CWMixA
        Inp = IAND(BC%Pack_I32_Partial(Input, Offset, Remaining), SHIFTL(1, Length*8) - 1)
        CALL CWFold(Inp, CWOW_32_M, K, H)
    END IF

    ! CWMixB
    CALL CWFold(IEOR(H, K + CWOW_32_N), CWOW_32_N, H, K)

    ! get output
    HashCode = IEOR(K, H)

    RETURN

END FUNCTION CrapWow_Hash32

!******************************************************************************

SUBROUTINE CWFold(A, B, Lo, Hi) 

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform mixing

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: A, B
    tInteger, INTENT(OUT)   :: Lo, Hi

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: P

!** FLOW
    
    P  = ToUnsigned(A)*ToLong(B)
    Lo = IEOR(Lo, ToInteger(IAND(P, LONG_LO_MASK)))
    Hi = IEOR(Hi, ToInteger(SHIFTR(P, 32)))

    RETURN

CONTAINS

    FUNCTION ToUnsigned(Input) RESULT(Output)
        tInteger    :: Input
        tLong       :: Output
        IF (Input >= 0) THEN
            Output = ToLong(Input)
        ELSE
            Output = 2_kLong**32 + ToLong(Input)
        END IF
    END FUNCTION

    !**************************************************************************

END SUBROUTINE CWFold

!******************************************************************************

END SUBMODULE SubBase_CrapWowHash_Ref

!******************************************************************************

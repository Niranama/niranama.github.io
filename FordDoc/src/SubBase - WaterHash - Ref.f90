
SUBMODULE (ModBase_ReferenceHash32) SubBase_WaterHash_Ref

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains a reference implementation for the Water hash algorithm
!   by Tommy Ettinger. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    USE Class_ByteConverter
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define MaskI32(X)          IAND(ToLong(X), Z'00000000FFFFFFFF')
#define WaterR08(Inp,Off)   MaskI32(BC%Get_U8(Inp, Off))
#define WaterR16(Inp,Off)   MaskI32(BC%Pack_U16(Inp, Off))
#define WaterR32(Inp,Off)   BC%Pack_U32(Inp, Off)

!** MODULE PARAMETERS:
    tLong, PARAMETER    :: WaterP0 = ToLong(Z'00000000A0761D65')
    tLong, PARAMETER    :: WaterP1 = ToLong(Z'00000000E7037ED1')
    tLong, PARAMETER    :: WaterP2 = ToLong(Z'000000008EBC6AF1')
    tLong, PARAMETER    :: WaterP3 = ToLong(Z'00000000589965CD')
    tLong, PARAMETER    :: WaterP4 = ToLong(Z'000000001D8E4E27')
    tLong, PARAMETER    :: WaterP5 = ToLong(Z'00000000EB44ACCB')
    tLong, PARAMETER    :: LONG_LO_MASK = ToLong(Z'00000000FFFFFFFF')

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE FUNCTION WaterHash_I32(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the Water hash algorithm by Tommy Ettinger.
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
    HashCode = Water_Hash32(InpPtr, Seed, ByteConv)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION WaterHash_I32

!******************************************************************************

MODULE FUNCTION WaterHash_I32_New(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the Water hash algorithm by Tommy Ettinger.
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
    HashCode = Water_Hash32_New(InpPtr, Seed, ByteConv)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION WaterHash_I32_New

!******************************************************************************

FUNCTION Water_Hash32(Input, Seed, BC) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the Water hash algorithm by Tommy Ettinger.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN) :: Input(0:)    !! input bytes
    tInteger,            INTENT(IN) :: Seed         !! seed
    TYPE(ByteConverter), INTENT(IN) :: BC           !! byte converter
    tInteger                        :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: H
    tIndex      :: Length, Remaining, Offset

!** FLOW

    ! initialize
    Length = SIZE(Input)
    Remaining = Length
    Offset = 0
    H = Seed

    DO WHILE (Remaining >= 16)
        H = WaterMum(WaterMum(IEOR(WaterR32(Input, Offset),      WaterP1),      &
                              IEOR(WaterR32(Input, Offset + 4),  WaterP2)) + H, &
                     WaterMum(IEOR(WaterR32(Input, Offset + 8),  WaterP3),      &
                              IEOR(WaterR32(Input, Offset + 12), WaterP4)))
        ! update indices
        Offset = Offset + 16
        Remaining = Remaining - 16
    END DO
    H = H + WaterP5

    SELECT CASE(Remaining)
    CASE(1)
        H = WaterMum(IEOR(WaterP2, H), IEOR(WaterR08(Input, Offset), WaterP1))
    CASE(2)
        H = WaterMum(IEOR(WaterP3, H), IEOR(WaterR16(Input, Offset), WaterP4))
    CASE(3)
        H = WaterMum(IEOR(WaterR16(Input, Offset), H), &
                     IEOR(WaterR08(Input, Offset + 2), WaterP2))
    CASE(4)
        H = WaterMum(IEOR(WaterR16(Input, Offset), H), &
                     IEOR(WaterR16(Input, Offset + 2), WaterP3))
    CASE(5)
        H = WaterMum(IEOR(WaterR32(Input, Offset), H), &
                     IEOR(WaterR08(Input, Offset + 4), WaterP1))
    CASE(6)
        H = WaterMum(IEOR(WaterR32(Input, Offset), H), &
                     IEOR(WaterR16(Input, Offset + 4), WaterP1))
    CASE(7)
        H = WaterMum(IEOR(WaterR32(Input, Offset), H), &
                     IEOR(IOR(SHIFTL(WaterR16(Input, Offset + 4), 8), &
                            WaterR08(Input, Offset + 6)), WaterP1))
    CASE(8)
        H = WaterMum(IEOR(WaterR32(Input, Offset), H), &
                     IEOR(WaterR32(Input, Offset + 4), WaterP0))
    CASE(9)
        H = IEOR(WaterMum(IEOR(WaterR32(Input, Offset), H), &
                          IEOR(WaterR32(Input, Offset + 4), WaterP2)), &
                 WaterMum(IEOR(H, WaterP4), IEOR(WaterR08(Input, Offset + 8), WaterP3)))
    CASE(10)
        H = IEOR(WaterMum(IEOR(WaterR32(Input, Offset), H), &
                          IEOR(WaterR32(Input, Offset + 4), WaterP2)), &
                 WaterMum(H, IEOR(WaterR16(Input, Offset + 8), WaterP3)))
    CASE(11)
        H = IEOR(WaterMum(IEOR(WaterR32(Input, Offset), H), &
                          IEOR(WaterR32(Input, Offset + 4), WaterP2)), &
                 WaterMum(H, IEOR(IOR(SHIFTL(WaterR16(Input, Offset + 8),8), &
                          WaterR08(Input, Offset + 10)), WaterP3)))
    CASE(12)
        H = IEOR(WaterMum(IEOR(WaterR32(Input, Offset), H), &
                          IEOR(WaterR32(Input, Offset + 4), WaterP2)), &
                 WaterMum(IEOR(H, WaterR32(Input, Offset + 8)), WaterP4))
    CASE(13)
        H = IEOR(WaterMum(IEOR(WaterR32(Input, Offset), H), &
                          IEOR(WaterR32(Input, Offset + 4), WaterP2)), &
                 WaterMum(IEOR(H, WaterR32(Input, Offset + 8)), &
                          IEOR(WaterR08(Input, Offset + 12), WaterP4)))
    CASE(14)
        H = IEOR(WaterMum(IEOR(WaterR32(Input, Offset), H), &
                          IEOR(WaterR32(Input, Offset + 4), WaterP2)), &
                 WaterMum(IEOR(H, WaterR32(Input, Offset + 8)), &
                          IEOR(WaterR16(Input, Offset + 12), WaterP4)))
    CASE(15)
        H = IEOR(WaterMum(IEOR(WaterR32(Input, Offset), H), &
                          IEOR(WaterR32(Input, Offset + 4), WaterP2)), &
                 WaterMum(IEOR(H, WaterR32(Input, Offset + 8)), &
                          IEOR(IOR(SHIFTL(WaterR16(Input, Offset + 12),8), &
                               WaterR08(Input, Offset + 14)), WaterP4)))
    END SELECT

    H = IEOR(H, SHIFTL(H,16))*IEOR(ToLong(Length), WaterP0)
    HashCode = ToInteger(IAND(H - SHIFTR(H, 32), LONG_LO_MASK))

    RETURN
    
    CONTAINS

    FUNCTION WaterMum(A, B) RESULT(R)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication and mixing.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(IN)   :: A, B
        tLong               :: R

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        R = A*B
        R = R - SHIFTR(R, 32)

        RETURN

    END FUNCTION WaterMum

    !**************************************************************************

END FUNCTION Water_Hash32

!******************************************************************************

FUNCTION Water_Hash32_New(Input, Seed, BC) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the Water hash algorithm by Tommy Ettinger.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN) :: Input(0:)    !! input bytes
    tInteger,            INTENT(IN) :: Seed         !! seed
    TYPE(ByteConverter), INTENT(IN) :: BC           !! byte converter
    tInteger                        :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: H, LHS, RHS
    tIndex      :: Length, Remaining, Offset

!** FLOW

    ! initialize
    Length = SIZE(Input)
    Remaining = Length
    Offset = 0
    H = Seed

    DO WHILE (Remaining >= 16)
        CALL WaterMum(IEOR(WaterR32(Input, Offset),      WaterP1), &
                      IEOR(WaterR32(Input, Offset + 4),  WaterP2), LHS)
        CALL WaterMum(IEOR(WaterR32(Input, Offset + 8),  WaterP3), &
                      IEOR(WaterR32(Input, Offset + 12), WaterP4), RHS)
        CALL WaterMum(LHS + H, RHS, H)
        ! update indices
        Offset = Offset + 16
        Remaining = Remaining - 16
    END DO
    H = H + WaterP5

    SELECT CASE(Remaining)
    CASE(1)
        CALL WaterMum(IEOR(WaterP2, H), IEOR(WaterR08(Input, Offset), WaterP1), H)
    CASE(2)
        CALL WaterMum(IEOR(WaterP3, H), IEOR(WaterR16(Input, Offset), WaterP4), H)
    CASE(3)
        CALL WaterMum(IEOR(WaterR16(Input, Offset), H), &
                      IEOR(WaterR08(Input, Offset + 2), WaterP2), H)
    CASE(4)
        CALL WaterMum(IEOR(WaterR16(Input, Offset), H), &
                      IEOR(WaterR16(Input, Offset + 2), WaterP3), H)
    CASE(5)
        CALL WaterMum(IEOR(WaterR32(Input, Offset), H), &
                      IEOR(WaterR08(Input, Offset + 4), WaterP1), H)
    CASE(6)
        CALL WaterMum(IEOR(WaterR32(Input, Offset), H), &
                      IEOR(WaterR16(Input, Offset + 4), WaterP1), H)
    CASE(7)
        CALL WaterMum(IEOR(WaterR32(Input, Offset), H), &
                      IEOR(IOR(SHIFTL(WaterR16(Input, Offset + 4), 8), &
                           WaterR08(Input, Offset + 6)), WaterP1), H)
    CASE(8)
        CALL WaterMum(IEOR(WaterR32(Input, Offset), H), &
                    IEOR(WaterR32(Input, Offset + 4), WaterP0), H)
    CASE(9)
        CALL WaterMum(IEOR(WaterR32(Input, Offset), H), &
                      IEOR(WaterR32(Input, Offset + 4), WaterP2), LHS)
        CALL WaterMum(IEOR(H, WaterP4), IEOR(WaterR08(Input, Offset + 8), WaterP3), RHS)
        H = IEOR(LHS, RHS)
    CASE(10)
        CALL WaterMum(IEOR(WaterR32(Input, Offset), H), &
                      IEOR(WaterR32(Input, Offset + 4), WaterP2), LHS)
        CALL WaterMum(H, IEOR(WaterR16(Input, Offset + 8), WaterP3), RHS)
        H = IEOR(LHS, RHS)
    CASE(11)
        CALL WaterMum(IEOR(WaterR32(Input, Offset), H), &
                      IEOR(WaterR32(Input, Offset + 4), WaterP2), LHS)
        CALL WaterMum(H, IEOR(IOR(SHIFTL(WaterR16(Input, Offset + 8),8), &
                              WaterR08(Input, Offset + 10)), WaterP3), RHS)
        H = IEOR(LHS, RHS)
    CASE(12)
        CALL WaterMum(IEOR(WaterR32(Input, Offset), H), &
                      IEOR(WaterR32(Input, Offset + 4), WaterP2), LHS)
        CALL WaterMum(IEOR(H, WaterR32(Input, Offset + 8)), WaterP4, RHS)
        H = IEOR(LHS, RHS)
    CASE(13)
        CALL WaterMum(IEOR(WaterR32(Input, Offset), H), &
                      IEOR(WaterR32(Input, Offset + 4), WaterP2), LHS)
        CALL WaterMum(IEOR(H, WaterR32(Input, Offset + 8)), &
                      IEOR(WaterR08(Input, Offset + 12), WaterP4), RHS)
        H = IEOR(LHS, RHS)
    CASE(14)
        CALL WaterMum(IEOR(WaterR32(Input, Offset), H), &
                      IEOR(WaterR32(Input, Offset + 4), WaterP2), LHS)
        CALL WaterMum(IEOR(H, WaterR32(Input, Offset + 8)), &
                      IEOR(WaterR16(Input, Offset + 12), WaterP4), RHS)
        H = IEOR(LHS, RHS)
    CASE(15)
        CALL WaterMum(IEOR(WaterR32(Input, Offset), H), &
                      IEOR(WaterR32(Input, Offset + 4), WaterP2), LHS)
        CALL WaterMum(IEOR(H, WaterR32(Input, Offset + 8)), &
                      IEOR(IOR(SHIFTL(WaterR16(Input, Offset + 12),8), &
                           WaterR08(Input, Offset + 14)), WaterP4), RHS)
        H = IEOR(LHS, RHS)
    END SELECT

    H = IEOR(H, SHIFTL(H,16))*IEOR(ToLong(Length), WaterP0)
    HashCode = ToInteger(IAND(H - SHIFTR(H, 32), LONG_LO_MASK))

    RETURN

    CONTAINS

    SUBROUTINE WaterMum(A, B, R)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication and mixing.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(IN)   :: A, B
        tLong, INTENT(OUT)  :: R

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        R = A*B
        R = R - SHIFTR(R, 32)

        RETURN

    END SUBROUTINE WaterMum

    !**************************************************************************

END FUNCTION Water_Hash32_New

!******************************************************************************

END SUBMODULE SubBase_WaterHash_Ref

!******************************************************************************

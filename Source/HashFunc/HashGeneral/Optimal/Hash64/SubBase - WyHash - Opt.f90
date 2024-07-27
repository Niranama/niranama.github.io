
SUBMODULE (ModBase_OptimalHash64) SubBase_WyHash_Opt

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an optimal implementation for the WyHash hash algorithm
!   by Wang Yi. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    USE ModBase_UIntUtil,   ONLY: UMul128_Upper64
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define MaskI32(X)      IAND(ToLong(X), ToLong(Z'00000000FFFFFFFF'))
#define MaskI8(X)       IAND(ToLong(X), ToLong(Z'00000000000000FF'))

!** MODULE PARAMETERS:
    tLong, PARAMETER    :: WyP0 = ToLong(Z'A0761D6478BD642F')
    tLong, PARAMETER    :: WyP1 = ToLong(Z'E7037ED1A0B428DB')
    tLong, PARAMETER    :: WyP2 = ToLong(Z'8EBC6AF09C88C6E3')
    tLong, PARAMETER    :: WyP3 = ToLong(Z'589965CC75374CC3')
    tLong, PARAMETER    :: WyP4 = ToLong(Z'1D8E4E27C47D124F')

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE FUNCTION Wy_Hash64_Opt(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code using version 3 (?) of the WyHash hash algorithm by Wang Yi.
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
    HashCode = WyV3_Hash64(InpPtr, Seed)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION Wy_Hash64_Opt

!******************************************************************************

MODULE FUNCTION WyF3_Hash64_Opt(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code using the final version 3 of the WyHash hash algorithm by Wang Yi.
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
    HashCode = WyF3_Hash64(InpPtr, Seed)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION WyF3_Hash64_Opt

!******************************************************************************

FUNCTION WyV3_Hash64(Input, Seed) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using version 3 (?) of the WyHash hash algorithm by Wang Yi.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, INTENT(IN)   :: Input(0:)    !! input bytes
    tLong, INTENT(IN)   :: Seed         !! seed
    tLong               :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong           :: Seed1, Seed2, LHS1, RHS1, LHS2, RHS2
    tIndex          :: I, Length, P, Offset

!** SUBROUTINE MACRO DEFINITIONS:
#define Pack_U32(Buf,Index)     MaskI32(PackInteger(Buf, Index))
#define U64Rotate32(Buf,Index)  IOR(SHIFTL(Pack_U32(Buf, Index), 32), Pack_U32(Buf, Index + 4))
#define WyMum(LHS, RHS)         UnsignedLongMultiplyOrFold(LHS, RHS)
#define WyR3(Buf,Index,K)                                     \
    IOR(IOR(SHIFTL(MaskI8(Buf(Index)), 16),                 \
            SHIFTL(MaskI8(Buf(Index + SHIFTR(K, 1))), 8)),  \
                    MaskI8(Buf(Index + K - 1)))

!** FLOW

    ! initialize
    Length = SIZE(Input)
    Offset = 0
    Seed1  = Seed

    ! perform hashing
    IF (Length <= 0) THEN
        HashCode = 0_kLong
    ELSEIF (Length < 4) THEN
        LHS1 = IEOR(IEOR(WyR3(Input, Offset, Length), Seed1), WyP0)
        RHS1 = IEOR(Seed1, WyP1)
        LHS2 = IEOR(WyMum(LHS1, RHS1), Seed1)
        RHS2 = IEOR(ToLong(Length), WyP4)
        HashCode = WyMum(LHS2, RHS2)
    ELSEIF (Length <= 8) THEN
        LHS1 = IEOR(IEOR(Pack_U32(Input, Offset), Seed1), WyP0)
        RHS1 = IEOR(IEOR(Pack_U32(Input, Offset+Length-4), Seed1), WyP1)
        LHS2 = IEOR(WyMum(LHS1, RHS1), Seed1)
        RHS2 = IEOR(ToLong(Length), WyP4)
        HashCode = WyMum(LHS2, RHS2)
    ELSEIF (Length <= 16) THEN
        LHS1 = IEOR(IEOR(U64Rotate32(Input, Offset), Seed1), WyP0)
        RHS1 = IEOR(IEOR(U64Rotate32(Input, Offset+Length-8), Seed1), WyP1)
        LHS2 = IEOR(WyMum(LHS1, RHS1), Seed1)
        RHS2 = IEOR(ToLong(Length), WyP4)
        HashCode = WyMum(LHS2, RHS2)
    ELSEIF (Length <= 24) THEN
        LHS1 = IEOR(IEOR(U64Rotate32(Input, Offset), Seed1), WyP0)
        RHS1 = IEOR(IEOR(U64Rotate32(Input, Offset+8), Seed1), WyP1)
        LHS2 = IEOR(IEOR(U64Rotate32(Input, Offset+Length-8), Seed1), WyP2)
        RHS2 = IEOR(Seed1, WyP3)
        HashCode = WyMum(IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2)), \
                            IEOR(ToLong(Length), WyP4))
    ELSEIF (Length <= 32) THEN
        LHS1 = IEOR(IEOR(U64Rotate32(Input, Offset), Seed1), WyP0)
        RHS1 = IEOR(IEOR(U64Rotate32(Input, Offset+8), Seed1), WyP1)
        LHS2 = IEOR(IEOR(U64Rotate32(Input, Offset+16), Seed1), WyP2)
        RHS2 = IEOR(IEOR(U64Rotate32(Input, Offset+Length-8), Seed1), WyP3)
        HashCode = WyMum(IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2)), \
                            IEOR(ToLong(Length), WyP4))
    ELSE
        Seed2 = Seed1
        I = Length
        P = Offset
        DO WHILE(I > 256)
            LHS1 = IEOR(IEOR(PackLong(Input, P), Seed1), WyP0)
            RHS1 = IEOR(IEOR(PackLong(Input, P+8), Seed1), WyP1)
            LHS2 = IEOR(IEOR(PackLong(Input, P+16), Seed1), WyP2)
            RHS2 = IEOR(IEOR(PackLong(Input, P+24), Seed1), WyP3)
            Seed1 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(PackLong(Input, P+32), Seed2), WyP1)
            RHS1 = IEOR(IEOR(PackLong(Input, P+40), Seed2), WyP2)
            LHS2 = IEOR(IEOR(PackLong(Input, P+48), Seed2), WyP3)
            RHS2 = IEOR(IEOR(PackLong(Input, P+56), Seed2), WyP0)
            Seed2 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(PackLong(Input, P+64), Seed1), WyP0)
            RHS1 = IEOR(IEOR(PackLong(Input, P+72), Seed1), WyP1)
            LHS2 = IEOR(IEOR(PackLong(Input, P+80), Seed1), WyP2)
            RHS2 = IEOR(IEOR(PackLong(Input, P+88), Seed1), WyP3)
            Seed1 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(PackLong(Input, P+96), Seed2), WyP1)
            RHS1 = IEOR(IEOR(PackLong(Input, P+104), Seed2), WyP2)
            LHS2 = IEOR(IEOR(PackLong(Input, P+112), Seed2), WyP3)
            RHS2 = IEOR(IEOR(PackLong(Input, P+120), Seed2), WyP0)
            Seed2 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(PackLong(Input, P+128), Seed1), WyP0)
            RHS1 = IEOR(IEOR(PackLong(Input, P+136), Seed1), WyP1)
            LHS2 = IEOR(IEOR(PackLong(Input, P+144), Seed1), WyP2)
            RHS2 = IEOR(IEOR(PackLong(Input, P+152), Seed1), WyP3)
            Seed1 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(PackLong(Input, P+160), Seed2), WyP1)
            RHS1 = IEOR(IEOR(PackLong(Input, P+168), Seed2), WyP2)
            LHS2 = IEOR(IEOR(PackLong(Input, P+176), Seed2), WyP3)
            RHS2 = IEOR(IEOR(PackLong(Input, P+184), Seed2), WyP0)
            Seed2 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(PackLong(Input, P+192), Seed1), WyP0)
            RHS1 = IEOR(IEOR(PackLong(Input, P+200), Seed1), WyP1)
            LHS2 = IEOR(IEOR(PackLong(Input, P+208), Seed1), WyP2)
            RHS2 = IEOR(IEOR(PackLong(Input, P+216), Seed1), WyP3)
            Seed1 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(PackLong(Input, P+224), Seed2), WyP1)
            RHS1 = IEOR(IEOR(PackLong(Input, P+232), Seed2), WyP2)
            LHS2 = IEOR(IEOR(PackLong(Input, P+240), Seed2), WyP3)
            RHS2 = IEOR(IEOR(PackLong(Input, P+248), Seed2), WyP0)
            Seed2 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            I = I - 256
            P = P + 256
        END DO
        DO WHILE(I  > 32)
            LHS1 = IEOR(IEOR(PackLong(Input, P), Seed1), WyP0)
            RHS1 = IEOR(IEOR(PackLong(Input, P+8), Seed1), WyP1)
            Seed1 = WyMum(LHS1, RHS1)
            LHS2 = IEOR(IEOR(PackLong(Input, P+16), Seed2), WyP2)
            RHS2 = IEOR(IEOR(PackLong(Input, P+24), Seed2), WyP3)
            Seed2 = WyMum(LHS2, RHS2)
            I = I - 32
            P = P + 32
        END DO
        IF (I < 4) THEN
            Seed1 = WyMum(IEOR(IEOR(WyR3(Input, P, I), Seed1), WyP0), IEOR(Seed1, WyP1))
        ELSEIF (I <= 8)  THEN
            Seed1 = WyMum(IEOR(IEOR(Pack_U32(Input, P), Seed1), WyP0), \
                            IEOR(IEOR(Pack_U32(Input, P+I-4), Seed1), WyP1))
        ELSEIF (I <= 16)  THEN
            Seed1 = WyMum(IEOR(IEOR(U64Rotate32(Input, P), Seed1), WyP0), \
                            IEOR(IEOR(U64Rotate32(Input, P+I-8), Seed1), WyP1))
        ELSEIF (I <= 24)  THEN
            Seed1 = WyMum(IEOR(IEOR(U64Rotate32(Input, P), Seed1), WyP0), \
                            IEOR(IEOR(U64Rotate32(Input, P+8), Seed1), WyP1))
            Seed2 = WyMum(IEOR(IEOR(U64Rotate32(Input, P+I-8), Seed2), WyP2), \
                            IEOR(Seed2, WyP3))
        ELSE
            Seed1 = WyMum(IEOR(IEOR(U64Rotate32(Input, P), Seed1), WyP0), \
                            IEOR(IEOR(U64Rotate32(Input, P+8), Seed1), WyP1))
            Seed2 = WyMum(IEOR(IEOR(U64Rotate32(Input, P+16), Seed2), WyP2), \
                            IEOR(IEOR(U64Rotate32(Input, P+I-8), Seed2), WyP3))
        END IF
        HashCode = WyMum(IEOR(Seed1, Seed2), IEOR(ToLong(Length), WyP4))
    END IF

    RETURN

#undef WyMum
#undef WyR3
#undef U64Rotate32
#undef Pack_U32

END FUNCTION WyV3_Hash64

!******************************************************************************

FUNCTION WyF3_Hash64(Input, SeedForHash) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the final version 3 of the WyHash hash algorithm by Wang Yi.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, INTENT(IN)   :: Input(0:)    !! input bytes
    tLong, INTENT(IN)   :: SeedForHash  !! seed
    tLong               :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Seed, Seed1, Seed2, A, B
    tLong       :: LHS1, RHS1, LHS2, RHS2
    tIndex      :: Length, Remaining, Offset, ShiftLen

!** SUBROUTINE MACRO DEFINITIONS:
#define Pack_U32(Buf,Index)     MaskI32(PackInteger(Buf, Index))
#define WyMix(A, B)             UnsignedLongMultiplyorFold(A, B)
#define WyR3(Buf,Index,K)                                     \
    IOR(IOR(SHIFTL(MaskI8(Buf(Index)), 16),                 \
            SHIFTL(MaskI8(Buf(Index + SHIFTR(K, 1))), 8)),  \
                    MaskI8(Buf(Index + K - 1)))
#define WyR4(Buf,Index)         Pack_U32(Buf, Index)

!** FLOW

    ! initialize
    Length = SIZE(Input)
    Seed   = IEOR(SeedForHash, WyP0)

    ! perform hashing
    IF (Length <= 16) THEN
        IF (Length >= 4) THEN
            ShiftLen = SHIFTL(SHIFTR(Length, 3), 2)
            Offset  = Length - 4
            A = IOR(SHIFTL(WyR4(Input, 0), 32),  WyR4(Input, ShiftLen))
            B = IOR(SHIFTL(WyR4(Input, Offset), 32), WyR4(Input, Offset - ShiftLen))
        ELSEIF (Length > 0) THEN
            A = WyR3(Input, 0, Length)
            B = 0_kLong
        ELSE
            A = 0_kLong
            B = 0_kLong
        END IF
    ELSE
        Remaining = Length
        Offset = 0
        IF (Remaining > 48) THEN
            Seed1 = Seed
            Seed2 = Seed
            DO
                LHS1 = IEOR(PackLong(Input, Offset), WyP1)
                RHS1 = IEOR(PackLong(Input, Offset + 8), Seed)
                Seed = WyMix(LHS1, RHS1)
                LHS1 = IEOR(PackLong(Input, Offset + 16), WyP2)
                RHS1 = IEOR(PackLong(Input, Offset + 24), Seed1)
                Seed1 = WyMix(LHS1, RHS1)
                LHS1 = IEOR(PackLong(Input, Offset + 32), WyP3)
                RHS1 = IEOR(PackLong(Input, Offset + 40), Seed2)
                Seed2 = WyMix(LHS1, RHS1)
                Offset = Offset + 48
                Remaining = Remaining - 48
                IF (Remaining <= 48) EXIT
            END DO
            Seed = IEOR(Seed, IEOR(Seed1, Seed2))
        END IF
        DO WHILE (Remaining > 16)
            LHS1 = IEOR(PackLong(Input, Offset), WyP1)
            RHS1 = IEOR(PackLong(Input, Offset + 8), Seed)
            Seed = WyMix(LHS1, RHS1)
            Remaining = Remaining - 16
            Offset = Offset + 16
        END DO
        A = PackLong(Input, Offset + Remaining - 16)
        B = PackLong(Input, Offset + Remaining - 8)
    END IF
    LHS1 = IEOR(A, WyP1)
    RHS1 = IEOR(B, Seed)
    LHS2 = IEOR(WyP1, ToLong(Length))
    RHS2 = WyMix(LHS1, RHS1)
    HashCode = WyMix(LHS2, RHS2)

    RETURN

#undef WyMix
#undef WyR3
#undef WyR4
#undef Pack_U32

END FUNCTION WyF3_Hash64

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

!**************************************************************************

PURE FUNCTION PackInteger(Buf, Off) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To pack the array 'Buf' at Offset 'Off' into the 32-bit word 'Res',
    ! in little-endian convention (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,  INTENT(IN)  :: Buf(0:)  ! buffer
    tIndex, INTENT(IN)  :: Off      ! Offset
    tInteger            :: Res      ! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

#define MaskInteger(X)  IAND(ToInteger(X), Z'000000FF')

    ! implementation algorithm #1
    Res = IOR(IOR(IOR(       MaskInteger(Buf(Off)),         &
                      SHIFTL(MaskInteger(Buf(Off+1)),  8)), &
                      SHIFTL(MaskInteger(Buf(Off+2)), 16)), &
                      SHIFTL(MaskInteger(Buf(Off+3)), 24))

#undef MaskInteger

    RETURN

END FUNCTION PackInteger

!******************************************************************************

END SUBMODULE SubBase_WyHash_Opt

!******************************************************************************

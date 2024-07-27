
SUBMODULE (ModBase_ExperimentalHash64) SubBase_WyHash_Exp

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an experimental implementation for the WyHash hash algorithm
!   by Wang Yi. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    USE ModBase_UIntUtil,   ONLY: UMul128_Upper64
    USE ModBase_ExperimentalHash32
    
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
    tLong, PARAMETER    :: Secret0 = ToLong(Z'A0761D6478BD642F')
    tLong, PARAMETER    :: Secret1 = ToLong(Z'E7037ED1A0B428DB')
    tLong, PARAMETER    :: Secret2 = ToLong(Z'8EBC6AF09C88C6E3')
    tLong, PARAMETER    :: Secret3 = ToLong(Z'589965CC75374CC3')

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE FUNCTION Wy_Hash64_Exp(Input, InpSize, Algo, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code using version 3 (?) of the WyHash hash algorithm by Wang Yi.
    !  This is an interface routine that calls the working routine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), DIMENSION(..), INTENT(IN)  :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
    tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-11)
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
        HashCode = WyV3_Hash64(InpPtr, Seed, Pack_I64_A1, Pack_I32_A1)
    CASE (2)
        HashCode = WyV3_Hash64(InpPtr, Seed, Pack_I64_A2, Pack_I32_A2)
    CASE (3)
        HashCode = WyV3_Hash64(InpPtr, Seed, Pack_I64_A3, Pack_I32_A3)
    CASE (4)
        HashCode = WyV3_Hash64(InpPtr, Seed, Pack_I64_A4, Pack_I32_A4)
    CASE (5)
        HashCode = WyV3_Hash64(InpPtr, Seed, Pack_I64_A5, Pack_I32_A5)
    CASE (6)
        HashCode = WyV3_Hash64(InpPtr, Seed, Pack_I64_A6, Pack_I32_A6)
    CASE (7)
        HashCode = WyV3_Hash64(InpPtr, Seed, Pack_I64_A7, Pack_I32_A7)
    CASE (8)
        HashCode = WyV3_Hash64_08(InpPtr, Seed, Pack_I32_A6)
    CASE (9)
        HashCode = WyV3_Hash64_09(InpPtr, Seed, Pack_I32_A7)
    CASE (10)
        HashCode = WyV3_Hash64_10(InpPtr, Seed, Pack_I32_A6)
    CASE (11)
        HashCode = WyV3_Hash64_11(InpPtr, Seed)
    END SELECT

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION Wy_Hash64_Exp

!******************************************************************************

MODULE FUNCTION WyF3_Hash64_Exp(Input, InpSize, Algo, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code using the final version 3 of the WyHash hash algorithm by Wang Yi.
    !  This is an interface routine that calls the working routine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), DIMENSION(..), INTENT(IN)  :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
    tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-10)
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
        HashCode = WyF3_Hash64(InpPtr, Seed, Pack_I64_A1, Pack_I32_A1)
    CASE (2)
        HashCode = WyF3_Hash64(InpPtr, Seed, Pack_I64_A2, Pack_I32_A2)
    CASE (3)
        HashCode = WyF3_Hash64(InpPtr, Seed, Pack_I64_A3, Pack_I32_A3)
    CASE (4)
        HashCode = WyF3_Hash64(InpPtr, Seed, Pack_I64_A4, Pack_I32_A4)
    CASE (5)
        HashCode = WyF3_Hash64(InpPtr, Seed, Pack_I64_A5, Pack_I32_A5)
    CASE (6)
        HashCode = WyF3_Hash64(InpPtr, Seed, Pack_I64_A6, Pack_I32_A6)
    CASE (7)
        HashCode = WyF3_Hash64(InpPtr, Seed, Pack_I64_A7, Pack_I32_A7)
    CASE (8)
        HashCode = WyF3_Hash64_08(InpPtr, Seed)
    CASE (9)
        HashCode = WyF3_Hash64_09(InpPtr, Seed)
    CASE (10)
        HashCode = WyF3_Hash64_10(InpPtr, Seed)
    END SELECT

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION WyF3_Hash64_Exp

!******************************************************************************

FUNCTION WyV3_Hash64(Input, Seed, PackLong, PackInteger) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using version 3 (?) of the WyHash hash algorithm by Wang Yi.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, INTENT(IN)   :: Input(0:)    !! input bytes
    tLong, INTENT(IN)   :: Seed         !! seed
    PROCEDURE(Pack_I64) :: PackLong     !! procedure to convert a byte array to 64-bit integer
    PROCEDURE(Pack_I32) :: PackInteger  !! procedure to convert a byte array to 64-bit integer
    tLong               :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Seed1, Seed2, LHS1, RHS1, LHS2, RHS2
    tIndex      :: I, Length, P, Offset

!** SUBROUTINE MACRO DEFINITIONS:
#define GetU8(In, Off)      MaskI8(In(Off))
#define Pack_U32(In, Off)   MaskI32(PackInteger(In, Off))
#define WyMum(LHS, RHS)     UnsignedLongMultiplyorFold(LHS, RHS)
#define WyR3(In, Index, K) \
    IOR(IOR(SHIFTL(ToLong(GetU8(In, Index)), 16),                   \
            SHIFTL(ToLong(GetU8(In, Index + SHIFTR(K, 1))), 8)),    \
                   ToLong(GetU8(In, Index + K - 1)))
#define U64Rotate32(In, Index)  \
    IOR(SHIFTL(Pack_U32(In, Index), 32), Pack_U32(In, Index + 4))

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

#undef GetU8
#undef Pack_U32
#undef WyMum
#undef WyR3
#undef U64Rotate32

END FUNCTION WyV3_Hash64

!******************************************************************************

FUNCTION WyV3_Hash64_08(Input, Seed, PackInteger) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the WyHash hash algorithm by Wang Yi.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, TARGET, INTENT(IN)   :: Input(0:)    !! input bytes
    tLong,         INTENT(IN)   :: Seed         !! seed
    PROCEDURE(Pack_I32)         :: PackInteger  !! procedure to convert a byte array to 64-bit integer
    tLong                       :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong, POINTER  :: I64Val(:) => NULL()
    tLong           :: Seed1, Seed2, LHS1, RHS1, LHS2, RHS2
    tIndex          :: I, Length, P, Offset, Index
    TYPE(C_PTR)     :: CPTR

!** FLOW

! define macros for better performance than using internal procedures
#define GetU8(In, Off)      MaskI8(In(Off))
#define Pack_U32(In, Off)   MaskI32(PackInteger(In, Off))
#define WyMum(LHS, RHS)     UnsignedLongMultiplyorFold(LHS, RHS)
#define WyR3(In, Index, K) \
    IOR(IOR(SHIFTL(ToLong(GetU8(In, Index)), 16),                   \
            SHIFTL(ToLong(GetU8(In, Index + SHIFTR(K, 1))), 8)),    \
                   ToLong(GetU8(In, Index + K - 1)))
#define U64Rotate32(In, Index)  \
    IOR(SHIFTL(Pack_U32(In, Index), 32), Pack_U32(In, Index + 4))

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
        ! initialize
        Seed2 = Seed1
        I = Length
        P = Offset
        Index = 1
        CPTR = C_LOC(Input(0))
        CALL C_F_POINTER(CPTR, I64Val, SHAPE=[Length/8])
        DO WHILE(I > 256)
            LHS1 = IEOR(IEOR(I64Val(Index), Seed1), WyP0)
            RHS1 = IEOR(IEOR(I64Val(Index+1), Seed1), WyP1)
            LHS2 = IEOR(IEOR(I64Val(Index+2), Seed1), WyP2)
            RHS2 = IEOR(IEOR(I64Val(Index+3), Seed1), WyP3)
            Seed1 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(I64Val(Index+4), Seed2), WyP1)
            RHS1 = IEOR(IEOR(I64Val(Index+5), Seed2), WyP2)
            LHS2 = IEOR(IEOR(I64Val(Index+6), Seed2), WyP3)
            RHS2 = IEOR(IEOR(I64Val(Index+7), Seed2), WyP0)
            Seed2 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(I64Val(Index+8), Seed1), WyP0)
            RHS1 = IEOR(IEOR(I64Val(Index+9), Seed1), WyP1)
            LHS2 = IEOR(IEOR(I64Val(Index+10), Seed1), WyP2)
            RHS2 = IEOR(IEOR(I64Val(Index+11), Seed1), WyP3)
            Seed1 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(I64Val(Index+12), Seed2), WyP1)
            RHS1 = IEOR(IEOR(I64Val(Index+13), Seed2), WyP2)
            LHS2 = IEOR(IEOR(I64Val(Index+14), Seed2), WyP3)
            RHS2 = IEOR(IEOR(I64Val(Index+15), Seed2), WyP0)
            Seed2 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(I64Val(Index+16), Seed1), WyP0)
            RHS1 = IEOR(IEOR(I64Val(Index+17), Seed1), WyP1)
            LHS2 = IEOR(IEOR(I64Val(Index+18), Seed1), WyP2)
            RHS2 = IEOR(IEOR(I64Val(Index+19), Seed1), WyP3)
            Seed1 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(I64Val(Index+20), Seed2), WyP1)
            RHS1 = IEOR(IEOR(I64Val(Index+21), Seed2), WyP2)
            LHS2 = IEOR(IEOR(I64Val(Index+22), Seed2), WyP3)
            RHS2 = IEOR(IEOR(I64Val(Index+23), Seed2), WyP0)
            Seed2 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(I64Val(Index+24), Seed1), WyP0)
            RHS1 = IEOR(IEOR(I64Val(Index+25), Seed1), WyP1)
            LHS2 = IEOR(IEOR(I64Val(Index+26), Seed1), WyP2)
            RHS2 = IEOR(IEOR(I64Val(Index+27), Seed1), WyP3)
            Seed1 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(I64Val(Index+28), Seed2), WyP1)
            RHS1 = IEOR(IEOR(I64Val(Index+29), Seed2), WyP2)
            LHS2 = IEOR(IEOR(I64Val(Index+30), Seed2), WyP3)
            RHS2 = IEOR(IEOR(I64Val(Index+31), Seed2), WyP0)
            Seed2 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            I = I - 256
            P = P + 256
            Index = Index + 32
        END DO
        DO WHILE(I  > 32)
            LHS1 = IEOR(IEOR(I64Val(Index), Seed1), WyP0)
            RHS1 = IEOR(IEOR(I64Val(Index+1), Seed1), WyP1)
            Seed1 = WyMum(LHS1, RHS1)
            LHS2 = IEOR(IEOR(I64Val(Index+2), Seed2), WyP2)
            RHS2 = IEOR(IEOR(I64Val(Index+3), Seed2), WyP3)
            Seed2 = WyMum(LHS2, RHS2)
            I = I - 32
            P = P + 32
            Index = Index + 4
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

    ! free pointers
    NULLIFY(I64Val)

    RETURN

#undef GetU8
#undef Pack_U32
#undef WyMum
#undef WyR3
#undef U64Rotate32

END FUNCTION WyV3_Hash64_08

!******************************************************************************

FUNCTION WyV3_Hash64_09(Input, Seed, PackInteger) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the WyHash hash algorithm by Wang Yi.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, INTENT(IN)   :: Input(0:)    ! input bytes
    tLong, INTENT(IN)   :: Seed         ! seed
    PROCEDURE(Pack_I32) :: PackInteger  !! procedure to convert a byte array to 64-bit integer
    tLong               :: HashCode     ! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Seed1, Seed2, LHS1, RHS1, LHS2, RHS2
    tIndex      :: I, Length, P, Offset
    tByte       :: Bytes(0:255)
    tLong       :: Longs(0:31)
    tInteger    :: Integers(0:7)
    EQUIVALENCE(Bytes, Longs)
    EQUIVALENCE(Bytes, Integers)

!** FLOW

! define macros for better performance than using internal procedures
#define GetU8(In, Off)          MaskI8(In(Off))
#define Pack_U32(In, Off)       MaskI32(PackInteger(In, Off))
#define WyMum(LHS, RHS)         UnsignedLongMultiplyorFold(LHS, RHS)
#define WyR3(In, Index, K) \
    IOR(IOR(SHIFTL(ToLong(GetU8(In, Index)), 16),                   \
            SHIFTL(ToLong(GetU8(In, Index + SHIFTR(K, 1))), 8)),    \
                   ToLong(GetU8(In, Index + K - 1)))
#define U64Rotate32(Val,Index)  IOR(SHIFTL(MaskI32(Val(Index)), 32), MaskI32(Val(Index+1)))

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
        Bytes(0:7)  = Input(Offset:Offset+7)
        Bytes(8:15) = Input(Offset+Length-8:Offset+Length-1)
        LHS1 = IEOR(IEOR(U64Rotate32(Integers, 0), Seed1), WyP0)
        RHS1 = IEOR(IEOR(U64Rotate32(Integers, 2), Seed1), WyP1)
        LHS2 = IEOR(WyMum(LHS1, RHS1), Seed1)
        RHS2 = IEOR(ToLong(Length), WyP4)
        HashCode = WyMum(LHS2, RHS2)
    ELSEIF (Length <= 24) THEN
        Bytes(0:15)  = Input(Offset:Offset+15)
        Bytes(16:23) = Input(Offset+Length-8:Offset+Length-1)
        LHS1 = IEOR(IEOR(U64Rotate32(Integers, 0), Seed1), WyP0)
        RHS1 = IEOR(IEOR(U64Rotate32(Integers, 2), Seed1), WyP1)
        LHS2 = IEOR(IEOR(U64Rotate32(Integers, 4), Seed1), WyP2)
        RHS2 = IEOR(Seed1, WyP3)
        HashCode = WyMum(IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2)), \
                            IEOR(ToLong(Length), WyP4))
    ELSEIF (Length <= 32) THEN
        Bytes( 0:23) = Input(Offset:Offset+23)
        Bytes(24:31) = Input(Offset+Length-8:Offset+Length-1)
        LHS1 = IEOR(IEOR(U64Rotate32(Integers, 0), Seed1), WyP0)
        RHS1 = IEOR(IEOR(U64Rotate32(Integers, 2), Seed1), WyP1)
        LHS2 = IEOR(IEOR(U64Rotate32(Integers, 4), Seed1), WyP2)
        RHS2 = IEOR(IEOR(U64Rotate32(Integers, 6), Seed1), WyP3)
        HashCode = WyMum(IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2)), \
                            IEOR(ToLong(Length), WyP4))
    ELSE
        ! initialize
        Seed2 = Seed1
        I = Length
        P = Offset
        DO WHILE(I > 256)
            Bytes(0:255) = Input(P:P+255)
            LHS1 = IEOR(IEOR(Longs(0), Seed1), WyP0)
            RHS1 = IEOR(IEOR(Longs(1), Seed1), WyP1)
            LHS2 = IEOR(IEOR(Longs(2), Seed1), WyP2)
            RHS2 = IEOR(IEOR(Longs(3), Seed1), WyP3)
            Seed1 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(Longs(4), Seed2), WyP1)
            RHS1 = IEOR(IEOR(Longs(5), Seed2), WyP2)
            LHS2 = IEOR(IEOR(Longs(6), Seed2), WyP3)
            RHS2 = IEOR(IEOR(Longs(7), Seed2), WyP0)
            Seed2 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(Longs(8), Seed1), WyP0)
            RHS1 = IEOR(IEOR(Longs(9), Seed1), WyP1)
            LHS2 = IEOR(IEOR(Longs(10), Seed1), WyP2)
            RHS2 = IEOR(IEOR(Longs(11), Seed1), WyP3)
            Seed1 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(Longs(12), Seed2), WyP1)
            RHS1 = IEOR(IEOR(Longs(13), Seed2), WyP2)
            LHS2 = IEOR(IEOR(Longs(14), Seed2), WyP3)
            RHS2 = IEOR(IEOR(Longs(15), Seed2), WyP0)
            Seed2 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(Longs(16), Seed1), WyP0)
            RHS1 = IEOR(IEOR(Longs(17), Seed1), WyP1)
            LHS2 = IEOR(IEOR(Longs(18), Seed1), WyP2)
            RHS2 = IEOR(IEOR(Longs(19), Seed1), WyP3)
            Seed1 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(Longs(20), Seed2), WyP1)
            RHS1 = IEOR(IEOR(Longs(21), Seed2), WyP2)
            LHS2 = IEOR(IEOR(Longs(22), Seed2), WyP3)
            RHS2 = IEOR(IEOR(Longs(23), Seed2), WyP0)
            Seed2 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(Longs(24), Seed1), WyP0)
            RHS1 = IEOR(IEOR(Longs(25), Seed1), WyP1)
            LHS2 = IEOR(IEOR(Longs(26), Seed1), WyP2)
            RHS2 = IEOR(IEOR(Longs(27), Seed1), WyP3)
            Seed1 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(Longs(28), Seed2), WyP1)
            RHS1 = IEOR(IEOR(Longs(29), Seed2), WyP2)
            LHS2 = IEOR(IEOR(Longs(30), Seed2), WyP3)
            RHS2 = IEOR(IEOR(Longs(31), Seed2), WyP0)
            Seed2 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            I = I - 256
            P = P + 256
        END DO
        DO WHILE(I  > 32)
            Bytes(0:31) = Input(P:P+31)
            LHS1 = IEOR(IEOR(Longs(0), Seed1), WyP0)
            RHS1 = IEOR(IEOR(Longs(1), Seed1), WyP1)
            Seed1 = WyMum(LHS1, RHS1)
            LHS2 = IEOR(IEOR(Longs(2), Seed2), WyP2)
            RHS2 = IEOR(IEOR(Longs(3), Seed2), WyP3)
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
            Bytes(0:7)  = Input(P:P+7)
            Bytes(8:15) = Input(P+I-8:P+I-1)
            Seed1 = WyMum(IEOR(IEOR(U64Rotate32(Integers, 0), Seed1), WyP0), \
                            IEOR(IEOR(U64Rotate32(Integers, 2), Seed1), WyP1))
        ELSEIF (I <= 24)  THEN
            Bytes(0:15)  = Input(P:P+15)
            Bytes(16:23) = Input(P+I-8:P+I-1)
            Seed1 = WyMum(IEOR(IEOR(U64Rotate32(Integers, 0), Seed1), WyP0), \
                            IEOR(IEOR(U64Rotate32(Integers, 2), Seed1), WyP1))
            Seed2 = WyMum(IEOR(IEOR(U64Rotate32(Integers, 4), Seed2), WyP2), \
                            IEOR(Seed2, WyP3))
        ELSE
            Bytes(0:23) = Input(P:P+23)
            Bytes(24:31) = Input(P+I-8:P+I-1)
            Seed1 = WyMum(IEOR(IEOR(U64Rotate32(Integers, 0), Seed1), WyP0), \
                            IEOR(IEOR(U64Rotate32(Integers, 2), Seed1), WyP1))
            Seed2 = WyMum(IEOR(IEOR(U64Rotate32(Integers, 4), Seed2), WyP2), \
                            IEOR(IEOR(U64Rotate32(Integers, 6), Seed2), WyP3))
        END IF
        HashCode = WyMum(IEOR(Seed1, Seed2), IEOR(ToLong(Length), WyP4))
    END IF

    RETURN

#undef GetU8
#undef Pack_U32
#undef WyMum
#undef WyR3
#undef U64Rotate32

END FUNCTION WyV3_Hash64_09

!******************************************************************************

FUNCTION WyV3_Hash64_10(Input, Seed, PackInteger) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the WyHash hash algorithm by Wang Yi.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, TARGET, INTENT(IN)   :: Input(0:)    ! input bytes
    tLong,         INTENT(IN)   :: Seed         ! seed
    PROCEDURE(Pack_I32)         :: PackInteger  !! procedure to convert a byte array to 64-bit integer
    tLong                       :: HashCode     ! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong, POINTER  :: I64Val(:) => NULL()
    tLong           :: Seed1, Seed2, LHS1, RHS1, LHS2, RHS2
    tIndex          :: I, Length, P, Offset, Index
    tByte           :: Bytes(0:31)
    tInteger        :: I32Val(0:7)
    EQUIVALENCE(Bytes, I32Val)
    TYPE(C_PTR)     :: CPTR

!** FLOW

! define macros for better performance than using internal procedures
#define GetU8(In, Off)          MaskI8(In(Off))
#define Pack_U32(In, Off)       MaskI32(PackInteger(In, Off))
#define WyMum(LHS, RHS)         UnsignedLongMultiplyorFold(LHS, RHS)
#define WyR3(In, Index, K) \
    IOR(IOR(SHIFTL(ToLong(GetU8(In, Index)), 16),                   \
            SHIFTL(ToLong(GetU8(In, Index + SHIFTR(K, 1))), 8)),    \
                   ToLong(GetU8(In, Index + K - 1)))
#define U64Rotate32(Val,Index)  IOR(SHIFTL(MaskI32(Val(Index)), 32), MaskI32(Val(Index+1)))

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
        Bytes(0:7)  = Input(Offset:Offset+7)
        Bytes(8:15) = Input(Offset+Length-8:Offset+Length-1)
        LHS1 = IEOR(IEOR(U64Rotate32(I32Val, 0), Seed1), WyP0)
        RHS1 = IEOR(IEOR(U64Rotate32(I32Val, 2), Seed1), WyP1)
        LHS2 = IEOR(WyMum(LHS1, RHS1), Seed1)
        RHS2 = IEOR(ToLong(Length), WyP4)
        HashCode = WyMum(LHS2, RHS2)
    ELSEIF (Length <= 24) THEN
        Bytes(0:15)  = Input(Offset:Offset+15)
        Bytes(16:23) = Input(Offset+Length-8:Offset+Length-1)
        LHS1 = IEOR(IEOR(U64Rotate32(I32Val, 0), Seed1), WyP0)
        RHS1 = IEOR(IEOR(U64Rotate32(I32Val, 2), Seed1), WyP1)
        LHS2 = IEOR(IEOR(U64Rotate32(I32Val, 4), Seed1), WyP2)
        RHS2 = IEOR(Seed1, WyP3)
        HashCode = WyMum(IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2)), \
                            IEOR(ToLong(Length), WyP4))
    ELSEIF (Length <= 32) THEN
        Bytes( 0:23) = Input(Offset:Offset+23)
        Bytes(24:31) = Input(Offset+Length-8:Offset+Length-1)
        LHS1 = IEOR(IEOR(U64Rotate32(I32Val, 0), Seed1), WyP0)
        RHS1 = IEOR(IEOR(U64Rotate32(I32Val, 2), Seed1), WyP1)
        LHS2 = IEOR(IEOR(U64Rotate32(I32Val, 4), Seed1), WyP2)
        RHS2 = IEOR(IEOR(U64Rotate32(I32Val, 6), Seed1), WyP3)
        HashCode = WyMum(IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2)), \
                            IEOR(ToLong(Length), WyP4))
    ELSE
        ! initialize
        Seed2 = Seed1
        I = Length
        P = Offset
        Index = 1
        CPTR = C_LOC(Input(0))
        CALL C_F_POINTER(CPTR, I64Val, SHAPE=[Length/8])
        DO WHILE(I > 256)
            LHS1 = IEOR(IEOR(I64Val(Index), Seed1), WyP0)
            RHS1 = IEOR(IEOR(I64Val(Index+1), Seed1), WyP1)
            LHS2 = IEOR(IEOR(I64Val(Index+2), Seed1), WyP2)
            RHS2 = IEOR(IEOR(I64Val(Index+3), Seed1), WyP3)
            Seed1 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(I64Val(Index+4), Seed2), WyP1)
            RHS1 = IEOR(IEOR(I64Val(Index+5), Seed2), WyP2)
            LHS2 = IEOR(IEOR(I64Val(Index+6), Seed2), WyP3)
            RHS2 = IEOR(IEOR(I64Val(Index+7), Seed2), WyP0)
            Seed2 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(I64Val(Index+8), Seed1), WyP0)
            RHS1 = IEOR(IEOR(I64Val(Index+9), Seed1), WyP1)
            LHS2 = IEOR(IEOR(I64Val(Index+10), Seed1), WyP2)
            RHS2 = IEOR(IEOR(I64Val(Index+11), Seed1), WyP3)
            Seed1 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(I64Val(Index+12), Seed2), WyP1)
            RHS1 = IEOR(IEOR(I64Val(Index+13), Seed2), WyP2)
            LHS2 = IEOR(IEOR(I64Val(Index+14), Seed2), WyP3)
            RHS2 = IEOR(IEOR(I64Val(Index+15), Seed2), WyP0)
            Seed2 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(I64Val(Index+16), Seed1), WyP0)
            RHS1 = IEOR(IEOR(I64Val(Index+17), Seed1), WyP1)
            LHS2 = IEOR(IEOR(I64Val(Index+18), Seed1), WyP2)
            RHS2 = IEOR(IEOR(I64Val(Index+19), Seed1), WyP3)
            Seed1 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(I64Val(Index+20), Seed2), WyP1)
            RHS1 = IEOR(IEOR(I64Val(Index+21), Seed2), WyP2)
            LHS2 = IEOR(IEOR(I64Val(Index+22), Seed2), WyP3)
            RHS2 = IEOR(IEOR(I64Val(Index+23), Seed2), WyP0)
            Seed2 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(I64Val(Index+24), Seed1), WyP0)
            RHS1 = IEOR(IEOR(I64Val(Index+25), Seed1), WyP1)
            LHS2 = IEOR(IEOR(I64Val(Index+26), Seed1), WyP2)
            RHS2 = IEOR(IEOR(I64Val(Index+27), Seed1), WyP3)
            Seed1 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(I64Val(Index+28), Seed2), WyP1)
            RHS1 = IEOR(IEOR(I64Val(Index+29), Seed2), WyP2)
            LHS2 = IEOR(IEOR(I64Val(Index+30), Seed2), WyP3)
            RHS2 = IEOR(IEOR(I64Val(Index+31), Seed2), WyP0)
            Seed2 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            I = I - 256
            P = P + 256
            Index = Index + 32
        END DO
        DO WHILE(I  > 32)
            LHS1 = IEOR(IEOR(I64Val(Index), Seed1), WyP0)
            RHS1 = IEOR(IEOR(I64Val(Index+1), Seed1), WyP1)
            Seed1 = WyMum(LHS1, RHS1)
            LHS2 = IEOR(IEOR(I64Val(Index+2), Seed2), WyP2)
            RHS2 = IEOR(IEOR(I64Val(Index+3), Seed2), WyP3)
            Seed2 = WyMum(LHS2, RHS2)
            I = I - 32
            P = P + 32
            Index = Index + 4
        END DO
        IF (I < 4) THEN
            Seed1 = WyMum(IEOR(IEOR(WyR3(Input, P, I), Seed1), WyP0), IEOR(Seed1, WyP1))
        ELSEIF (I <= 8)  THEN
            Seed1 = WyMum(IEOR(IEOR(Pack_U32(Input, P), Seed1), WyP0), \
                            IEOR(IEOR(Pack_U32(Input, P+I-4), Seed1), WyP1))
        ELSEIF (I <= 16)  THEN
            Bytes(0:7)  = Input(P:P+7)
            Bytes(8:15) = Input(P+I-8:P+I-1)
            Seed1 = WyMum(IEOR(IEOR(U64Rotate32(I32Val, 0), Seed1), WyP0), \
                            IEOR(IEOR(U64Rotate32(I32Val, 2), Seed1), WyP1))
        ELSEIF (I <= 24)  THEN
            Bytes(0:15)  = Input(P:P+15)
            Bytes(16:23) = Input(P+I-8:P+I-1)
            Seed1 = WyMum(IEOR(IEOR(U64Rotate32(I32Val, 0), Seed1), WyP0), \
                            IEOR(IEOR(U64Rotate32(I32Val, 2), Seed1), WyP1))
            Seed2 = WyMum(IEOR(IEOR(U64Rotate32(I32Val, 4), Seed2), WyP2), \
                            IEOR(Seed2, WyP3))
        ELSE
            Bytes(0:23) = Input(P:P+23)
            Bytes(24:31) = Input(P+I-8:P+I-1)
            Seed1 = WyMum(IEOR(IEOR(U64Rotate32(I32Val, 0), Seed1), WyP0), \
                            IEOR(IEOR(U64Rotate32(I32Val, 2), Seed1), WyP1))
            Seed2 = WyMum(IEOR(IEOR(U64Rotate32(I32Val, 4), Seed2), WyP2), \
                            IEOR(IEOR(U64Rotate32(I32Val, 6), Seed2), WyP3))
        END IF
        HashCode = WyMum(IEOR(Seed1, Seed2), IEOR(ToLong(Length), WyP4))
    END IF

    ! free pointers
    NULLIFY(I64Val)

    RETURN

#undef GetU8
#undef Pack_U32
#undef WyMum
#undef WyR3
#undef U64Rotate32

END FUNCTION WyV3_Hash64_10

!******************************************************************************

FUNCTION WyV3_Hash64_11(Input, Seed) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the WyHash hash algorithm by Wang Yi.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, TARGET, INTENT(IN)   :: Input(0:)    ! input bytes
    tLong,         INTENT(IN)   :: Seed         ! seed
    tLong                       :: HashCode     ! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong, POINTER  :: I64Val(:) => NULL()
    tLong           :: Seed1, Seed2, LHS1, RHS1, LHS2, RHS2
    tIndex          :: I, Length, P, Offset, Index
    tByte           :: Bytes(0:31)
    tInteger        :: I32Val(0:7)
    EQUIVALENCE(Bytes, I32Val)
    TYPE(C_PTR)     :: CPTR

!** FLOW

! define macros for better performance than using internal procedures
#define GetU8(In, Off)          MaskI8(In(Off))
#define Pack_U32(Val,Index)     MaskI32(Val(Index))
#define WyMum(LHS, RHS)         UnsignedLongMultiplyorFold(LHS, RHS)
#define WyR3(In, Index, K) \
    IOR(IOR(SHIFTL(ToLong(GetU8(In, Index)), 16),                   \
            SHIFTL(ToLong(GetU8(In, Index + SHIFTR(K, 1))), 8)),    \
                   ToLong(GetU8(In, Index + K - 1)))
#define U64Rotate32(Val,Index)  IOR(SHIFTL(MaskI32(Val(Index)), 32), MaskI32(Val(Index+1)))

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
        Bytes(0:3)  = Input(Offset:Offset+3)
        Bytes(4:7) = Input(Offset+Length-4:Offset+Length-1)
        LHS1 = IEOR(IEOR(Pack_U32(I32Val, 0), Seed1), WyP0)
        RHS1 = IEOR(IEOR(Pack_U32(I32Val, 1), Seed1), WyP1)
        LHS2 = IEOR(WyMum(LHS1, RHS1), Seed1)
        RHS2 = IEOR(ToLong(Length), WyP4)
        HashCode = WyMum(LHS2, RHS2)
    ELSEIF (Length <= 16) THEN
        Bytes(0:7)  = Input(Offset:Offset+7)
        Bytes(8:15) = Input(Offset+Length-8:Offset+Length-1)
        LHS1 = IEOR(IEOR(U64Rotate32(I32Val, 0), Seed1), WyP0)
        RHS1 = IEOR(IEOR(U64Rotate32(I32Val, 2), Seed1), WyP1)
        LHS2 = IEOR(WyMum(LHS1, RHS1), Seed1)
        RHS2 = IEOR(ToLong(Length), WyP4)
        HashCode = WyMum(LHS2, RHS2)
    ELSEIF (Length <= 24) THEN
        Bytes(0:15)  = Input(Offset:Offset+15)
        Bytes(16:23) = Input(Offset+Length-8:Offset+Length-1)
        LHS1 = IEOR(IEOR(U64Rotate32(I32Val, 0), Seed1), WyP0)
        RHS1 = IEOR(IEOR(U64Rotate32(I32Val, 2), Seed1), WyP1)
        LHS2 = IEOR(IEOR(U64Rotate32(I32Val, 4), Seed1), WyP2)
        RHS2 = IEOR(Seed1, WyP3)
        HashCode = WyMum(IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2)), \
                            IEOR(ToLong(Length), WyP4))
    ELSEIF (Length <= 32) THEN
        Bytes( 0:23) = Input(Offset:Offset+23)
        Bytes(24:31) = Input(Offset+Length-8:Offset+Length-1)
        LHS1 = IEOR(IEOR(U64Rotate32(I32Val, 0), Seed1), WyP0)
        RHS1 = IEOR(IEOR(U64Rotate32(I32Val, 2), Seed1), WyP1)
        LHS2 = IEOR(IEOR(U64Rotate32(I32Val, 4), Seed1), WyP2)
        RHS2 = IEOR(IEOR(U64Rotate32(I32Val, 6), Seed1), WyP3)
        HashCode = WyMum(IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2)), \
                            IEOR(ToLong(Length), WyP4))
    ELSE
        ! initialize
        Seed2 = Seed1
        I = Length
        P = Offset
        Index = 1
        CPTR = C_LOC(Input(0))
        CALL C_F_POINTER(CPTR, I64Val, SHAPE=[Length/8])
        DO WHILE(I > 256)
            LHS1 = IEOR(IEOR(I64Val(Index), Seed1), WyP0)
            RHS1 = IEOR(IEOR(I64Val(Index+1), Seed1), WyP1)
            LHS2 = IEOR(IEOR(I64Val(Index+2), Seed1), WyP2)
            RHS2 = IEOR(IEOR(I64Val(Index+3), Seed1), WyP3)
            Seed1 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(I64Val(Index+4), Seed2), WyP1)
            RHS1 = IEOR(IEOR(I64Val(Index+5), Seed2), WyP2)
            LHS2 = IEOR(IEOR(I64Val(Index+6), Seed2), WyP3)
            RHS2 = IEOR(IEOR(I64Val(Index+7), Seed2), WyP0)
            Seed2 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(I64Val(Index+8), Seed1), WyP0)
            RHS1 = IEOR(IEOR(I64Val(Index+9), Seed1), WyP1)
            LHS2 = IEOR(IEOR(I64Val(Index+10), Seed1), WyP2)
            RHS2 = IEOR(IEOR(I64Val(Index+11), Seed1), WyP3)
            Seed1 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(I64Val(Index+12), Seed2), WyP1)
            RHS1 = IEOR(IEOR(I64Val(Index+13), Seed2), WyP2)
            LHS2 = IEOR(IEOR(I64Val(Index+14), Seed2), WyP3)
            RHS2 = IEOR(IEOR(I64Val(Index+15), Seed2), WyP0)
            Seed2 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(I64Val(Index+16), Seed1), WyP0)
            RHS1 = IEOR(IEOR(I64Val(Index+17), Seed1), WyP1)
            LHS2 = IEOR(IEOR(I64Val(Index+18), Seed1), WyP2)
            RHS2 = IEOR(IEOR(I64Val(Index+19), Seed1), WyP3)
            Seed1 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(I64Val(Index+20), Seed2), WyP1)
            RHS1 = IEOR(IEOR(I64Val(Index+21), Seed2), WyP2)
            LHS2 = IEOR(IEOR(I64Val(Index+22), Seed2), WyP3)
            RHS2 = IEOR(IEOR(I64Val(Index+23), Seed2), WyP0)
            Seed2 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(I64Val(Index+24), Seed1), WyP0)
            RHS1 = IEOR(IEOR(I64Val(Index+25), Seed1), WyP1)
            LHS2 = IEOR(IEOR(I64Val(Index+26), Seed1), WyP2)
            RHS2 = IEOR(IEOR(I64Val(Index+27), Seed1), WyP3)
            Seed1 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            LHS1 = IEOR(IEOR(I64Val(Index+28), Seed2), WyP1)
            RHS1 = IEOR(IEOR(I64Val(Index+29), Seed2), WyP2)
            LHS2 = IEOR(IEOR(I64Val(Index+30), Seed2), WyP3)
            RHS2 = IEOR(IEOR(I64Val(Index+31), Seed2), WyP0)
            Seed2 = IEOR(WyMum(LHS1, RHS1), WyMum(LHS2, RHS2))

            I = I - 256
            P = P + 256
            Index = Index + 32
        END DO
        DO WHILE(I  > 32)
            LHS1 = IEOR(IEOR(I64Val(Index), Seed1), WyP0)
            RHS1 = IEOR(IEOR(I64Val(Index+1), Seed1), WyP1)
            Seed1 = WyMum(LHS1, RHS1)
            LHS2 = IEOR(IEOR(I64Val(Index+2), Seed2), WyP2)
            RHS2 = IEOR(IEOR(I64Val(Index+3), Seed2), WyP3)
            Seed2 = WyMum(LHS2, RHS2)
            I = I - 32
            P = P + 32
            Index = Index + 4
        END DO
        IF (I < 4) THEN
            Seed1 = WyMum(IEOR(IEOR(WyR3(Input, P, I), Seed1), WyP0), IEOR(Seed1, WyP1))
        ELSEIF (I <= 8)  THEN
            Bytes(0:3) = Input(P:P+3)
            Bytes(4:7) = Input(P+I-4:P+I-1)
            Seed1 = WyMum(IEOR(IEOR(Pack_U32(I32Val, 0), Seed1), WyP0), \
                            IEOR(IEOR(Pack_U32(I32Val, 1), Seed1), WyP1))
        ELSEIF (I <= 16)  THEN
            Bytes(0:7)  = Input(P:P+7)
            Bytes(8:15) = Input(P+I-8:P+I-1)
            Seed1 = WyMum(IEOR(IEOR(U64Rotate32(I32Val, 0), Seed1), WyP0), \
                            IEOR(IEOR(U64Rotate32(I32Val, 2), Seed1), WyP1))
        ELSEIF (I <= 24)  THEN
            Bytes(0:15)  = Input(P:P+15)
            Bytes(16:23) = Input(P+I-8:P+I-1)
            Seed1 = WyMum(IEOR(IEOR(U64Rotate32(I32Val, 0), Seed1), WyP0), \
                            IEOR(IEOR(U64Rotate32(I32Val, 2), Seed1), WyP1))
            Seed2 = WyMum(IEOR(IEOR(U64Rotate32(I32Val, 4), Seed2), WyP2), \
                            IEOR(Seed2, WyP3))
        ELSE
            Bytes(0:23) = Input(P:P+23)
            Bytes(24:31) = Input(P+I-8:P+I-1)
            Seed1 = WyMum(IEOR(IEOR(U64Rotate32(I32Val, 0), Seed1), WyP0), \
                            IEOR(IEOR(U64Rotate32(I32Val, 2), Seed1), WyP1))
            Seed2 = WyMum(IEOR(IEOR(U64Rotate32(I32Val, 4), Seed2), WyP2), \
                            IEOR(IEOR(U64Rotate32(I32Val, 6), Seed2), WyP3))
        END IF
        HashCode = WyMum(IEOR(Seed1, Seed2), IEOR(ToLong(Length), WyP4))
    END IF

    ! free pointers
    NULLIFY(I64Val)

    RETURN

#undef GetU8
#undef Pack_U32
#undef WyMum
#undef WyR3
#undef U64Rotate32

END FUNCTION WyV3_Hash64_11

!******************************************************************************

FUNCTION WyF3_Hash64(Input, SeedForHash, PackLong, PackInteger) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the final version 3 of the WyHash hash algorithm by Wang Yi.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, INTENT(IN)   :: Input(0:)    !! input bytes
    tLong, INTENT(IN)   :: SeedForHash  !! seed
    PROCEDURE(Pack_I64) :: PackLong     !! procedure to convert a byte array to 64-bit integer
    PROCEDURE(Pack_I32) :: PackInteger  !! procedure to convert a byte array to 64-bit integer
    tLong               :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Seed, Seed1, Seed2, A, B
    tLong       :: LHS1, RHS1, LHS2, RHS2
    tIndex      :: Length, Remaining, Offset, ShiftLen

!** SUBROUTINE MACRO DEFINITIONS:
#define GetU8(In, Off)      MaskI8(In(Off))
#define WyMix(A, B)         IEOR(A*B, UMul128_Upper64(A, B))
#define WyR3(In, Index, K) \
    IOR(IOR(SHIFTL(ToLong(GetU8(In, Index)), 16),                \
            SHIFTL(ToLong(GetU8(In, Index + SHIFTR(K, 1))), 8)), \
                   ToLong(GetU8(In, Index + K - 1)))
#define WyR4(In, Off)       MaskI32(PackInteger(In, Off))

!** FLOW

    ! initialize
    Length = SIZE(Input)
    Seed   = IEOR(SeedForHash, Secret0)

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
                LHS1 = IEOR(PackLong(Input, Offset), Secret1)
                RHS1 = IEOR(PackLong(Input, Offset + 8), Seed)
                Seed = WyMix(LHS1, RHS1)
                LHS1 = IEOR(PackLong(Input, Offset + 16), Secret2)
                RHS1 = IEOR(PackLong(Input, Offset + 24), Seed1)
                Seed1 = WyMix(LHS1, RHS1)
                LHS1 = IEOR(PackLong(Input, Offset + 32), Secret3)
                RHS1 = IEOR(PackLong(Input, Offset + 40), Seed2)
                Seed2 = WyMix(LHS1, RHS1)
                Offset = Offset + 48
                Remaining = Remaining - 48
                IF (Remaining <= 48) EXIT
            END DO
            Seed = IEOR(Seed, IEOR(Seed1, Seed2))
        END IF
        DO WHILE (Remaining > 16)
            LHS1 = IEOR(PackLong(Input, Offset), Secret1)
            RHS1 = IEOR(PackLong(Input, Offset + 8), Seed)
            Seed = WyMix(LHS1, RHS1)
            Remaining = Remaining - 16
            Offset = Offset + 16
        END DO
        A = PackLong(Input, Offset + Remaining - 16)
        B = PackLong(Input, Offset + Remaining - 8)
    END IF
    LHS1 = IEOR(A, Secret1)
    RHS1 = IEOR(B, Seed)
    LHS2 = IEOR(Secret1, ToLong(Length))
    RHS2 = WyMix(LHS1, RHS1)
    HashCode = WyMix(LHS2, RHS2)

    RETURN

#undef GetU8
#undef WyMix
#undef WyR3
#undef WyR4

END FUNCTION WyF3_Hash64

!******************************************************************************

FUNCTION WyF3_Hash64_08(Input, SeedForHash) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the WyHash hash (final version 3) algorithm by Wang Yi.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, TARGET, INTENT(IN)   :: Input(0:)    ! input bytes
    tLong,         INTENT(IN)   :: SeedForHash  ! seed
    tLong                       :: HashCode     ! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong, POINTER  :: LongVal(:) => NULL()
    tLong           :: Seed, Seed1, Seed2, A, B
    tLong           :: LHS1, RHS1, LHS2, RHS2
    tIndex          :: Length, Remaining, ShiftLen, Index
    tByte           :: ByteVal(0:15)
    tInteger        :: IntVal(1:4)
    EQUIVALENCE(ByteVal, IntVal)
    TYPE(C_PTR)     :: CPTR

!** FLOW

! define macros for better performance than using internal procedures
#define GetU8(In, Off)      MaskI8(In(Off))
#define WyR3(In, Index, K) \
    IOR(IOR(SHIFTL(ToLong(GetU8(In, Index)), 16),                \
            SHIFTL(ToLong(GetU8(In, Index + SHIFTR(K, 1))), 8)), \
                   ToLong(GetU8(In, Index + K - 1)))
#define WyMix(A, B)     IEOR(A*B, UMul128_Upper64(A, B))
#define WyR4(Inp,Off)   MaskI32(Inp(Off))

    ! initialize
    Length = SIZE(Input)
    Seed   = IEOR(SeedForHash, Secret0)

    ! perform hashing
    IF (Length <= 16) THEN
        IF (Length >= 4) THEN
            ShiftLen = SHIFTL(SHIFTR(Length, 3), 2)
            ByteVal(0:3)   = Input(0:3)
            ByteVal(4:7)   = Input(ShiftLen:+3)
            ByteVal(8:11)  = Input(Length-4:Length-1)
            ByteVal(12:15) = Input(Length-4-ShiftLen:Length-1-ShiftLen)
            A = IOR(SHIFTL(WyR4(IntVal, 1), 32), WyR4(IntVal, 2))
            B = IOR(SHIFTL(WyR4(IntVal, 3), 32), WyR4(IntVal, 4))
        ELSEIF (Length > 0) THEN
            A = WyR3(Input, 0, Length)
            B = 0_kLong
        ELSE
            A = 0_kLong
            B = 0_kLong
        END IF
    ELSE
        Remaining = Length
        Index = 1
        CPTR = C_LOC(Input(0))
        CALL C_F_POINTER(CPTR, LongVal, SHAPE=[Length/8])
        IF (Remaining > 48) THEN
            Seed1 = Seed
            Seed2 = Seed
            DO
                LHS1 = IEOR(LongVal(Index), Secret1)
                RHS1 = IEOR(LongVal(Index+1), Seed)
                Seed = WyMix(LHS1, RHS1)
                LHS1 = IEOR(LongVal(Index+2), Secret2)
                RHS1 = IEOR(LongVal(Index+3), Seed1)
                Seed1 = WyMix(LHS1, RHS1)
                LHS1 = IEOR(LongVal(Index+4), Secret3)
                RHS1 = IEOR(LongVal(Index+5), Seed2)
                Seed2 = WyMix(LHS1, RHS1)
                Index = Index + 6
                Remaining = Remaining - 48
                IF (Remaining <= 48) EXIT
            END DO
            Seed = IEOR(Seed, IEOR(Seed1, Seed2))
        END IF
        DO WHILE (Remaining > 16)
            LHS1 = IEOR(LongVal(Index), Secret1)
            RHS1 = IEOR(LongVal(Index+1), Seed)
            Seed = WyMix(LHS1, RHS1)
            Remaining = Remaining - 16
            Index = Index + 2
        END DO
        CPTR = C_LOC(Input(Length-16))
        CALL C_F_POINTER(CPTR, LongVal, SHAPE=[2])
        A = LongVal(1)
        B = LongVal(2)
    END IF
    LHS1 = IEOR(A, Secret1)
    RHS1 = IEOR(B, Seed)
    LHS2 = IEOR(Secret1, ToLong(Length))
    RHS2 = WyMix(LHS1, RHS1)
    HashCode = WyMix(LHS2, RHS2)

    RETURN

#undef GetU8
#undef WyMix
#undef WyR3
#undef WyR4

END FUNCTION WyF3_Hash64_08

!******************************************************************************

FUNCTION WyF3_Hash64_09(Input, SeedForHash) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the WyHash hash (final version 3) algorithm by Wang Yi.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, INTENT(IN)   :: Input(0:)    ! input bytes
    tLong, INTENT(IN)   :: SeedForHash  ! seed
    tLong               :: HashCode     ! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong           :: Seed, Seed1, Seed2, A, B
    tLong           :: LHS1, RHS1, LHS2, RHS2
    tIndex          :: Length, Remaining, Offset, ShiftLen
    tByte           :: ByteVal(0:47)
    tLong           :: LongVal(1:6)
    EQUIVALENCE(ByteVal, LongVal)
    tInteger        :: IntVal(1:4)
    EQUIVALENCE(ByteVal, IntVal)

!** FLOW

! define macros for better performance than using internal procedures
#define GetU8(In, Off)      MaskI8(In(Off))
#define WyR3(In, Index, K) \
    IOR(IOR(SHIFTL(ToLong(GetU8(In, Index)), 16),                \
            SHIFTL(ToLong(GetU8(In, Index + SHIFTR(K, 1))), 8)), \
                   ToLong(GetU8(In, Index + K - 1)))
#define WyMix(A, B)     IEOR(A*B, UMul128_Upper64(A, B))
#define WyR4(Inp,Off)   MaskI32(Inp(Off))

    ! initialize
    Length = SIZE(Input)
    Seed   = IEOR(SeedForHash, Secret0)

    ! perform hashing
    IF (Length <= 16) THEN
        IF (Length >= 4) THEN
            ShiftLen = SHIFTL(SHIFTR(Length, 3), 2)
            ByteVal(0:3)   = Input(0:3)
            ByteVal(4:7)   = Input(ShiftLen:+3)
            ByteVal(8:11)  = Input(Length-4:Length-1)
            ByteVal(12:15) = Input(Length-4-ShiftLen:Length-1-ShiftLen)
            A = IOR(SHIFTL(WyR4(IntVal, 1), 32), WyR4(IntVal, 2))
            B = IOR(SHIFTL(WyR4(IntVal, 3), 32), WyR4(IntVal, 4))
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
                ByteVal(0:47) = Input(Offset:Offset+47)
                LHS1 = IEOR(LongVal(1), Secret1)
                RHS1 = IEOR(LongVal(2), Seed)
                Seed = WyMix(LHS1, RHS1)
                LHS1 = IEOR(LongVal(3), Secret2)
                RHS1 = IEOR(LongVal(4), Seed1)
                Seed1 = WyMix(LHS1, RHS1)
                LHS1 = IEOR(LongVal(5), Secret3)
                RHS1 = IEOR(LongVal(6), Seed2)
                Seed2 = WyMix(LHS1, RHS1)
                Offset = Offset + 48
                Remaining = Remaining - 48
                IF (Remaining <= 48) EXIT
            END DO
            Seed = IEOR(Seed, IEOR(Seed1, Seed2))
        END IF
        DO WHILE (Remaining > 16)
            ByteVal(0:15) = Input(Offset:Offset+15)
            LHS1 = IEOR(LongVal(1), Secret1)
            RHS1 = IEOR(LongVal(2), Seed)
            Seed = WyMix(LHS1, RHS1)
            Remaining = Remaining - 16
            Offset = Offset + 16
        END DO
        ByteVal(0:15) = Input(Length-16:Length-1)
        A = LongVal(1)
        B = LongVal(2)
    END IF
    LHS1 = IEOR(A, Secret1)
    RHS1 = IEOR(B, Seed)
    LHS2 = IEOR(Secret1, ToLong(Length))
    RHS2 = WyMix(LHS1, RHS1)
    HashCode = WyMix(LHS2, RHS2)

    RETURN

#undef GetU8
#undef WyMix
#undef WyR3
#undef WyR4

END FUNCTION WyF3_Hash64_09

!******************************************************************************

FUNCTION WyF3_Hash64_10(Input, SeedForHash) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the WyHash hash (final version 3) algorithm by Wang Yi.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, TARGET, INTENT(IN)   :: Input(0:)    ! input bytes
    tLong,         INTENT(IN)   :: SeedForHash  ! seed
    tLong                       :: HashCode     ! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong, POINTER  :: LongVal(:) => NULL()
    tLong           :: Seed, Seed1, Seed2, A, B
    tLong           :: LHS1, RHS1, LHS2, RHS2
    tIndex          :: Length, Remaining, ShiftLen, Index
    tByte           :: ByteVal(0:15)
    tLong           :: LongVal2(1:6)
    EQUIVALENCE(ByteVal, LongVal2)
    tInteger        :: IntVal(1:4)
    EQUIVALENCE(ByteVal, IntVal)
    TYPE(C_PTR)     :: CPTR

!** FLOW

! define macros for better performance than using internal procedures
#define GetU8(In, Off)      MaskI8(In(Off))
#define WyR3(In, Index, K) \
    IOR(IOR(SHIFTL(ToLong(GetU8(In, Index)), 16),                \
            SHIFTL(ToLong(GetU8(In, Index + SHIFTR(K, 1))), 8)), \
                   ToLong(GetU8(In, Index + K - 1)))
#define WyMix(A, B)     IEOR(A*B, UMul128_Upper64(A, B))
#define WyR4(Inp,Off)   MaskI32(Inp(Off))

    ! initialize
    Length = SIZE(Input)
    Seed   = IEOR(SeedForHash, Secret0)

    ! perform hashing
    IF (Length <= 16) THEN
        IF (Length >= 4) THEN
            ShiftLen = SHIFTL(SHIFTR(Length, 3), 2)
            ByteVal(0:3)   = Input(0:3)
            ByteVal(4:7)   = Input(ShiftLen:+3)
            ByteVal(8:11)  = Input(Length-4:Length-1)
            ByteVal(12:15) = Input(Length-4-ShiftLen:Length-1-ShiftLen)
            A = IOR(SHIFTL(WyR4(IntVal, 1), 32), WyR4(IntVal, 2))
            B = IOR(SHIFTL(WyR4(IntVal, 3), 32), WyR4(IntVal, 4))
        ELSEIF (Length > 0) THEN
            A = WyR3(Input, 0, Length)
            B = 0_kLong
        ELSE
            A = 0_kLong
            B = 0_kLong
        END IF
    ELSE
        Remaining = Length
        Index = 1
        CPTR = C_LOC(Input(0))
        CALL C_F_POINTER(CPTR, LongVal, SHAPE=[Length/8])
        IF (Remaining > 48) THEN
            Seed1 = Seed
            Seed2 = Seed
            DO
                LHS1 = IEOR(LongVal(Index), Secret1)
                RHS1 = IEOR(LongVal(Index+1), Seed)
                Seed = WyMix(LHS1, RHS1)
                LHS1 = IEOR(LongVal(Index+2), Secret2)
                RHS1 = IEOR(LongVal(Index+3), Seed1)
                Seed1 = WyMix(LHS1, RHS1)
                LHS1 = IEOR(LongVal(Index+4), Secret3)
                RHS1 = IEOR(LongVal(Index+5), Seed2)
                Seed2 = WyMix(LHS1, RHS1)
                Index = Index + 6
                Remaining = Remaining - 48
                IF (Remaining <= 48) EXIT
            END DO
            Seed = IEOR(Seed, IEOR(Seed1, Seed2))
        END IF
        DO WHILE (Remaining > 16)
            LHS1 = IEOR(LongVal(Index), Secret1)
            RHS1 = IEOR(LongVal(Index+1), Seed)
            Seed = WyMix(LHS1, RHS1)
            Remaining = Remaining - 16
            Index = Index + 2
        END DO
        ByteVal(0:15) = Input(Length-16:Length-1)
        A = LongVal2(1)
        B = LongVal2(2)
    END IF
    LHS1 = IEOR(A, Secret1)
    RHS1 = IEOR(B, Seed)
    LHS2 = IEOR(Secret1, ToLong(Length))
    RHS2 = WyMix(LHS1, RHS1)
    HashCode = WyMix(LHS2, RHS2)

    RETURN

#undef WyMix
#undef WyR4

END FUNCTION WyF3_Hash64_10

!******************************************************************************

END SUBMODULE SubBase_WyHash_Exp

!******************************************************************************

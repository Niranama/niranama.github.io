
SUBMODULE (ModBase_ReferenceHash32) SubBase_NMHashV1_Ref

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains a reference implementation for version 1 of the NM hash algorithms
!   by James Z. M. Gao. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    USE Class_ByteConverter
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! primes from xxh!
    tInteger, PARAMETER     :: NMH_PRIME32_1 = ToInteger(Z'9E3779B1')
    tInteger, PARAMETER     :: NMH_PRIME32_2 = ToInteger(Z'85EBCA77')
    tInteger, PARAMETER     :: NMH_PRIME32_3 = ToInteger(Z'C2B2AE3D')
    tInteger, PARAMETER     :: NMH_PRIME32_4 = ToInteger(Z'27D4EB2F')
    ! pseudorandom secret taken directly from FARSH
    tInteger, PARAMETER     :: NMH_ACC_INIT(0:31) = [                   &
                        ToInteger(Z'71644897'), ToInteger(Z'A20DF94E'), &
                        ToInteger(Z'3819EF46'), ToInteger(Z'A9DEACD8'), &
                        ToInteger(Z'A8FA763F'), ToInteger(Z'E39C343F'), &
                        ToInteger(Z'F9DCBBC7'), ToInteger(Z'C70B4F1D'), &
                        ToInteger(Z'8A51E04B'), ToInteger(Z'CDB45931'), &
                        ToInteger(Z'C89F7EC9'), ToInteger(Z'D9787364'), &
                        ToInteger(Z'B8FE6C39'), ToInteger(Z'23A44BBE'), &
                        ToInteger(Z'7C01812C'), ToInteger(Z'F721AD1C'), &
                        ToInteger(Z'DED46DE9'), ToInteger(Z'839097DB'), &
                        ToInteger(Z'7240A4A4'), ToInteger(Z'B7B3671F'), &
                        ToInteger(Z'CB79E64E'), ToInteger(Z'CCC0E578'), &
                        ToInteger(Z'825AD07D'), ToInteger(Z'CCFF7221'), &
                        ToInteger(Z'B8084674'), ToInteger(Z'F743248E'), &
                        ToInteger(Z'E03590E6'), ToInteger(Z'813A264C'), &
                        ToInteger(Z'3C2852BB'), ToInteger(Z'91C300CB'), &
                        ToInteger(Z'88D0658B'), ToInteger(Z'1B532EA3')]
    tInteger, PARAMETER     :: NMH_M1 = ToInteger(Z'3E550CF1')
    tInteger, PARAMETER     :: NMH_M2 = ToInteger(Z'9E1B7E49')
    tInteger, PARAMETER     :: NMH_M3 = ToInteger(Z'CCE5196D')
    tInteger, PARAMETER     :: NMH_M4 = ToInteger(Z'464BE229')
    tLogical, PARAMETER     :: NMH_SHORT32_WITHOUT_SEED2 = FalseVal
    tLogical, PARAMETER     :: NMH_SHORT32_WITH_SEED2    = TrueVal
    tLogical, PARAMETER     :: NMH_AVALANCHE_FULL  = FalseVal
    tLogical, PARAMETER     :: NMH_AVALANCHE_INNER = TrueVal
    tIndex,   PARAMETER     :: ACC_SIZE = SIZE(NMH_ACC_INIT)

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE FUNCTION NMHash_V1_I32(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using version 1 of the NMHASH hash algorithm
    !  by James Z. M. Gao. <br>
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
    HashCode = NM_V1_Hash32(InpPtr, Seed, ByteConv)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION NMHash_V1_I32

!******************************************************************************

MODULE FUNCTION NMxHash_V1_I32(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using version 1 of the NMxHASH hash algorithm
    !  by James Z. M. Gao. <br>
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
    HashCode = NMx_V1_Hash32(InpPtr, Seed, ByteConv)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION NMxHash_V1_I32

!******************************************************************************

FUNCTION NM_V1_Hash32(Input, Seed, BC) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using version 1 of the NMHASH hash algorithm
    !  by James Z. M. Gao.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN) :: Input(0:)    !! input bytes
    tInteger,            INTENT(IN) :: Seed         !! seed
    TYPE(ByteConverter), INTENT(IN) :: BC           !! byte converter
    tInteger                        :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: XU32
    tIndex      :: Length

!** FLOW

#define NMHASH32_Mix32(H,X,AvType)  NMHASH32_Avalanche32(IEOR(H, X), AvType)

    ! initialize
    Length = SIZE(Input)

    IF (Length <= 16) THEN
        IF (Length > 4) THEN
            HashCode = NMHASH32_5to127(Input, Length, Seed, NMH_AVALANCHE_INNER)
        ELSEIF (Length > 0) THEN
            XU32 = BC%Pack_I32_Partial(Input, 0, Length)
            HashCode = -NMHASH32_Short32(IEOR((NMH_PRIME32_4 + Seed), XU32), &
                                            IEOR(ToInteger(Length), Seed),      &
                                            NMH_SHORT32_WITH_SEED2)
        ELSE
            HashCode = NMHASH32_Short32(NMH_PRIME32_1 + Seed, 0, NMH_SHORT32_WITHOUT_SEED2)
        END IF
        RETURN
    END IF
    IF (Length < 128) THEN
        HashCode = NMHASH32_5to127(Input, Length, Seed, NMH_AVALANCHE_FULL)
    ELSE
        HashCode = NMHASH32_Long(Input, Length, Seed)
    END IF

    RETURN

CONTAINS

    FUNCTION NMHASH32_Short32(X, Seed2, WithSeed2) RESULT(HashCode)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, INTENT(IN)    :: X
        tInteger, INTENT(IN)    :: Seed2
        tLogical, INTENT(IN)    :: WithSeed2
        tInteger                :: HashCode

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: VxU32
        tShort      :: VxU16(0:1)
        EQUIVALENCE(VxU16, VxU32)
        tInteger    :: M1_32, M2_32
        tShort      :: M1_16(0:1), M2_16(0:1)
        EQUIVALENCE(M1_16, M1_32)
        EQUIVALENCE(M2_16, M2_32)

    !** FLOW

        ! initialize
        M1_32 = NMH_M1
        M2_32 = NMH_M2
        VxU32 = X

        ! avalanche
        VxU32 = IEOR(VxU32, IEOR(SHIFTL(VxU32, 18), SHIFTR(VxU32, 22)))
        VxU32 = IEOR(VxU32, IEOR(SHIFTR(VxU32, 11), SHIFTR(VxU32, 13)))
        VxU16 = VxU16*M1_16
        IF (NMH_SHORT32_WITH_SEED2 .EQV. WithSeed2) VxU32 = VxU32 + Seed2
        VxU32 = IEOR(VxU32, IEOR(SHIFTL(VxU32, 15), SHIFTR(VxU32, 24)))
        VxU32 = IEOR(VxU32, IEOR(SHIFTR(VxU32,  6), SHIFTR(VxU32, 21)))
        VxU16 = VxU16*M2_16
        VxU32 = IEOR(VxU32, IEOR(SHIFTL(VxU32,  7), SHIFTR(VxU32, 13)))
        VxU32 = IEOR(VxU32, IEOR(SHIFTL(VxU32, 15), SHIFTR(VxU32, 11)))
        HashCode = VxU32

        RETURN

    END FUNCTION NMHASH32_Short32

    !******************************************************************************

    FUNCTION NMHASH32_Avalanche32(X, AvType) RESULT(HashCode)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, INTENT(IN)    :: X
        tLogical, INTENT(IN)    :: AvType
        tInteger                :: HashCode

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: VxU32
        tShort      :: VxU16(0:1)
        EQUIVALENCE(VxU16, VxU32)
        tInteger    :: M3_32, M4_32
        tShort      :: M3_16(0:1), M4_16(0:1)
        EQUIVALENCE(M3_16, M3_32)
        EQUIVALENCE(M4_16, M4_32)

    !** FLOW

        ! initialize
        M3_32 = NMH_M3
        M4_32 = NMH_M4
        VxU32 = X

        ! avalanche
        VxU32 = IEOR(VxU32, IEOR(SHIFTR(VxU32, 8), SHIFTR(VxU32, 21)))
        VxU16 = VxU16*M3_16
        VxU32 = IEOR(VxU32, IEOR(SHIFTL(VxU32, 12), SHIFTR(VxU32, 7)))
        VxU16 = VxU16*M4_16
        IF (NMH_AVALANCHE_FULL .EQV. AvType) THEN
            HashCode = IEOR(IEOR(VxU32, SHIFTR(VxU32, 8)), SHIFTR(VxU32, 21))
        ELSE
            HashCode = VxU32
        END IF

        RETURN

    END FUNCTION NMHASH32_Avalanche32

    !******************************************************************************

    FUNCTION NMHASH32_5to127(P, Length, Seed, AvType) RESULT(HashCode)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,    INTENT(IN)    :: P(0:)
        tIndex,   INTENT(IN)    :: Length
        tInteger, INTENT(IN)    :: Seed
        tLogical, INTENT(IN)    :: AvType
        tInteger                :: HashCode

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: A, B, C, D
        tIndex      :: NbRounds, I

    !** FLOW

        A = NMH_PRIME32_1 + Seed
        B = NMH_PRIME32_2 + Seed
        C = NMH_PRIME32_3 + Seed
        D = NMH_PRIME32_4 + Seed

        IF (NMH_AVALANCHE_FULL .EQV. AvType) THEN
            ! 17 to 127 Bytes
            NbRounds = ToInteger(Length - 1)/16
            DO I = 0, NbRounds-1
                A = NMHASH32_Mix32(A, BC%Pack_I32(P, I*16),      NMH_AVALANCHE_INNER)
                B = NMHASH32_Mix32(B, BC%Pack_I32(P, I*16 + 4),  NMH_AVALANCHE_INNER)
                C = NMHASH32_Mix32(C, BC%Pack_I32(P, I*16 + 8),  NMH_AVALANCHE_INNER)
                D = NMHASH32_Mix32(D, BC%Pack_I32(P, I*16 + 12), NMH_AVALANCHE_INNER)
            END DO
            A = NMHASH32_Mix32(A, BC%Pack_I32(P, Length - 16), NMH_AVALANCHE_FULL)
            B = NMHASH32_Mix32(B, BC%Pack_I32(P, Length - 12), NMH_AVALANCHE_FULL)
            C = NMHASH32_Mix32(C, BC%Pack_I32(P, Length -  8), NMH_AVALANCHE_FULL)
            D = NMHASH32_Mix32(D, BC%Pack_I32(P, Length -  4), NMH_AVALANCHE_FULL)
        ELSE
            ! 5 to 16 Bytes
            A = NMHASH32_Mix32(A, BC%Pack_I32(P, 0), NMH_AVALANCHE_FULL)
            B = NMHASH32_Mix32(B, BC%Pack_I32(P, SHIFTL(SHIFTA(Length, 3), 2)), \
                                NMH_AVALANCHE_FULL)
            C = NMHASH32_Mix32(C, BC%Pack_I32(P, Length - 4), NMH_AVALANCHE_FULL)
            D = NMHASH32_Mix32(D, BC%Pack_I32(P, Length - 4 - SHIFTL(SHIFTA(Length, 3), 2)), \
                                NMH_AVALANCHE_FULL)
        END IF

        A = IEOR(A, NMH_PRIME32_1)
        B = IEOR(B, NMH_PRIME32_2)
        C = IEOR(C, NMH_PRIME32_3)
        D = IEOR(D, NMH_PRIME32_4)

        HAshCoDe = NMHASH32_Mix32(A + B + C + D, ToInteger(Length) + Seed, NMH_AVALANCHE_FULL)

        RETURN

    END FUNCTION NMHASH32_5to127

    !******************************************************************************

    SUBROUTINE NMHASH32_LongRound(Acc, P, Off, AvType)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, INTENT(INOUT) :: Acc(0:)
        tByte,    INTENT(IN)    :: P(0:)
        tIndex,   INTENT(IN)    :: Off
        tLogical, INTENT(IN)    :: AvType

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: A, B, C, D
        tIndex      :: I, Offset
        tInteger    :: AccU32
        tShort      :: Acc_16(0:1)
        EQUIVALENCE(Acc_16, AccU32)
        tInteger    :: NMH_M3_32
        tShort      :: NMH_M3_16(0:1)
        EQUIVALENCE(NMH_M3_16, NMH_M3_32)
        tInteger    :: NMH_M4_32
        tShort      :: NMH_M4_16(0:1)
        EQUIVALENCE(NMH_M4_16, NMH_M4_32)

    !** FLOW

        ! initialize
        Offset = Off
        NMH_M3_32 = NMH_M3
        NMH_M4_32 = NMH_M4

        DO I = 0, ACC_SIZE-1
            ! mixing input and constants
            Acc(I) = IEOR(Acc(I), BC%Pack_I32(P, Offset))
            Acc(I) = IEOR(Acc(I), IEOR(SHIFTR(Acc(I), 8), SHIFTR(Acc(I), 21)))
            AccU32 = Acc(I)
            Acc_16 = Acc_16*NMH_M3_16
            Acc(I) = AccU32
            Acc(I) = IEOR(Acc(I), IEOR(SHIFTL(Acc(I), 12), SHIFTR(Acc(I), 7)))
            AccU32 = Acc(I)
            Acc_16 = Acc_16*NMH_M4_16
            Acc(I) = AccU32
            ! update index
            Offset = Offset + 4
        END DO

        IF (NMH_AVALANCHE_FULL .EQV. AvType) THEN
            DO I = 0, ACC_SIZE-1
                Acc(I) = IEOR(Acc(I), IEOR(SHIFTR(Acc(I), 8), SHIFTR(Acc(I), 21)))
            END DO
        END IF

        RETURN

    END SUBROUTINE NMHASH32_LongRound

    !******************************************************************************

    FUNCTION NMHASH32_MergeAcc(Acc, Length) RESULT(ResVal)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, INTENT(INOUT) :: Acc(0:)
        tIndex,   INTENT(IN)    :: Length
        tInteger                :: ResVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: I
        tInteger    :: Sum

    !** FLOW

        Sum = ToInteger(SHIFTA(Length, 32))
        DO I = 0, ACC_SIZE-1
            Acc(I) = IEOR(Acc(I), NMH_ACC_INIT(I))
            Sum = Sum + Acc(I)
        END DO

        ResVal = NMHASH32_Mix32(Sum, ToInteger(Length), NMH_AVALANCHE_FULL)

        RETURN

    END FUNCTION NMHASH32_MergeAcc

    !******************************************************************************

    FUNCTION NMHASH32_Long(P, Length, Seed) RESULT(HashCode)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,    INTENT(IN)    :: P(0:)
        tIndex,   INTENT(IN)    :: Length
        tInteger, INTENT(IN)    :: Seed
        tInteger                :: HashCode

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: Acc(0:ACC_SIZE-1)
        tIndex      :: NbRounds, I, SizeOfACC

    !** FLOW

        ! initialize
        SizeOfACC = ACC_SIZE*4
        NbRounds = (Length-1)/SizeOfACC
        DO I = 0, ACC_SIZE-1
            Acc(I) = NMH_ACC_INIT(I) + Seed
        END DO

        DO I = 0, NbRounds-1
            CALL NMHASH32_LongRound(Acc, P, I*SizeOfACC, NMH_AVALANCHE_INNER)
        END DO
        CALL NMHASH32_LongRound(Acc, P, Length - SizeOfACC, NMH_AVALANCHE_FULL)

        HashCode = NMHASH32_MergeAcc(Acc, Length)

        RETURN

    END FUNCTION NMHASH32_Long

    !******************************************************************************

#undef NMHASH32_Mix32

END FUNCTION NM_V1_Hash32

!******************************************************************************

FUNCTION NMx_V1_Hash32(Input, Seed, BC) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using version 1 of the NMxHASH hash algorithm
    !  by James Z. M. Gao.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN) :: Input(0:)    !! input bytes
    tInteger,            INTENT(IN) :: Seed         !! seed
    TYPE(ByteConverter), INTENT(IN) :: BC           !! byte converter
    tInteger                        :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: XU32, Seed2
    tIndex      :: Length

!** FLOW

    ! initialize
    Length = SIZE(Input)

    IF (Length <= 8) THEN
        IF (Length > 4) THEN
            HashCode = NMxHASH32_4to8(Input, Length, Seed)
        ELSEIF (Length > 0) THEN
            Seed2 = Seed + ToInteger(Length)
            XU32 = IOR(SHIFTL(ToInteger(Length), 16), BC%Pack_I32_Partial(Input, 0, Length))
            HashCode = NMxHASH32_Avalanche32_M3(IEOR((NMH_PRIME32_4 + Seed2), XU32), Seed2)
        ELSE
            HashCode = NMxHASH32_Avalanche32_M2(IEOR(NMH_PRIME32_1, Seed))
        END IF
        RETURN
    END IF
    IF (Length < 128) THEN
        HashCode = NMxHASH32_9to127(Input, Length, Seed)
    ELSE
        HashCode = NMHASH32_Long(Input, Length, Seed)
    END IF

    RETURN

CONTAINS

    FUNCTION NMxHASH32_Avalanche32_M2(X) RESULT(HashCode)

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, INTENT(IN)    :: X
        tInteger                :: HashCode

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tInteger, PARAMETER :: M1 = ToInteger(Z'D168AAAD')
        tInteger, PARAMETER :: M2 = ToInteger(Z'AF723597')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        ! initialize
        HashCode = X

        HashCode = IEOR(HashCode, SHIFTR(HashCode, 15))
        HashCode = HashCode*M1
        HashCode = IEOR(HashCode, SHIFTR(HashCode, 15))
        HashCode = HashCode*M2
        HashCode = IEOR(HashCode, SHIFTR(HashCode, 15))

        RETURN

    END FUNCTION NMxHASH32_Avalanche32_M2

    !******************************************************************************

    FUNCTION NMxHASH32_Avalanche32_M3(X, Seed) RESULT(HashCode)

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, INTENT(IN)    :: X
        tInteger, INTENT(IN)    :: Seed
        tInteger                :: HashCode

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tInteger, PARAMETER :: M1 = ToInteger(Z'4C749D4B')
        tInteger, PARAMETER :: M2 = ToInteger(Z'A8437449')
        tInteger, PARAMETER :: M3 = ToInteger(Z'4BF49D4B')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        ! initialize
        HashCode = X

        HashCode = IEOR(HashCode, SHIFTR(HashCode, 16))
        HashCode = HashCode*M1
        HashCode = HashCode + Seed
        HashCode = IEOR(HashCode, SHIFTR(HashCode, 14))
        HashCode = HashCode*M2
        HashCode = IOR(SHIFTL(HashCode, 12), SHIFTR(HashCode, 20))
        HashCode = HashCode*M3
        HashCode = IEOR(HashCode, SHIFTR(HashCode, 16))

        RETURN

    END FUNCTION NMxHASH32_Avalanche32_M3

    !******************************************************************************

    FUNCTION NMxHASH32_4to8(P, Length, Seed) RESULT(HashCode)

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,    INTENT(IN)    :: P(0:)
        tIndex,   INTENT(IN)    :: Length
        tInteger, INTENT(IN)    :: Seed
        tInteger                :: HashCode

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tInteger, PARAMETER :: M1 = ToInteger(Z'A52FB2CD')
        tInteger, PARAMETER :: M2 = ToInteger(Z'551E4D49')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: X, Y

    !** FLOW

        X = NMH_PRIME32_1 + ToShort(IAND(Seed, Z'0000FFFF'))
        Y = NMH_PRIME32_2 + Seed

        IF (Length /= 4) THEN
            X = IEOR(X, BC%Pack_I32(P, 0))
            X = IEOR(X, SHIFTR(X, 16))
            X = X*M1
            X = IEOR(X, SHIFTR(X, 15))
            X = X*M2
        END IF

        Y = IEOR(Y, BC%Pack_I32(P, Length - 4))
        Y = IEOR(Y, SHIFTR(Y, 16))
        Y = Y*M1
        Y = IEOR(Y, SHIFTR(Y, 15))
        Y = Y*M2

        X = X + ToInteger(Length)
        Y = IOR(SHIFTL(Y, 3), SHIFTR(Y, (32 - 3)))
        X = IEOR(X, Y)

        X = IEOR(X, SHIFTR(X, 16))
        X = X*M1
        X = IEOR(X, SHIFTR(X, 17))
        HashCode = X

        RETURN

    END FUNCTION NMxHASH32_4to8

    !******************************************************************************

    FUNCTION NMxHASH32_9to127(P, Length, Seed) RESULT(HashCode)

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,    INTENT(IN)    :: P(0:)
        tIndex,   INTENT(IN)    :: Length
        tInteger, INTENT(IN)    :: Seed
        tInteger                :: HashCode

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tInteger, PARAMETER :: M1 = ToInteger(Z'A52FB2CD')
        tInteger, PARAMETER :: M2 = ToInteger(Z'551E4D49')
        tInteger, PARAMETER :: M3 = ToInteger(Z'141CC535')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: X, Y
        tIndex      :: R, I, Offset

    !** FLOW

        ! initialize
        X = NMH_PRIME32_1 + Seed
        Y = NMH_PRIME32_4 + ToShort(IAND(Seed, Z'0000FFFF'))    ! avoid bad Seeds
        R = (Length - 1) / 8
        Offset = 0

        DO I = 0, R-1
            Y = IEOR(Y,BC%Pack_I32(P, Offset + 4))
            Y = IEOR(Y, SHIFTR(Y, 16))
            Y = Y*M1
            Y = IEOR(Y, SHIFTR(Y, 15))
            Y = Y*M2

            X = IEOR(X, BC%Pack_I32(P, Offset))
            X = IEOR(X, SHIFTR(X, 16))
            X = X*M1
            X = IEOR(X, SHIFTR(X, 15))
            X = X*M2

            Offset = Offset + 8
        END DO

        IF (IAND(ToInteger(Length)-1, 4) /= 0) THEN
            X = IEOR(X, BC%Pack_I32(P, Offset))
            X = IEOR(X, SHIFTR(X, 16))
            X = X*M1
            X = IEOR(X, SHIFTR(X, 15))
            X = X*M2
        END IF

        Y = IEOR(Y,BC%Pack_I32(P, Length - 4))
        Y = IEOR(Y, SHIFTR(Y, 16))
        Y = Y*M1
        Y = IEOR(Y, SHIFTR(Y, 15))
        Y = Y*M2

        X = X + ToInteger(Length)
        Y = IOR(SHIFTL(Y, 3), SHIFTR(Y, (32 - 3)))  ! rotate one lane to pass Diff test */
        X = IEOR(X, Y)

        X = X*M3
        X = IEOR(X, SHIFTR(X, 13))
        HashCode = X

        RETURN

    END FUNCTION NMxHASH32_9to127

    !******************************************************************************

    SUBROUTINE NMHASH32_LongRound(Acc, P, Off, AvType)

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, INTENT(INOUT) :: Acc(0:)
        tByte,    INTENT(IN)    :: P(0:)
        tIndex,   INTENT(IN)    :: Off
        tLogical, INTENT(IN)    :: AvType

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: A, B, C, D
        tIndex      :: I, Offset
        tInteger    :: AccU32
        tShort      :: Acc_16(0:1)
        EQUIVALENCE(Acc_16, AccU32)
        tInteger    :: NMH_M3_32
        tShort      :: NMH_M3_16(0:1)
        EQUIVALENCE(NMH_M3_16, NMH_M3_32)
        tInteger    :: NMH_M4_32
        tShort      :: NMH_M4_16(0:1)
        EQUIVALENCE(NMH_M4_16, NMH_M4_32)

    !** FLOW

        ! initialize
        Offset = Off
        NMH_M3_32 = NMH_M3
        NMH_M4_32 = NMH_M4

        DO I = 0, ACC_SIZE-1
            ! mixing input and constants
            Acc(I) = IEOR(Acc(I), BC%Pack_I32(P, Offset))
            Acc(I) = IEOR(Acc(I), IEOR(SHIFTR(Acc(I), 8), SHIFTR(Acc(I), 21)))
            AccU32 = Acc(I)
            Acc_16 = Acc_16*NMH_M3_16
            Acc(I) = AccU32
            Acc(I) = IEOR(Acc(I), IEOR(SHIFTL(Acc(I), 12), SHIFTR(Acc(I), 7)))
            AccU32 = Acc(I)
            Acc_16 = Acc_16*NMH_M4_16
            Acc(I) = AccU32
            ! update index
            Offset = Offset + 4
        END DO

        IF (NMH_AVALANCHE_FULL .EQV. AvType) THEN
            DO I = 0, ACC_SIZE-1
                Acc(I) = IEOR(Acc(I), IEOR(SHIFTR(Acc(I), 8), SHIFTR(Acc(I), 21)))
            END DO
        END IF

        RETURN

    END SUBROUTINE NMHASH32_LongRound

    !******************************************************************************

    FUNCTION NMHASH32_MergeAcc(Acc, Length) RESULT(ResVal)

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, INTENT(INOUT) :: Acc(0:)
        tIndex,   INTENT(IN)    :: Length
        tInteger                :: ResVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: I
        tInteger    :: Sum

    !** FLOW

        Sum = ToInteger(SHIFTA(Length, 32))
        DO I = 0, ACC_SIZE-1
            Acc(I) = IEOR(Acc(I), NMH_ACC_INIT(I))
            Sum = Sum + Acc(I)
        END DO

        ResVal = NMxHASH32_Avalanche32_M2(IEOR(Sum, ToInteger(Length)))

        RETURN

    END FUNCTION NMHASH32_MergeAcc

    !******************************************************************************

    FUNCTION NMHASH32_Long(P, Length, Seed) RESULT(HashCode)

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,    INTENT(IN)    :: P(0:)
        tIndex,   INTENT(IN)    :: Length
        tInteger, INTENT(IN)    :: Seed
        tInteger                :: HashCode

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: Acc(0:ACC_SIZE-1)
        tIndex      :: NbRounds, I, SizeOfACC

    !** FLOW

        ! initialize
        SizeOfACC = ACC_SIZE*4
        NbRounds = (Length-1)/SizeOfACC
        DO I = 0, ACC_SIZE-1
            Acc(I) = NMH_ACC_INIT(I) + Seed
        END DO

        DO I = 0, NbRounds-1
            CALL NMHASH32_LongRound(Acc, P, I*SizeOfACC, NMH_AVALANCHE_INNER)
        END DO
        CALL NMHASH32_LongRound(Acc, P, Length - SizeOfACC, NMH_AVALANCHE_FULL)

        HashCode = NMHASH32_MergeAcc(Acc, Length)

        RETURN

    END FUNCTION NMHASH32_Long

    !******************************************************************************

END FUNCTION NMx_V1_Hash32

!******************************************************************************

END SUBMODULE SubBase_NMHashV1_Ref

!******************************************************************************

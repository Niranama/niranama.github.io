
SUBMODULE (ModBase_ExperimentalHash32) SubBase_NMHashV2_Exp

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an experimental implementation for version 2 of the NM hash algorithms
!   by James Z. M. Gao. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    
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
    ! Pseudorandom secret taken directly from FARSH
    tInteger, PARAMETER   :: NMH_ACC_INIT(0:31) = [                  &
                        ToInteger(Z'B8FE6C39'), ToInteger(Z'23A44BBE'), &
                        ToInteger(Z'7C01812C'), ToInteger(Z'F721AD1C'), &
                        ToInteger(Z'DED46DE9'), ToInteger(Z'839097DB'), &
                        ToInteger(Z'7240A4A4'), ToInteger(Z'B7B3671F'), &
                        ToInteger(Z'CB79E64E'), ToInteger(Z'CCC0E578'), &
                        ToInteger(Z'825AD07D'), ToInteger(Z'CCFF7221'), &
                        ToInteger(Z'B8084674'), ToInteger(Z'F743248E'), &
                        ToInteger(Z'E03590E6'), ToInteger(Z'813A264C'), &
                        ToInteger(Z'3C2852BB'), ToInteger(Z'91C300CB'), &
                        ToInteger(Z'88D0658B'), ToInteger(Z'1B532EA3'), &
                        ToInteger(Z'71644897'), ToInteger(Z'A20DF94E'), &
                        ToInteger(Z'3819EF46'), ToInteger(Z'A9DEACD8'), &
                        ToInteger(Z'A8FA763F'), ToInteger(Z'E39C343F'), &
                        ToInteger(Z'F9DCBBC7'), ToInteger(Z'C70B4F1D'), &
                        ToInteger(Z'8A51E04B'), ToInteger(Z'CDB45931'), &
                        ToInteger(Z'C89F7EC9'), ToInteger(Z'D9787364')]
    tInteger, PARAMETER     :: NMH_M1 = ToInteger(Z'F0D9649B')
    tInteger, PARAMETER     :: NMH_M2 = ToInteger(Z'29A7935D')
    tInteger, PARAMETER     :: NMH_M3 = ToInteger(Z'55D35831')
    tIndex,   PARAMETER     :: ACC_SIZE = SIZE(NMH_ACC_INIT)

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE FUNCTION NM_Hash32_Exp(Input, InpSize, Algo, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using version 2 of the NMHASH hash algorithm
    !  by James Z. M. Gao. <br>
    !  This is an interface routine that calls the working routine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), DIMENSION(..), INTENT(IN)  :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
    tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-7)
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
    SELECT CASE (Algo)
    CASE (1)
        HashCode = NM_V2_Hash32(InpPtr, Seed, Pack_I32_A1)
    CASE (2)
        HashCode = NM_V2_Hash32(InpPtr, Seed, Pack_I32_A2)
    CASE (3)
        HashCode = NM_V2_Hash32(InpPtr, Seed, Pack_I32_A3)
    CASE (4)
        HashCode = NM_V2_Hash32(InpPtr, Seed, Pack_I32_A4)
    CASE (5)
        HashCode = NM_V2_Hash32(InpPtr, Seed, Pack_I32_A5)
    CASE (6)
        HashCode = NM_V2_Hash32(InpPtr, Seed, Pack_I32_A6)
    CASE (7)
        HashCode = NM_V2_Hash32(InpPtr, Seed, Pack_I32_A7)
    END SELECT

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION NM_Hash32_Exp

!******************************************************************************

MODULE FUNCTION NMx_Hash32_Exp(Input, InpSize, Algo, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using version 2 of the NMxHASH hash algorithm
    !  by James Z. M. Gao. <br>
    !  This is an interface routine that calls the working routine.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(*), DIMENSION(..), INTENT(IN)  :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
    tIndex,                 INTENT(IN)  :: Algo         !! algorithm flag (1-7)
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
    SELECT CASE (Algo)
    CASE (1)
        HashCode = NMx_V2_Hash32(InpPtr, Seed, Pack_I32_A1)
    CASE (2)
        HashCode = NMx_V2_Hash32(InpPtr, Seed, Pack_I32_A2)
    CASE (3)
        HashCode = NMx_V2_Hash32(InpPtr, Seed, Pack_I32_A3)
    CASE (4)
        HashCode = NMx_V2_Hash32(InpPtr, Seed, Pack_I32_A4)
    CASE (5)
        HashCode = NMx_V2_Hash32(InpPtr, Seed, Pack_I32_A5)
    CASE (6)
        HashCode = NMx_V2_Hash32(InpPtr, Seed, Pack_I32_A6)
    CASE (7)
        HashCode = NMx_V2_Hash32(InpPtr, Seed, Pack_I32_A7)
    END SELECT

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION NMx_Hash32_Exp

!******************************************************************************

FUNCTION NM_V2_Hash32(Input, Seed, PackFull) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using version 2 of the NMHASH hash algorithm
    !  by James Z. M. Gao.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)    :: Input(0:)    !! input bytes
    tInteger, INTENT(IN)    :: Seed         !! seed
    PROCEDURE(Pack_I32)     :: PackFull     !! procedure to convert a byte array to 32-bit integer
    tInteger                :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Length

!** FLOW

#define NMHASH32_9to32(P, Len, Seed)    NMHASH32_9to255(P, Len, Seed, FalseVal)
#define NMHASH32_33to255(P, Len, Seed)  NMHASH32_9to255(P, Len, Seed, TrueVal)

    ! initialize
    Length = SIZE(Input)
        
    IF (Length <= 32) THEN
        IF (Length > 8) THEN
            HashCode = NMHASH32_9to32(Input, Length, Seed)
        ELSEIF (Length > 4) THEN
            BLOCK
                tInteger    :: X, Y
                X = PackFull(Input, 0)
                Y = IEOR(PackFull(Input, Length - 4), NMH_PRIME32_4 + 2 + Seed)
                X = X + Y
                X = IEOR(X, SHIFTL(X, Length + 7))
                HashCode = NMHASH32_0to8(X, RotateLeft(Y, 5))
            END BLOCK
        ELSE
            BLOCK
                tInteger    :: X, NewSeed
                SELECT CASE (Length)
                CASE(0)
                    NewSeed = Seed + NMH_PRIME32_2
                    X = 0
                CASE(1:3)
                    NewSeed = Seed + NMH_PRIME32_2 + SHIFTL(Length, 24) + SHIFTL(Length, 1)
                    X = PackPartial(Input, 0, Length, PackFull)
                CASE(4)
                    NewSeed = Seed + NMH_PRIME32_3
                    X = PackFull(Input, 0)
                END SELECT
                HashCode = NMHASH32_0to8(X + NewSeed, RotateLeft(NewSeed, 5))
            END BLOCK
        END IF
        RETURN
    END IF
    IF (Length < 256) THEN
        HashCode = NMHASH32_33to255(Input, Length, Seed)
    ELSE
        HashCode = NMHASH32_Avalanche32(NMHASH32_Long(Input, Length, Seed))
    END IF

    RETURN

CONTAINS

    FUNCTION NMHASH32_0to8(X, Seed) RESULT(HashCode)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, INTENT(IN)    :: X
        tInteger, INTENT(IN)    :: Seed
        tInteger                :: HashCode

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tInteger, PARAMETER :: M1 = ToInteger(Z'776BF593')
        tInteger, PARAMETER :: M2 = ToInteger(Z'3FB39C65')
        tInteger, PARAMETER :: M3 = ToInteger(Z'E9139917')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: VxU32
        tShort      :: VxU16(0:1)
        EQUIVALENCE(VxU16, VxU32)
        tInteger    :: M1_32, M2_32, M3_32
        tShort      :: M1_16(0:1), M2_16(0:1), M3_16(0:1)
        EQUIVALENCE(M1_16, M1_32)
        EQUIVALENCE(M2_16, M2_32)
        EQUIVALENCE(M3_16, M3_32)

    !** FLOW

        ! initialize
        M1_32 = M1
        M2_32 = M2
        M3_32 = M3
        VxU32 = X

        ! avalanche
        VxU32 = IEOR(VxU32, IEOR(SHIFTR(VxU32, 12), SHIFTR(VxU32, 6)))
        VxU16 = VxU16*M1_16
        VxU32 = IEOR(VxU32, IEOR(SHIFTL(VxU32, 11), SHIFTR(VxU32, 19)))
        VxU16 = VxU16*M2_16
        VxU32 = IEOR(VxU32, Seed)
        VxU32 = IEOR(VxU32, IEOR(SHIFTR(VxU32, 15), SHIFTR(VxU32, 9)))
        VxU16 = VxU16*M3_16
        HashCode = IEOR(VxU32, IEOR(SHIFTL(VxU32, 16), SHIFTR(VxU32, 11)))

        RETURN

    END FUNCTION NMHASH32_0to8

    !******************************************************************************

    FUNCTION NMHASH32_9to255(P, Length, Seed, FullAvalanche) RESULT(HashCode)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,    INTENT(IN)    :: P(0:)
        tIndex,   INTENT(IN)    :: Length
        tInteger, INTENT(IN)    :: Seed
        tLogical, INTENT(IN)    :: FullAvalanche
        tInteger                :: HashCode

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: I, J, R, ShiftLen
        tInteger    :: XU32(0:3), YU32(0:3), SL
        tShort      :: XU16(0:7)
        EQUIVALENCE(XU16, XU32)
        tInteger    :: NMH_M1_32, NMH_M2_32, NMH_M3_32
        tShort      :: NMH_M1_16(0:1), NMH_M2_16(0:1), NMH_M3_16(0:1)
        EQUIVALENCE(NMH_M1_16, NMH_M1_32)
        EQUIVALENCE(NMH_M2_16, NMH_M2_32)
        EQUIVALENCE(NMH_M3_16, NMH_M3_32)

    !** FLOW

        ! initialize
        SL = Seed + ToInteger(Length)
        XU32(0) = NMH_PRIME32_1
        XU32(1) = NMH_PRIME32_2
        XU32(2) = NMH_PRIME32_3
        XU32(3) = NMH_PRIME32_4
        YU32 = SL
        NMH_M1_32 = NMH_M1
        NMH_M2_32 = NMH_M2
        NMH_M3_32 = NMH_M3

        IF (FullAvalanche) THEN
            ! 33 to 255 bytes
            R = (Length - 1) / 32
            DO I = 0, R-1
                DO J = 0, 3
                    XU32(J) = IEOR(XU32(J), PackFull(P, I*32 + J*4))
                    YU32(J) = IEOR(YU32(J), PackFull(P, I*32 + J*4 + 16))
                    XU32(J) = XU32(J) + YU32(J)
                    XU16(J*2)   = XU16(J*2)*NMH_M1_16(0)
                    XU16(J*2+1) = XU16(J*2+1)*NMH_M1_16(1)
                    XU32(J) = IEOR(XU32(J), IEOR(SHIFTL(XU32(J), 5), SHIFTR(XU32(J), 13)))
                    XU16(J*2)   = XU16(J*2)*NMH_M2_16(0)
                    XU16(J*2+1) = XU16(J*2+1)*NMH_M2_16(1)
                    XU32(J) = IEOR(XU32(J), YU32(J))
                    XU32(J) = IEOR(XU32(J), IEOR(SHIFTL(XU32(J), 11), SHIFTR(XU32(J), 9)))
                    XU16(J*2)   = XU16(J*2)*NMH_M3_16(0)
                    XU16(J*2+1) = XU16(J*2+1)*NMH_M3_16(1)
                    XU32(J) = IEOR(XU32(J), IEOR(SHIFTR(XU32(J), 10), SHIFTR(XU32(J), 20)))
                END DO
            END DO
            DO J = 0, 3
                XU32(J) = IEOR(XU32(J), PackFull(P, Length - 32 + J*4))
                YU32(J) = IEOR(YU32(J), PackFull(P, Length - 16 + J*4))
            END DO
        ELSE
            ! 9 to 32 bytes
            ShiftLen = SHIFTL(SHIFTA(Length, 4), 3)
            XU32(0) = IEOR(XU32(0), PackFull(P, 0))
            XU32(1) = IEOR(XU32(1), PackFull(P, ShiftLen))
            XU32(2) = IEOR(XU32(2), PackFull(P, Length - 8))
            XU32(3) = IEOR(XU32(3), PackFull(P, Length - 8 - ShiftLen))
            YU32(0) = IEOR(YU32(0), PackFull(P, 4))
            YU32(1) = IEOR(YU32(1), PackFull(P, ShiftLen + 4))
            YU32(2) = IEOR(YU32(2), PackFull(P, Length - 8 + 4))
            YU32(3) = IEOR(YU32(3), PackFull(P, Length - 8 - ShiftLen + 4))
        END IF

        DO J = 0, 3
            XU32(J) = XU32(J) + YU32(J)
            YU32(J) = IEOR(YU32(J), IEOR(SHIFTL(YU32(J), 17), SHIFTR(YU32(J), 6)))
            XU16(J*2)   = XU16(J*2)*NMH_M1_16(0)
            XU16(J*2+1) = XU16(J*2+1)*NMH_M1_16(1)
            XU32(J) = IEOR(XU32(J), IEOR(SHIFTL(XU32(J), 5), SHIFTR(XU32(J), 13)))
            XU16(J*2)   = XU16(J*2)*NMH_M2_16(0)
            XU16(J*2+1) = XU16(J*2+1)*NMH_M2_16(1)
            XU32(J) = IEOR(XU32(J), YU32(J))
            XU32(J) = IEOR(XU32(J), IEOR(SHIFTL(XU32(J), 11), SHIFTR(XU32(J), 9)))
            XU16(J*2)   = XU16(J*2)*NMH_M3_16(0)
            XU16(J*2+1) = XU16(J*2+1)*NMH_M3_16(1)
            XU32(J) = IEOR(XU32(J), IEOR(SHIFTR(XU32(J), 10), SHIFTR(XU32(J), 20)))
        END DO
        XU32(0) = IEOR(XU32(0), NMH_PRIME32_1)
        XU32(1) = IEOR(XU32(1), NMH_PRIME32_2)
        XU32(2) = IEOR(XU32(2), NMH_PRIME32_3)
        XU32(3) = IEOR(XU32(3), NMH_PRIME32_4)
        XU32(0) = XU32(0) + XU32(1) + XU32(2) + XU32(3)
        XU32(0) = IEOR(XU32(0), SL + SHIFTR(SL, 5))
        XU16(0) = XU16(0)*NMH_M3_16(0)
        XU16(1) = XU16(1)*NMH_M3_16(1)
        XU32(0) = IEOR(XU32(0), IEOR(SHIFTR(XU32(0), 10), SHIFTR(XU32(0), 20)))

        HashCode = XU32(0)

        RETURN

    END FUNCTION NMHASH32_9to255

    !******************************************************************************

    FUNCTION NMHASH32_Avalanche32(X) RESULT(HashCode)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, INTENT(IN)    :: X
        tInteger                :: HashCode

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tInteger, PARAMETER :: M1 = ToInteger(Z'CCE5196D')
        tInteger, PARAMETER :: M2 = ToInteger(Z'464BE229')

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
        M1_32 = M1
        M2_32 = M2
        VxU32 = X

        ! avalanche
        VxU32 = IEOR(VxU32, IEOR(SHIFTR(VxU32, 8), SHIFTR(VxU32, 21)))
        VxU16 = VxU16*M1_16
        VxU32 = IEOR(VxU32, IEOR(SHIFTL(VxU32, 12), SHIFTR(VxU32, 7)))
        VxU16 = VxU16*M2_16
        HashCode = IEOR(IEOR(VxU32, SHIFTR(VxU32, 8)), SHIFTR(VxU32, 21))

        RETURN

    END FUNCTION NMHASH32_Avalanche32

    !******************************************************************************

    SUBROUTINE NMHASH32_LongRound(AccX, AccY, P, Off)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, INTENT(INOUT) :: AccX(0:)
        tInteger, INTENT(INOUT) :: AccY(0:)
        tByte,    INTENT(IN)    :: P(0:)
        tIndex,   INTENT(IN)    :: Off

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: I, Offset, SizeOfACC
        tInteger    :: AccU32
        tShort      :: Acc_16(0:1)
        EQUIVALENCE(Acc_16, AccU32)
        tInteger    :: NMH_M1_32, NMH_M2_32, NMH_M3_32
        tShort      :: NMH_M1_16(0:1), NMH_M2_16(0:1), NMH_M3_16(0:1)
        EQUIVALENCE(NMH_M1_16, NMH_M1_32)
        EQUIVALENCE(NMH_M2_16, NMH_M2_32)
        EQUIVALENCE(NMH_M3_16, NMH_M3_32)

    !** FLOW

        ! initialize
        Offset = Off
        SizeOfACC = ACC_SIZE*4
        NMH_M1_32 = NMH_M1
        NMH_M2_32 = NMH_M2
        NMH_M3_32 = NMH_M3

        DO I = 0, ACC_SIZE-1
            AccX(I) = IEOR(AccX(I), PackFull(P, Offset))
            AccY(I) = IEOR(AccY(I), PackFull(P, Offset + SizeOfACC))
            AccX(I) = AccX(I) + AccY(I)
            AccY(I) = IEOR(AccY(I), SHIFTR(AccX(I), 1))
            AccU32 = AccX(I)
            Acc_16 = Acc_16*NMH_M1_16
            AccX(I) = AccU32
            AccX(I) = IEOR(AccX(I), IEOR(SHIFTL(AccX(I), 5), SHIFTR(AccX(I), 13)))
            AccU32 = AccX(I)
            Acc_16 = Acc_16*NMH_M2_16
            AccX(I) = AccU32
            AccX(I) = IEOR(AccX(I), AccY(I))
            AccX(I) = IEOR(AccX(I), IEOR(SHIFTL(AccX(I), 11), SHIFTR(AccX(I), 9)))
            AccU32 = AccX(I)
            Acc_16 = Acc_16*NMH_M3_16
            AccX(I) = AccU32
            AccX(I) = IEOR(AccX(I), IEOR(SHIFTR(AccX(I), 10), SHIFTR(AccX(I), 20)))
            ! update index
            Offset = Offset + 4
        END DO

        RETURN

    END SUBROUTINE NMHASH32_LongRound

    !******************************************************************************

    FUNCTION NMHASH32_Long(P, Length, Seed) RESULT(HashCode)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,    INTENT(IN)    :: P(0:)
        tIndex,   INTENT(IN)    :: Length
        tInteger, INTENT(IN)    :: Seed
        tInteger                :: HashCode

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: AccX(0:ACC_SIZE-1)
        tInteger    :: AccY(0:ACC_SIZE-1)
        tInteger    :: Sum
        tIndex      :: NbRounds, I, TwiceSizeOfACC

    !** FLOW

        ! initialize
        TwiceSizeOfACC = 2*(ACC_SIZE*4)
        NbRounds = (Length-1)/(TwiceSizeOfACC)
        Sum = 0
        AccX = NMH_ACC_INIT
        AccY = Seed

        DO I = 0, NbRounds-1
            CALL NMHASH32_LongRound(AccX, AccY, P, I*TwiceSizeOfACC)
        END DO
        CALL NMHASH32_LongRound(AccX, AccY, P, Length - TwiceSizeOfACC)

        ! merge acc
        DO I = 0, ACC_SIZE-1
            AccX(I) = IEOR(AccX(I), NMH_ACC_INIT(I))
            Sum = Sum + AccX(I)
        END DO

        ! get hash
        Sum = Sum + ToInteger(SHIFTA(Length, 32))
        HashCode = IEOR(Sum, ToInteger(Length))

        RETURN

    END FUNCTION NMHASH32_Long

    !******************************************************************************

#undef NMHASH32_9to32
#undef NMHASH32_33to255

END FUNCTION NM_V2_Hash32

!******************************************************************************

FUNCTION NMx_V2_Hash32(Input, Seed, PackFull) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using version 2 of the NMxHASH hash algorithm
    !  by James Z. M. Gao.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)    :: Input(0:)    !! input bytes
    tInteger, INTENT(IN)    :: Seed         !! seed
    PROCEDURE(Pack_I32)     :: PackFull     !! procedure to convert a byte array to 32-bit integer
    tInteger                :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Length

!** FLOW

    ! initialize
    Length = SIZE(Input)

    IF (Length <= 8) THEN
        IF (Length > 4) THEN
            HashCode = NMxHASH32_5to8(Input, Length, Seed)
        ELSE
            BLOCK
                tInteger    :: X, NewSeed
                SELECT CASE (Length)
                CASE(0)
                    NewSeed = Seed + NMH_PRIME32_2
                    X = 0
                CASE(1:3)
                    NewSeed = Seed + NMH_PRIME32_2 + SHIFTL(Length, 24) + SHIFTL(Length, 1)
                    X = PackPartial(Input, 0, Length, PackFull)
                CASE(4)
                    NewSeed = Seed + NMH_PRIME32_1
                    X = PackFull(Input, 0)
                END SELECT
                HashCode = NMxHASH32_0to4(X, NewSeed)
            END BLOCK
        END IF
        RETURN
    END IF
    IF (Length < 256) THEN
        HashCode = NMxHASH32_9to255(Input, Length, Seed)
    ELSE
        HashCode = NMxHASH32_Avalanche32(NMHASH32_Long(Input, Length, Seed))
    END IF

    RETURN

CONTAINS

    FUNCTION NMxHASH32_0to4(X, Seed) RESULT(HashCode)

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, INTENT(IN)    :: X
        tInteger, INTENT(IN)    :: Seed
        tInteger                :: HashCode

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tInteger, PARAMETER :: M1 = ToInteger(Z'BDAB1EA9')
        tInteger, PARAMETER :: M2 = ToInteger(Z'A7896A1B')
        tInteger, PARAMETER :: M3 = ToInteger(Z'83796A2D')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        ! initialize
        HashCode = X

        ! mix
        HashCode = IEOR(HashCode, Seed)
        HashCode = HashCode*M1
        HashCode = HashCode + RotateLeft(Seed, 31)
        HashCode = IEOR(HashCode, SHIFTR(HashCode, 18))
        HashCode = HashCode*M2
        HashCode = IEOR(HashCode, SHIFTR(HashCode, 12))
        HashCode = HashCode*M3
        HashCode = IEOR(HashCode, SHIFTR(HashCode, 16))

        RETURN

    END FUNCTION NMxHASH32_0to4

    !******************************************************************************

    FUNCTION NMxHASH32_5to8(P, Length, Seed) RESULT(HashCode)

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,    INTENT(IN)    :: P(0:)
        tIndex,   INTENT(IN)    :: Length
        tInteger, INTENT(IN)    :: Seed
        tInteger                :: HashCode

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tInteger, PARAMETER :: M1 = ToInteger(Z'11049A7D')
        tInteger, PARAMETER :: M2 = ToInteger(Z'BCCCDC7B')
        tInteger, PARAMETER :: M3 = ToInteger(Z'065E9DAD')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: X, Y

    !** FLOW

        ! initialize
        X = IEOR(PackFull(P, 0), NMH_PRIME32_3)
        Y = IEOR(PackFull(P, Length - 4), Seed)

        ! mix
        X = X + Y
        X = IEOR(X, SHIFTR(X, Length))
        X = X*M1
        X = IEOR(X, SHIFTR(X, 23))
        X = X*M2
        X = IEOR(X, RotateLeft(Y, 3))
        X = IEOR(X, SHIFTR(X, 12))
        X = X*M3
        X = IEOR(X, SHIFTR(X, 12))
        HashCode = X

        RETURN

    END FUNCTION NMxHASH32_5to8

    !******************************************************************************

    FUNCTION NMxHASH32_9to255(P, Length, Seed) RESULT(HashCode)

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,    INTENT(IN)    :: P(0:)
        tIndex,   INTENT(IN)    :: Length
        tInteger, INTENT(IN)    :: Seed
        tInteger                :: HashCode

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tInteger, PARAMETER :: M1 = ToInteger(Z'11049A7D')
        tInteger, PARAMETER :: M2 = ToInteger(Z'BCCCDC7B')
        tInteger, PARAMETER :: M3 = ToInteger(Z'065E9DAD')
        tInteger, PARAMETER :: M4 = ToInteger(Z'A52FB2CD')
        tInteger, PARAMETER :: M5 = ToInteger(Z'551E4D49')
        tInteger, PARAMETER :: M6 = ToInteger(Z'141CC535')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: X, Y, A, B
        tIndex      :: I, R, Offset

    !** FLOW

        X = NMH_PRIME32_3
        Y = Seed
        A = NMH_PRIME32_4
        B = Seed
        R = (Length - 1) / 16
        Offset = 0

        DO I = 0, R-1
            X = IEOR(X, PackFull(P, Offset + 0))
            Y = IEOR(Y, PackFull(P, Offset + 4))
            X = IEOR(X, Y)
            X = X*M1
            X = IEOR(X, SHIFTR(X, 23))
            X = X*M2
            Y = RotateLeft(Y, 4)
            X = IEOR(X, Y)
            X = IEOR(X, SHIFTR(X, 12))
            X = X*M3
            X = IEOR(X, SHIFTR(X, 12))

            A = IEOR(A, PackFull(P, Offset + 8))
            B = IEOR(B, PackFull(P, Offset + 12))
            A = IEOR(A, B)
            A = A*M1
            A = IEOR(A, SHIFTR(A, 23))
            A = A*M2
            B = RotateLeft(B, 3)
            A = IEOR(A, B)
            A = IEOR(A, SHIFTR(A, 12))
            A = A*M3
            A = IEOR(A, SHIFTR(A, 12))
            Offset = Offset + 16
        END DO

        IF (IAND(ToInteger(Length)-1, 8) /= 0) THEN
            IF (IAND(ToInteger(Length)-1, 4) /= 0) THEN
                A = IEOR(A, PackFull(P, Offset))
                B = IEOR(B, PackFull(P, Offset + 4))
                A = IEOR(A, B)
                A = A*M1
                A = IEOR(A, SHIFTR(A, 23))
                A = A*M2
                A = IEOR(A, RotateLeft(B, 4))
                A = IEOR(A, SHIFTR(A, 12))
                A = A*M3
            ELSE
                A = IEOR(A, PackFull(P, Offset) + B)
                A = IEOR(A, SHIFTR(A, 16))
                A = A*M4
                A = IEOR(A, SHIFTR(A, 15))
                A = A*M5
            END IF

            X = IEOR(X, PackFull(P, Length - 8))
            Y = IEOR(Y, PackFull(P, Length - 4))
            X = IEOR(X, Y)
            X = X*M1
            X = IEOR(X, SHIFTR(X, 23))
            X = X*M2
            X = IEOR(X, RotateLeft(Y, 3))
            X = IEOR(X, SHIFTR(X, 12))
            X = X*M3
        ELSE
            IF (IAND(ToInteger(Length)-1, 4) /= 0) THEN
                A = IEOR(A, PackFull(P, Offset) + B)
                A = IEOR(A, SHIFTR(A, 16))
                A = A*M4
                A = IEOR(A, SHIFTR(A, 15))
                A = A*M5
            END IF
            X = IEOR(X, PackFull(P, Length - 4) + Y)
            X = IEOR(X, SHIFTR(X, 16))
            X = X*M4
            X = IEOR(X, SHIFTR(X, 15))
            X = X*M5
        END IF

        X = IEOR(X, ToInteger(Length))
        ! rotate one lane to pass diff test
        X = IEOR(X, RotateLeft(A, 27))
        X = IEOR(X, SHIFTR(X, 14))
        X = X*M6

        HashCode = X

        RETURN

    END FUNCTION NMxHASH32_9to255

    !******************************************************************************

    FUNCTION NMxHASH32_Avalanche32(X) RESULT(HashCode)

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

    END FUNCTION NMxHASH32_Avalanche32

    !******************************************************************************

    SUBROUTINE NMHASH32_LongRound(AccX, AccY, P, Off)

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, INTENT(INOUT) :: AccX(0:)
        tInteger, INTENT(INOUT) :: AccY(0:)
        tByte,    INTENT(IN)    :: P(0:)
        tIndex,   INTENT(IN)    :: Off

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: I, Offset, SizeOfACC
        tInteger    :: AccU32
        tShort      :: Acc_16(0:1)
        EQUIVALENCE(Acc_16, AccU32)
        tInteger    :: NMH_M1_32, NMH_M2_32, NMH_M3_32
        tShort      :: NMH_M1_16(0:1), NMH_M2_16(0:1), NMH_M3_16(0:1)
        EQUIVALENCE(NMH_M1_16, NMH_M1_32)
        EQUIVALENCE(NMH_M2_16, NMH_M2_32)
        EQUIVALENCE(NMH_M3_16, NMH_M3_32)

    !** FLOW

        ! initialize
        Offset = Off
        SizeOfACC = ACC_SIZE*4
        NMH_M1_32 = NMH_M1
        NMH_M2_32 = NMH_M2
        NMH_M3_32 = NMH_M3

        DO I = 0, ACC_SIZE-1
            AccX(I) = IEOR(AccX(I), PackFull(P, Offset))
            AccY(I) = IEOR(AccY(I), PackFull(P, Offset + SizeOfACC))
            AccX(I) = AccX(I) + AccY(I)
            AccY(I) = IEOR(AccY(I), SHIFTR(AccX(I), 1))
            AccU32 = AccX(I)
            Acc_16 = Acc_16*NMH_M1_16
            AccX(I) = AccU32
            AccX(I) = IEOR(AccX(I), IEOR(SHIFTL(AccX(I), 5), SHIFTR(AccX(I), 13)))
            AccU32 = AccX(I)
            Acc_16 = Acc_16*NMH_M2_16
            AccX(I) = AccU32
            AccX(I) = IEOR(AccX(I), AccY(I))
            AccX(I) = IEOR(AccX(I), IEOR(SHIFTL(AccX(I), 11), SHIFTR(AccX(I), 9)))
            AccU32 = AccX(I)
            Acc_16 = Acc_16*NMH_M3_16
            AccX(I) = AccU32
            AccX(I) = IEOR(AccX(I), IEOR(SHIFTR(AccX(I), 10), SHIFTR(AccX(I), 20)))
            ! update index
            Offset = Offset + 4
        END DO

        RETURN

    END SUBROUTINE NMHASH32_LongRound

    !******************************************************************************

    FUNCTION NMHASH32_Long(P, Length, Seed) RESULT(HashCode)

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,    INTENT(IN)    :: P(0:)
        tIndex,   INTENT(IN)    :: Length
        tInteger, INTENT(IN)    :: Seed
        tInteger                :: HashCode

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: AccX(0:ACC_SIZE-1)
        tInteger    :: AccY(0:ACC_SIZE-1)
        tInteger    :: Sum
        tIndex      :: NbRounds, I, TwiceSizeOfACC

    !** FLOW

        ! initialize
        TwiceSizeOfACC = 2*(ACC_SIZE*4)
        NbRounds = (Length-1)/(TwiceSizeOfACC)
        Sum = 0
        AccX = NMH_ACC_INIT
        AccY = Seed

        DO I = 0, NbRounds-1
            CALL NMHASH32_LongRound(AccX, AccY, P, I*TwiceSizeOfACC)
        END DO
        CALL NMHASH32_LongRound(AccX, AccY, P, Length - TwiceSizeOfACC)

        ! merge acc
        DO I = 0, ACC_SIZE-1
            AccX(I) = IEOR(AccX(I), NMH_ACC_INIT(I))
            Sum = Sum + AccX(I)
        END DO

        ! get hash
        Sum = Sum + ToInteger(SHIFTA(Length, 32))
        HashCode = IEOR(Sum, ToInteger(Length))

        RETURN

    END FUNCTION NMHASH32_Long

    !******************************************************************************

END FUNCTION NMx_V2_Hash32

!******************************************************************************

END SUBMODULE SubBase_NMHashV2_Exp

!******************************************************************************

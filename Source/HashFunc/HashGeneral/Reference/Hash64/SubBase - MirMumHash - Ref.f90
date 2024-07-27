
SUBMODULE (ModBase_ReferenceHash64) SubBase_MirMumHash_Ref

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains a reference implementation for the MirHash and MumHash hash algorithms
!   by Vladimir Makarov. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    USE Class_ByteConverter
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define MaskI8(X)       IAND(ToLong(X), ToLong(Z'00000000000000FF'))
#define MaskI32(X)      IAND(ToLong(X), ToLong(Z'00000000FFFFFFFF'))

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

MODULE FUNCTION MirHash_I64(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the MirHash hash algorithm by Vladimir Makarov.
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
    HashCode = Mir_Hash64(InpPtr, Seed, ByteConv)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION MirHash_I64

!******************************************************************************

MODULE FUNCTION MumHash_I64(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the MumHash hash algorithm by Vladimir Makarov.
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
    HashCode = Mum_Hash64(InpPtr, Seed, ByteConv)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)
    CALL ByteConv%Reset()

    RETURN

END FUNCTION MumHash_I64

!******************************************************************************

FUNCTION Mir_Hash64(Input, Seed, BC) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the MirHash hash algorithm by Vladimir Makarov.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN) :: Input(0:)    !! input bytes
    tLong,               INTENT(IN) :: Seed         !! seed
    TYPE(ByteConverter), INTENT(IN) :: BC           !! byte converter
    tLong                           :: HashCode     !! hash code

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong,    PARAMETER    :: P1 = ToLong(Z'65862B62BDF5EF4D')
    tLong,    PARAMETER    :: P2 = ToLong(Z'288EEA216831E6A7')
    tLogical, PARAMETER    :: Relax = FalseVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: R
    tIndex      :: Length, Remaining, Offset

!** FLOW

    ! initialize
    Length = SIZE(Input)
    Remaining = Length
    Offset = 0
    R = Seed + ToLong(Length)

    DO WHILE (Remaining >= 16)
        R = IEOR(R, MirMum(BC%Pack_I64(Input, Offset), P1))
        R = IEOR(R, MirMum(BC%Pack_I64(Input, Offset + 8), P2))
        R = IEOR(R, MirMum(R, P1))
        ! update indices
        Offset = Offset + 16
        Remaining = Remaining - 16
    END DO

    DO WHILE (Remaining >= 8)
        R = IEOR(R, MirMum(BC%Pack_I64(Input, Offset), P1))
        ! update indices
        Offset = Offset + 8
        Remaining = Remaining - 8
    END DO

    IF (Remaining /= 0) THEN
        ! R = IEOR(R, MirMum(BC%Pack_I64_Partial(Input, Offset, Remaining), P2))
        R = IEOR(R, MirMum(MirGetKeyPart(Input, Offset, Remaining, Relax), P2))
    END IF

    CALL MirRound(R, R, HashCode)

    RETURN

CONTAINS

    FUNCTION MirMum(LHS, RHS) RESULT(ResVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To multiply and sum.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(IN)   :: LHS
        tLong, INTENT(IN)   :: RHS
        tLong               :: ResVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong   :: LHS_Lo, LHS_Hi, RHS_Lo, RHS_Hi
        tLong   :: Rm

    !** FLOW

        LHS_Lo = MaskI32(LHS)
        LHS_Hi = SHIFTR(LHS, 32)
        RHS_Lo = MaskI32(RHS)
        RHS_Hi = SHIFTR(RHS, 32)
        Rm = LHS_Lo*RHS_Hi + LHS_Hi*RHS_Lo
        ResVal = LHS_Hi*RHS_Hi + SHIFTR(Rm, 32) + LHS_Lo*RHS_Lo + SHIFTL(Rm, 32)

        RETURN

    END FUNCTION MirMum

    !**************************************************************************

    FUNCTION MirGetKeyPart(Buf, Off, Length, Relax) RESULT(OutVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To get parts of the input.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,    INTENT(IN)  :: Buf(0:)  ! buffer
        tIndex,   INTENT(IN)  :: Off      ! offset
        tIndex,   INTENT(IN)  :: Length   ! the number of bytes to pack (between 1 to 7)
        tLogical, INTENT(IN)  :: Relax
        tLong                 :: OutVal   ! result

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong   :: Tail
        tIndex  :: Start, I

    !** FLOW

        Start = 0
        Tail = 0_kLong

        IF (Relax) THEN
            IF (Length == 8) THEN
                OutVal = BC%Pack_I64(Buf, Off)
                RETURN
            END IF
            IF (Length >= 4) THEN
              Tail = SHIFTL(MaskI32(BC%Pack_I32(Buf, Off)), 32)
              Start = 4
            END IF
        END IF
        DO I = Start, Length-1
            Tail = IOR(SHIFTR(Tail, 8), SHIFTL(MaskI8(Buf(Off+I)), 56))
        END DO
        OutVal = Tail

        RETURN

    END FUNCTION MirGetKeyPart

    !**************************************************************************

    SUBROUTINE MirRound(State, V, H)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To multiply and sum.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(INOUT)    :: State
        tLong, INTENT(IN)       :: V
        tLong, INTENT(OUT)      :: H

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        State = IEOR(State, MirMum(V, P1))
        H = IEOR(State, MirMum(State, P2))

        RETURN

    END SUBROUTINE MirRound

    !**************************************************************************

END FUNCTION Mir_Hash64

!******************************************************************************

FUNCTION Mum_Hash64(Input, Seed, BC) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the MumHash hash algorithm by Vladimir Makarov.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,               INTENT(IN) :: Input(0:)    !! input bytes
    tLong,               INTENT(IN) :: Seed         !! seed
    TYPE(ByteConverter), INTENT(IN) :: BC           !! byte converter
    tLong                           :: HashCode     !! hash code

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: Mum_Block_Start_Prime = ToLong(Z'C42B5E2E6480B23B')
    tLong, PARAMETER    :: Mum_Unroll_Prime      = ToLong(Z'7B51EC3D22F7096F')
    tLong, PARAMETER    :: Mum_Tail_Prime        = ToLong(Z'AF47D47C99B1461B')
    tLong, PARAMETER    :: Mum_Finish_Prime1     = ToLong(Z'A9A7AE7CEFF79F3F')
    tLong, PARAMETER    :: Mum_Finish_Prime2     = ToLong(Z'AF47D47C99B1461B')
    tLong, PARAMETER    :: Mum_Primes(0:15) =                     &
       [ToLong(Z'9EBDCAE10D981691'), ToLong(Z'32B9B9B97A27AC7D'), &
        ToLong(Z'29B5584D83D35BBD'), ToLong(Z'4B04E0E61401255F'), &
        ToLong(Z'25E8F7B1F1C9D027'), ToLong(Z'80D4C8C000F3E881'), &
        ToLong(Z'BD1255431904B9DD'), ToLong(Z'8A3BD4485EEE6D81'), &
        ToLong(Z'3BC721B2AAD05197'), ToLong(Z'71B1A19B907D6E33'), &
        ToLong(Z'525E6C1084A8534B'), ToLong(Z'9E4C2CD340C1299F'), &
        ToLong(Z'DE3ADD92E94CAA37'), ToLong(Z'7E14EADB1F65311D'), &
        ToLong(Z'3F5AA40F89812853'), ToLong(Z'33B15A3B587D15C9')]
    tIndex, PARAMETER   :: MUM_UNROLL_FACTOR_POWER = 2
    tIndex, PARAMETER   :: MUM_UNROLL_FACTOR = SHIFTL(1, MUM_UNROLL_FACTOR_POWER)
    tIndex, PARAMETER   :: INNER_BLOCK_LEN = MUM_UNROLL_FACTOR*8
    tIndex, PARAMETER   :: MUM_BLOCK_LEN = 1024

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Length, Remaining, Offset, BlockLen, RemLen, I

!** FLOW

    ! initialize
    Length = SIZE(Input)
    Remaining = Length
    Offset = 0
    HashCode = Seed + ToLong(Length)

    ! _mum_hash_default
    DO WHILE (Remaining /= 0)
        ! set block length
        IF (Remaining < 1024) THEN
            BlockLen = Remaining
        ELSE
            BlockLen = 1024
        END IF
        RemLen = BlockLen
        ! _mum_hash_aligned
        HashCode = Mum(HashCode, Mum_Block_Start_Prime)
        ! main block
        DO WHILE (RemLen > INNER_BLOCK_LEN)
            DO I = 0, MUM_UNROLL_FACTOR-1
                HashCode = IEOR(HashCode, Mum(BC%Pack_I64(Input, Offset), Mum_Primes(I)))
                ! update index
                Offset = Offset + 8
            END DO
            ! update index
            RemLen = RemLen - INNER_BLOCK_LEN
            ! We will use the same prime numbers on the next iterations --
            !   randomize the state.
            HashCode = Mum(HashCode, Mum_Unroll_Prime)
        END DO
        ! sub block
        I = 0
        DO WHILE (RemLen >= 8)
            HashCode = IEOR(HashCode, Mum(BC%Pack_I64(Input, Offset), Mum_Primes(I)))
            ! update indices
            I = I + 1
            Offset = Offset + 8
            RemLen = RemLen - 8
        END DO
        ! remaining bytes
        IF (RemLen > 0) THEN
            HashCode = IEOR(HashCode, Mum(BC%Pack_I64_Partial(Input, Offset, RemLen), &
                                          Mum_Tail_Prime))
        END IF
        ! update index
        Remaining = Remaining - BlockLen
    END DO

    ! _mum_final
    HashCode = IEOR(HashCode, Mum(HashCode, Mum_Finish_Prime1))
    HashCode = IEOR(HashCode, Mum(HashCode, Mum_Finish_Prime2))

    RETURN

CONTAINS

    FUNCTION Mum(LHS, RHS) RESULT(ResVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To multiply and sum.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(IN)   :: LHS
        tLong, INTENT(IN)   :: RHS
        tLong               :: ResVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong   :: LHS_Lo, LHS_Hi, RHS_Lo, RHS_Hi
        tLong   :: R_Hi, R_Lo, R_Mid_0, R_Mid_1
        tLong   :: T, Upp, Low

    !** FLOW

        LHS_Lo = MaskI32(LHS)
        LHS_Hi = SHIFTR(LHS, 32)
        RHS_Lo = MaskI32(RHS)
        RHS_Hi = SHIFTR(RHS, 32)
        R_Hi    = LHS_Hi*RHS_Hi
        R_Mid_0 = LHS_Hi*RHS_Lo
        R_Mid_1 = RHS_Hi*LHS_Lo
        R_Lo    = LHS_Lo*RHS_Lo
        T = R_Lo + SHIFTL(R_Mid_0, 32)
        Low = T + SHIFTL(R_Mid_1, 32)
        Upp = R_Hi + SHIFTR(R_Mid_0, 32) + SHIFTR(R_Mid_1, 32)
        ResVal = Upp + Low

        RETURN

    END FUNCTION Mum

    !**************************************************************************

END FUNCTION Mum_Hash64

!******************************************************************************

END SUBMODULE SubBase_MirMumHash_Ref

!******************************************************************************

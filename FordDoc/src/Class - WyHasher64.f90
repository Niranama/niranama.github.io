
MODULE Class_WyHasher64

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *WyHasher64* type and its related routines.
!   The *WyHasher64* type is a hasher type that extends directly from the
!   <a href="../module/class_hasher64.html#type-hasher64">Hasher64</a> type.
!   It provides all deferred procedures required by a *Hasher64* class and
!   outputs the hash value as a 64-bit integer. <br>
!   The *WyHasher64* type employs the *Wy* hash algorithm for 64-bit integer
!   output by Wang Yi [1].  As a hasher, it can be used to compute the
!   hash value incrementally.  It also provides a method to compute the hash
!   value directly (i.e. non-incrementally).  The following code snippet shows
!   a typical usage of the hasher.
!   <Pre><Code style="color:MidnightBlue;">
!   ! first, initialize the hasher (once)
!   CALL Hasher%Initialize(Seed)
!   ! then, put data into the hasher (a number of times)
!   CALL Hasher%Update(Input, InpSize)
!               ...
!               ...
!               ...
!   ! finally, get the hash value from the hasher (once)
!   HashCode = Hasher%Finalize()
!   </Code></Pre>
!   However, if the *Update* method is to be called only one time, then the
!   *HashDirect* method should be employed as follows.
!   <Pre><Code style="color:MidnightBlue;">
!   ! directly compute the hash value
!   HashCode = Hasher%HashDirect(Input, InpSize, Seed)
!   </Code></Pre>
!^ **REFERENCES**: <br>
!   [1] <a href="https://github.com/wangyi-fudan/wyhash">WYHASH and WYRAND - The FASTEST
!       QUALITY hash function, random number generators (PRNG) and hash map. </a> <br>

!** USE STATEMENTS:
    USE ModBase_Common
    USE Class_Hasher64
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: WyHasher64

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define MaskI32(X)              IAND(ToLong(X), ToLong(Z'00000000FFFFFFFF'))
#define MaskI8(X)               IAND(ToLong(X), ToLong(Z'00000000000000FF'))
#define Pack_U32(Buf,Index)     MaskI32(PackInteger(Buf, Index))
#define WyMix(A, B)             UnsignedLongMultiplyorFold(A, B)
#define WyR3(Buf,Index,K)                                     \
    IOR(IOR(SHIFTL(MaskI8(Buf(Index)), 16),                 \
            SHIFTL(MaskI8(Buf(Index + SHIFTR(K, 1))), 8)),  \
                    MaskI8(Buf(Index + K - 1)))
#define WyR4(Buf,Index)         Pack_U32(Buf, Index)

!** MODULE PARAMETERS:
    tLong,  PARAMETER   :: WyP0 = ToLong(Z'A0761D6478BD642F')
    tLong,  PARAMETER   :: WyP1 = ToLong(Z'E7037ED1A0B428DB')
    tLong,  PARAMETER   :: WyP2 = ToLong(Z'8EBC6AF09C88C6E3')
    tLong,  PARAMETER   :: WyP3 = ToLong(Z'589965CC75374CC3')
    tIndex, PARAMETER   :: BlockLen = 48_kIndex

!** DERIVED TYPE DEFINITIONS
    !> *WyHasher64* is a hasher type that outputs the hash value as a 64-bit integer.
    !  It employs the final version 3 of the *Wy* hash algorithm by Wang Yi.
    TYPE, EXTENDS(Hasher64) :: WyHasher64
        PRIVATE
        !% state
        tLong       :: State(3)             = 0_kLong
        !% buffer array used to store input data
        tByte       :: BufArr(0:BlockLen-1) = 0_kByte
        !% flag indicating whether to remove sign from the final hash value
        tLogical    :: RemoveSign   = FalseVal
    CONTAINS
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the hasher. <br>
        !  **Usage**: <br>
        !   --->    Name = Hasher%GetName()
        PROCEDURE   :: GetName          => Wy_GetName
        !> *GetBlockLength* is a procedure to return the block length of this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: GetBlockLength   => Wy_BlockLength
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: SetBufPtr        => Wy_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: ProcessBlock     => Wy_ProcessBlock
        !> **Type-Bound Subroutine**: Initialize <br>
        !  **Purpose**:  To initialize the hasher. <br>
        !  **Usage**: <br>
        !   --->    CALL Hasher%Initialize(Seed)            ! hash value with sign <br>
        !   --->    CALL Hasher%Initialize(Seed, .TRUE.)    ! remove sign from hash value <br>
        PROCEDURE   :: Initialize       => Wy_Initialize
        !> **Type-Bound Function**: Finalize <br>
        !  **Purpose**:  To finalize the current hash computation and return the hash value
        !                in a 64-bit integer.  The object is reset. <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%Finalize()
        PROCEDURE   :: Finalize         => Wy_Finalize
        !> **Type-Bound Function**: HashDirect <br>
        !  **Purpose**:  To compute the hash value directly (non-incrementally). <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize) <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize, Seed) <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize, RemoveSign=.TRUE.) <br>
        !  **Note**: The specified input can be any type and any rank where its size is
        !            the number of bytes of storage used by the input.
        PROCEDURE   :: HashDirect       => Wy_HashDirect
    END TYPE WyHasher64

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

FUNCTION Wy_GetName(HS) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the display name for this hasher.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(WyHasher64), INTENT(IN)   :: HS   !! a hasher (HS) object
    tCharAlloc                      :: Name !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Name = 'Wy_Hahser64'
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION Wy_GetName

!******************************************************************************

FUNCTION Wy_BlockLength(HS) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the *block length* for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(WyHasher64), INTENT(IN)   :: HS       !! a hasher (HS) object
    tIndex                          :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Length = BlockLen
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION Wy_BlockLength

!******************************************************************************

SUBROUTINE Wy_SetBufPtr(HS, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(WyHasher64), TARGET, INTENT(INOUT)    :: HS           !! a hasher (HS) object
    tByte,            POINTER, INTENT(INOUT)    :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => HS%BufArr

    RETURN

END SUBROUTINE Wy_SetBufPtr

!******************************************************************************

SUBROUTINE Wy_ProcessBlock(HS, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(WyHasher64), INTENT(INOUT)    :: HS           !! a hasher (HS) object
    tByte,             INTENT(IN)       :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: LHS1, RHS1

!** FLOW

    ASSOCIATE (Seed => HS%State(1), Seed1 => HS%State(2), Seed2 => HS%State(3))
        LHS1 = IEOR(PackLong(BytesIn, 0_kIndex), WyP1)
        RHS1 = IEOR(PackLong(BytesIn, 8_kIndex), Seed)
        Seed = WyMix(LHS1, RHS1)
        LHS1 = IEOR(PackLong(BytesIn, 16_kIndex), WyP2)
        RHS1 = IEOR(PackLong(BytesIn, 24_kIndex), Seed1)
        Seed1 = WyMix(LHS1, RHS1)
        LHS1 = IEOR(PackLong(BytesIn, 32_kIndex), WyP3)
        RHS1 = IEOR(PackLong(BytesIn, 40_kIndex), Seed2)
        Seed2 = WyMix(LHS1, RHS1)
    END ASSOCIATE

    RETURN

END SUBROUTINE Wy_ProcessBlock

!******************************************************************************

SUBROUTINE Wy_Initialize(HS, Seed, RemoveSign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the hasher without seed or with one seed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(WyHasher64), TARGET, INTENT(INOUT)    :: HS   !! a hasher (HS) object
    tLong,                     INTENT(IN)       :: Seed !! seed
    tLogical,        OPTIONAL, INTENT(IN)       :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ASSOCIATE (Seed0 => HS%State(1), Seed1 => HS%State(2), Seed2 => HS%State(3))
        Seed0 = IEOR(Seed, WyP0)
        Seed1 = Seed0
        Seed2 = Seed0
    END ASSOCIATE
    SET_OPTION(HS%RemoveSign, FalseVal, RemoveSign)
    CALL HS%Reset()

    RETURN

END SUBROUTINE Wy_Initialize

!******************************************************************************

FUNCTION Wy_Finalize(HS) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  in a 64-bit integer.  The object is reset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(WyHasher64), INTENT(INOUT)    :: HS       !! a hasher (HS) object
    tLong                               :: HashCode !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: A, B
    tLong       :: LHS1, RHS1, LHS2, RHS2
    tIndex      :: Length, Remaining, Offset, ShiftLen

!** FLOW

    Remaining = HS%GetBufLen()
    IF (HS%GetBlockCount() > 0_kIndex) THEN
        Length = HS%GetBlockCount()*HS%GetBlockLength() + Remaining
        HS%State(1) = IEOR(HS%State(1), IEOR(HS%State(2), HS%State(3)))
    ELSE
        Length = Remaining
        IF (Length <= 16_kIndex) THEN
            IF (Length >= 4_kIndex) THEN
                ShiftLen = SHIFTL(SHIFTR(Length, 3), 2)
                Offset = Length - 4
                A = IOR(SHIFTL(WyR4(HS%BufArr, 0_kIndex), 32),  WyR4(HS%BufArr, ShiftLen))
                B = IOR(SHIFTL(WyR4(HS%BufArr, Offset), 32), WyR4(HS%BufArr, Offset - ShiftLen))
            ELSEIF (Length > 0_kIndex) THEN
                A = WyR3(HS%BufArr, 0_kIndex, Length)
                B = 0_kLong
            ELSE
                A = 0_kLong
                B = 0_kLong
            END IF
            Remaining = 0_kIndex
        END IF
    END IF
    ASSOCIATE (Seed => HS%State(1), Input => HS%BufArr)
        IF (Remaining > 0_kIndex) THEN
            Offset = 0_kIndex
            DO WHILE (Remaining > 16_kIndex)
                LHS1 = IEOR(PackLong(Input, Offset), WyP1)
                RHS1 = IEOR(PackLong(Input, Offset + 8_kIndex), Seed)
                Seed = WyMix(LHS1, RHS1)
                Remaining = Remaining - 16_kIndex
                Offset = Offset + 16_kIndex
            END DO
            A = PackLong(Input, Offset + Remaining - 16_kIndex)
            B = PackLong(Input, Offset + Remaining - 8_kIndex)
        END IF
        LHS1 = IEOR(A, WyP1)
        RHS1 = IEOR(B, Seed)
        LHS2 = IEOR(WyP1, ToLong(Length))
        RHS2 = WyMix(LHS1, RHS1)
        HashCode = WyMix(LHS2, RHS2)
    END ASSOCIATE

    ! remove sign if needed
    IF (HS%RemoveSign) HashCode = IAND(HashCode, ToLong(Z'7FFFFFFFFFFFFFFF'))

    ! reset the hasher
    HS%State  = 0_kLong
    HS%BufArr = 0_kByte
    HS%RemoveSign = FalseVal
    CALL HS%Reset()

    RETURN

END FUNCTION Wy_Finalize

!******************************************************************************

FUNCTION Wy_HashDirect(HS, Input, InpSize, Seed, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value directly (non-incremental) without seed or with one seed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(WyHasher64),      INTENT(INOUT)   :: HS           !! a hasher (HS) object
    TYPE(*), DIMENSION(..), INTENT(IN)      :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize      !! size of the input (in bytes)
    tLong,    OPTIONAL,     INTENT(IN)      :: Seed         !! seed
    tLogical, OPTIONAL,     INTENT(IN)      :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tLong                                   :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Seed0

!** FLOW

    SET_OPTION(Seed0, 0_kLong, Seed)
    
    ! The following code illustrates simple use of the hasher.
    CALL HS%Initialize(Seed0, RemoveSign)
    CALL HS%Update(Input, InpSize)
    HashCode = HS%Finalize()

    RETURN

END FUNCTION Wy_HashDirect

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

FUNCTION UnsignedLongMultiplyorFold(LHS, RHS) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply or fold.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: LHS
    tLong, INTENT(IN)   :: RHS
    tLong               :: Res

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: MaxU32 = ToLong(Z'00000000FFFFFFFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong   :: LHS_Lo, LHS_Hi, RHS_Lo, RHS_Hi
    tLong   :: Lo_Lo, Hi_Lo
    tLong   :: Cross

!** FLOW

    ! the Grade School method of multiplication.
    LHS_Lo = IAND(LHS, MaxU32)
    LHS_Hi = SHIFTR(LHS, 32)
    RHS_Lo = IAND(RHS, MaxU32)
    RHS_Hi = SHIFTR(RHS, 32)
    Lo_Lo = LHS_Lo*RHS_Lo
    Hi_Lo = LHS_Hi*RHS_Lo

    ! Add the products together. This will never overfLow.
    Cross = SHIFTR(Lo_Lo, 32) + IAND(Hi_Lo, MaxU32) + LHS_Lo*RHS_Hi
    Res   = IEOR(IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, MaxU32)), &
                 SHIFTR(Hi_Lo, 32) + SHIFTR(Cross, 32) + LHS_Hi*RHS_Hi)

    RETURN

END FUNCTION UnsignedLongMultiplyorFold

!******************************************************************************

END MODULE Class_WyHasher64

!******************************************************************************

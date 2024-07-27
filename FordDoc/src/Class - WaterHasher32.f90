
MODULE Class_WaterHasher32

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *WaterHasher32* type and its related routines.
!   The *WaterHasher32* type is a hasher type that extends directly from the
!   <a href="../module/class_hasher32.html#type-hasher32">Hasher32</a> type.
!   It provides all deferred procedures required by a *Hasher32* class and
!   outputs the hash value as a 32-bit integer. <br>
!   The *WaterHasher32* type employs the *Water* hash algorithm for 32-bit integer
!   output by Tommy Ettinger [1].  As a hasher, it can be used to compute the
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
!   [1] <a href="https://github.com/tommyettinger/waterhash">WaterHash: A variant of WyHash. </a> <br>

!** USE STATEMENTS:
    USE ModBase_Common
    USE Class_Hasher32
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: WaterHasher32

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define MaskShort(X)        IAND(ToShort(X), ToShort(Z'00FF'))
#define MaskInteger(X)      IAND(ToInteger(X), Z'000000FF')
#define Pack_I16(Buf,Off)   (MaskShort(Buf(Off)) + SHIFTL(MaskShort(Buf(Off+1)), 8))
#define Pack_I32(Buf,Off)   (MaskInteger(Buf(Off)) + SHIFTL(MaskInteger(Buf(Off+1)),  8) + \
                            SHIFTL(MaskInteger(Buf(Off+2)), 16) + SHIFTL(MaskInteger(Buf(Off+3)), 24))
#define MaskI32(X)          IAND(ToLong(X), Z'00000000FFFFFFFF')
#define WaterR08(Inp,Off)   MaskI32(IAND(ToInteger(Inp(Off)), Z'000000FF'))
#define WaterR16(Inp,Off)   MaskI32(IAND(ToInteger(Pack_I16(Inp, Off)), Z'0000FFFF'))
#define WaterR32(Inp,Off)   MaskI32(Pack_I32(Inp, Off))

!** MODULE PARAMETERS:
    tLong, PARAMETER    :: WaterP0 = ToLong(Z'00000000A0761D65')
    tLong, PARAMETER    :: WaterP1 = ToLong(Z'00000000E7037ED1')
    tLong, PARAMETER    :: WaterP2 = ToLong(Z'000000008EBC6AF1')
    tLong, PARAMETER    :: WaterP3 = ToLong(Z'00000000589965CD')
    tLong, PARAMETER    :: WaterP4 = ToLong(Z'000000001D8E4E27')
    tLong, PARAMETER    :: WaterP5 = ToLong(Z'00000000EB44ACCB')
    tLong, PARAMETER    :: LONG_LO_MASK = ToLong(Z'00000000FFFFFFFF')

!** DERIVED TYPE DEFINITIONS
    !> *WaterHasher32* is a hasher type that outputs the hash value as a 32-bit integer.
    !  It employs the *Water* hash algorithm by Tommy Ettinger.
    TYPE, EXTENDS(Hasher32) :: WaterHasher32
        PRIVATE
        !% state
        tLong       :: State        = 0_kInteger
        !% buffer array used to store input data
        tByte       :: BufArr(0:15) = 0_kByte
        !% flag indicating whether to remove sign from the final hash value
        tLogical    :: RemoveSign   = FalseVal
    CONTAINS
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the hasher. <br>
        !  **Usage**: <br>
        !   --->    Name = Hasher%GetName()
        PROCEDURE   :: GetName          => Water_GetName
        !> *GetBlockLength* is a procedure to return the block length of this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: GetBlockLength   => Water_BlockLength
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: SetBufPtr        => Water_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: ProcessBlock     => Water_ProcessBlock
        !> **Type-Bound Subroutine**: Initialize <br>
        !  **Purpose**:  To initialize the hasher. <br>
        !  **Usage**: <br>
        !   --->    CALL Hasher%Initialize(Seed)            ! hash value with sign <br>
        !   --->    CALL Hasher%Initialize(Seed, .TRUE.)    ! remove sign from hash value <br>
        PROCEDURE   :: Initialize       => Water_Initialize
        !> **Type-Bound Function**: Finalize <br>
        !  **Purpose**:  To finalize the current hash computation and return the hash value
        !                in a 32-bit integer.  The object is reset. <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%Finalize()
        PROCEDURE   :: Finalize         => Water_Finalize
        !> **Type-Bound Function**: HashDirect <br>
        !  **Purpose**:  To compute the hash value directly (non-incrementally). <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize) <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize, Seed) <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize, RemoveSign=.TRUE.) <br>
        !  **Note**: The specified input can be any type and any rank where its size is
        !            the number of bytes of storage used by the input.
        PROCEDURE   :: HashDirect       => Water_HashDirect
    END TYPE WaterHasher32

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

FUNCTION Water_GetName(HS) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the display name for this hasher.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(WaterHasher32), INTENT(IN)    :: HS   !! a hasher (HS) object
    tCharAlloc                          :: Name !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Name = 'Water_Hahser32'
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION Water_GetName

!******************************************************************************

FUNCTION Water_BlockLength(HS) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the *block length* for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(WaterHasher32), INTENT(IN)    :: HS       !! a hasher (HS) object
    tIndex                              :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Length = 16_kIndex
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION Water_BlockLength

!******************************************************************************

SUBROUTINE Water_SetBufPtr(HS, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(WaterHasher32), TARGET, INTENT(INOUT) :: HS           !! a hasher (HS) object
    tByte,               POINTER, INTENT(INOUT) :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => HS%BufArr

    RETURN

END SUBROUTINE Water_SetBufPtr

!******************************************************************************

SUBROUTINE Water_ProcessBlock(HS, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(WaterHasher32), INTENT(INOUT) :: HS           !! a hasher (HS) object
    tByte,                INTENT(IN)    :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: LHS, RHS

!** FLOW

    ASSOCIATE (H => HS%State)
        CALL WaterMum(IEOR(WaterR32(BytesIn, 0_kIndex),  WaterP1), &
                      IEOR(WaterR32(BytesIn, 4_kIndex),  WaterP2), LHS)
        CALL WaterMum(IEOR(WaterR32(BytesIn, 8_kIndex),  WaterP3), &
                      IEOR(WaterR32(BytesIn, 12_kIndex), WaterP4), RHS)
        CALL WaterMum(LHS + H, RHS, H)
    END ASSOCIATE

    RETURN

END SUBROUTINE Water_ProcessBlock

!******************************************************************************

SUBROUTINE Water_Initialize(HS, Seed, RemoveSign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the hasher.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(WaterHasher32), TARGET, INTENT(INOUT) :: HS   !! a hasher (HS) object
    tInteger,                     INTENT(IN)    :: Seed !! seed
    tLogical,           OPTIONAL, INTENT(IN)    :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    HS%State = Seed
    SET_OPTION(HS%RemoveSign, FalseVal, RemoveSign)
    CALL HS%Reset()

    RETURN

END SUBROUTINE Water_Initialize

!******************************************************************************

FUNCTION Water_Finalize(HS) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  in a 32-bit integer.  The object is reset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(WaterHasher32), INTENT(INOUT) :: HS       !! a hasher (HS) object
    tInteger                            :: HashCode !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: LHS, RHS
    tIndex      :: Length, Remaining, Offset

!** FLOW
    
    ! initialize
    Remaining = HS%GetBufLen()
    Length    = HS%GetBlockCount()*HS%GetBlockLength() + Remaining
    Offset    = 0_kIndex

    ASSOCIATE (H => HS%State)
        H = H + WaterP5
        ! process remaining
        SELECT CASE(Remaining)
        CASE(1)
            CALL WaterMum(IEOR(WaterP2, H), IEOR(WaterR08(HS%BufArr, Offset), WaterP1), H)
        CASE(2)
            CALL WaterMum(IEOR(WaterP3, H), IEOR(WaterR16(HS%BufArr, Offset), WaterP4), H)
        CASE(3)
            CALL WaterMum(IEOR(WaterR16(HS%BufArr, Offset), H), &
                          IEOR(WaterR08(HS%BufArr, Offset + 2), WaterP2), H)
        CASE(4)
            CALL WaterMum(IEOR(WaterR16(HS%BufArr, Offset), H), &
                          IEOR(WaterR16(HS%BufArr, Offset + 2), WaterP3), H)
        CASE(5)
            CALL WaterMum(IEOR(WaterR32(HS%BufArr, Offset), H), &
                          IEOR(WaterR08(HS%BufArr, Offset + 4), WaterP1), H)
        CASE(6)
            CALL WaterMum(IEOR(WaterR32(HS%BufArr, Offset), H), &
                          IEOR(WaterR16(HS%BufArr, Offset + 4), WaterP1), H)
        CASE(7)
            CALL WaterMum(IEOR(WaterR32(HS%BufArr, Offset), H), &
                          IEOR(IOR(SHIFTL(WaterR16(HS%BufArr, Offset + 4), 8), &
                               WaterR08(HS%BufArr, Offset + 6)), WaterP1), H)
        CASE(8)
            CALL WaterMum(IEOR(WaterR32(HS%BufArr, Offset), H), &
                        IEOR(WaterR32(HS%BufArr, Offset + 4), WaterP0), H)
        CASE(9)
            CALL WaterMum(IEOR(WaterR32(HS%BufArr, Offset), H), &
                          IEOR(WaterR32(HS%BufArr, Offset + 4), WaterP2), LHS)
            CALL WaterMum(IEOR(H, WaterP4), IEOR(WaterR08(HS%BufArr, Offset + 8), WaterP3), RHS)
            H = IEOR(LHS, RHS)
        CASE(10)
            CALL WaterMum(IEOR(WaterR32(HS%BufArr, Offset), H), &
                          IEOR(WaterR32(HS%BufArr, Offset + 4), WaterP2), LHS)
            CALL WaterMum(H, IEOR(WaterR16(HS%BufArr, Offset + 8), WaterP3), RHS)
            H = IEOR(LHS, RHS)
        CASE(11)
            CALL WaterMum(IEOR(WaterR32(HS%BufArr, Offset), H), &
                          IEOR(WaterR32(HS%BufArr, Offset + 4), WaterP2), LHS)
            CALL WaterMum(H, IEOR(IOR(SHIFTL(WaterR16(HS%BufArr, Offset + 8),8), &
                                  WaterR08(HS%BufArr, Offset + 10)), WaterP3), RHS)
            H = IEOR(LHS, RHS)
        CASE(12)
            CALL WaterMum(IEOR(WaterR32(HS%BufArr, Offset), H), &
                          IEOR(WaterR32(HS%BufArr, Offset + 4), WaterP2), LHS)
            CALL WaterMum(IEOR(H, WaterR32(HS%BufArr, Offset + 8)), WaterP4, RHS)
            H = IEOR(LHS, RHS)
        CASE(13)
            CALL WaterMum(IEOR(WaterR32(HS%BufArr, Offset), H), &
                          IEOR(WaterR32(HS%BufArr, Offset + 4), WaterP2), LHS)
            CALL WaterMum(IEOR(H, WaterR32(HS%BufArr, Offset + 8)), &
                          IEOR(WaterR08(HS%BufArr, Offset + 12), WaterP4), RHS)
            H = IEOR(LHS, RHS)
        CASE(14)
            CALL WaterMum(IEOR(WaterR32(HS%BufArr, Offset), H), &
                          IEOR(WaterR32(HS%BufArr, Offset + 4), WaterP2), LHS)
            CALL WaterMum(IEOR(H, WaterR32(HS%BufArr, Offset + 8)), &
                          IEOR(WaterR16(HS%BufArr, Offset + 12), WaterP4), RHS)
            H = IEOR(LHS, RHS)
        CASE(15)
            CALL WaterMum(IEOR(WaterR32(HS%BufArr, Offset), H), &
                          IEOR(WaterR32(HS%BufArr, Offset + 4), WaterP2), LHS)
            CALL WaterMum(IEOR(H, WaterR32(HS%BufArr, Offset + 8)), &
                          IEOR(IOR(SHIFTL(WaterR16(HS%BufArr, Offset + 12),8), &
                               WaterR08(HS%BufArr, Offset + 14)), WaterP4), RHS)
            H = IEOR(LHS, RHS)
        END SELECT
        ! finalize
        H = IEOR(H, SHIFTL(H,16))*IEOR(ToLong(Length), WaterP0)
        HashCode = ToInteger(IAND(H - SHIFTR(H, 32), LONG_LO_MASK))
    END ASSOCIATE
    
    ! remove sign if needed
    IF (HS%RemoveSign) HashCode = IAND(HashCode, ToInteger(Z'7FFFFFFF'))

    ! reset the hasher
    HS%State  = 0_kInteger
    HS%BufArr = 0_kByte
    HS%RemoveSign = FalseVal
    CALL HS%Reset()

    RETURN

END FUNCTION Water_Finalize

!******************************************************************************

FUNCTION Water_HashDirect(HS, Input, InpSize, Seed, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value directly (non-incremental).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(WaterHasher32),   INTENT(INOUT)   :: HS           !! a hasher (HS) object
    TYPE(*), DIMENSION(..), INTENT(IN)      :: Input        !! input (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize      !! size of the input (in bytes)
    tInteger, OPTIONAL,     INTENT(IN)      :: Seed         !! seed
    tLogical, OPTIONAL,     INTENT(IN)      :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tInteger                                :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: Seed0

!** FLOW

    SET_OPTION(Seed0, 0_kInteger, Seed)
    
    ! The following code illustrates simple use of the hasher.
    CALL HS%Initialize(Seed0, RemoveSign)
    CALL HS%Update(Input, InpSize)
    HashCode = HS%Finalize()

    RETURN

END FUNCTION Water_HashDirect

!******************************************************************************

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

!******************************************************************************

END MODULE Class_WaterHasher32

!******************************************************************************


MODULE Class_NMHasher32

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *NMHasher32* and *NMxHasher32* types and their related
!   routines.  The *NMHasher32* type is a hasher type that extends directly from the
!   <a href="../module/class_hasher32.html#type-hasher32">Hasher32</a> type.  It
!   provides all deferred procedures required by a *Hasher32* class and outputs the
!   hash value as a 32-bit integer.  The *NMxHasher32* type is a hasher type that
!   extends from the *NMHasher32* type and overrides a couple of methods that require
!   different implementation from those of the *NMHasher32* type. <br>
!   The *NMHasher32* type employs the *NM* hash algorithm for 32-bit integer
!   output by James Z. M. Gao. [1] whereas the *NMxHasher32* type employs the *NMx*
!   hash algorithm, which is a variant of the *NM* hash algorithm.  As hashers, both
!   can be used to compute the hash value incrementally.  They also provide a method
!   to compute the hash value directly (i.e. non-incrementally).  The following code
!   snippet illustrates a typical usage of both hashers.
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
!   However, if the *Update* method is to be called only one time, then the *HashDirect*
!   method should be employed as follows.
!   <Pre><Code style="color:MidnightBlue;">
!   ! directly compute the hash value
!   HashCode = Hasher%HashDirect(Input, InpSize, Seed)
!   </Code></Pre>
!^ **REFERENCES**: <br>
!   [1] <a href="https://github.com/gzm55/hash-garage">NMHash32 Hash Functions. </a> <br>

!** USE STATEMENTS:
    USE ModBase_Common
    USE Class_Hasher32
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: NMHasher32
    PUBLIC :: NMxHasher32

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define MaskInteger(X)                  IAND(ToInteger(X), Z'000000FF')
#define NMHASH32_9to32(P, Len, Seed)    NMHASH32_9to255(P, Len, Seed, FalseVal)
#define NMHASH32_33to255(P, Len, Seed)  NMHASH32_9to255(P, Len, Seed, TrueVal)
#define PackFull(Buf, Off)              MaskInteger(Buf(Off)) + SHIFTL(MaskInteger(Buf(Off+1)),  8) + \
                                        SHIFTL(MaskInteger(Buf(Off+2)), 16) + SHIFTL(MaskInteger(Buf(Off+3)), 24)
#define MaskU32(X)                      IAND(ToInteger(X), Z'0000FFFF')
#define PackInteger(X)                  IOR(MaskU32(X(0)), SHIFTL(MaskU32(X(1)), 16))
#define UnPackInteger(X, Y)             Y(0) = ToShort(X); Y(1) = ToShort(SHIFTR(X, 16))

!** MODULE PARAMETERS:
    ! primes from xxh!
    tInteger, PARAMETER     :: NMH_PRIME32_1 = ToInteger(Z'9E3779B1')
    tInteger, PARAMETER     :: NMH_PRIME32_2 = ToInteger(Z'85EBCA77')
    tInteger, PARAMETER     :: NMH_PRIME32_3 = ToInteger(Z'C2B2AE3D')
    tInteger, PARAMETER     :: NMH_PRIME32_4 = ToInteger(Z'27D4EB2F')
    ! Pseudorandom secret taken directly from FARSH
    tInteger, PARAMETER     :: NMH_ACC_INIT(0:31) = [                   &
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
    tShort,   PARAMETER     :: NMH_M1_16(0:1) = [ToShort(Z'F0D9649B'), &
                                                 ToShort(SHIFTR(ToInteger(Z'F0D9649B'), 16))]
    tShort,   PARAMETER     :: NMH_M2_16(0:1) = [ToShort(Z'29A7935D'), &
                                                 ToShort(SHIFTR(ToInteger(Z'29A7935D'), 16))]
    tShort,   PARAMETER     :: NMH_M3_16(0:1) = [ToShort(Z'55D35831'), &
                                                 ToShort(SHIFTR(ToInteger(Z'55D35831'), 16))]
    tIndex,   PARAMETER     :: ACC_SIZE = SIZE(NMH_ACC_INIT)
    tIndex,   PARAMETER     :: BlockLen = 2_kIndex*(ACC_SIZE*4_kIndex)

!** DERIVED TYPE DEFINITIONS
    !> *NMHasher32* is a hasher type that outputs the hash value as a 32-bit integer.
    !  It employs the *NM* hash algorithm by James Z. M. Gao.
    TYPE, EXTENDS(Hasher32)   :: NMHasher32
        PRIVATE
        !% states
        tInteger    :: AccX(0:ACC_SIZE-1)   = 0_kInteger
        tInteger    :: AccY(0:ACC_SIZE-1)   = 0_kInteger
        !% buffer array used to store input data
        tByte       :: BufArr(0:BlockLen-1) = 0_kByte
        !% flag indicating whether to remove sign from the final hash value
        tLogical    :: RemoveSign   = FalseVal
    CONTAINS
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the hasher. <br>
        !  **Usage**: <br>
        !   --->    Name = Hasher%GetName()
        PROCEDURE   :: GetName          => NM_GetName
        !> *GetBlockLength* is a procedure to return the block length of this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: GetBlockLength   => NM_BlockLength
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: SetBufPtr        => NM_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: ProcessBlock     => NM_ProcessBlock
        !> **Type-Bound Subroutine**: Initialize <br>
        !  **Purpose**:  To initialize the hasher. <br>
        !  **Usage**: <br>
        !   --->    CALL Hasher%Initialize(Seed)            ! hash value with sign <br>
        !   --->    CALL Hasher%Initialize(Seed, .TRUE.)    ! remove sign from hash value <br>
        PROCEDURE   :: Initialize       => NM_Initialize
        !> **Type-Bound Function**: Finalize <br>
        !  **Purpose**:  To finalize the current hash computation and return the hash value
        !                in a 32-bit integer.  The object is reset. <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%Finalize()
        PROCEDURE   :: Finalize         => NM_Finalize
        !> **Type-Bound Function**: HashDirect <br>
        !  **Purpose**:  To compute the hash value directly (non-incrementally). <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize) <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize, Seed) <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize, RemoveSign=.TRUE.) <br>
        !  **Note**: The specified input can be any type and any rank where its size is
        !            the number of bytes of storage used by the input.
        PROCEDURE   :: HashDirect       => NM_HashDirect
    END TYPE NMHasher32
    !^ *NMHasher32* is a hasher type that outputs the hash value as a 32-bit integer.
    !  It employs the *NMx* hash algorithm, which is a variant of the  *NM* hash algorithm.
    TYPE, EXTENDS(NMHasher32) :: NMxHasher32
    CONTAINS
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the hasher. <br>
        !  **Usage**: <br>
        !   --->    Name = Hasher%GetName()
        PROCEDURE   :: GetName          => NMx_GetName
        !> **Type-Bound Function**: Finalize <br>
        !  **Purpose**:  To finalize the current hash computation and return the hash value
        !                in a 32-bit integer.  The object is reset. <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%Finalize()
        PROCEDURE   :: Finalize         => NMx_Finalize
    END TYPE NMxHasher32

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

FUNCTION NM_GetName(HS) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the display name for this hasher.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NMHasher32), INTENT(IN)   :: HS   !! a hasher (HS) object
    tCharAlloc                      :: Name !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Name = 'NM_Hahser32'
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION NM_GetName

!******************************************************************************

FUNCTION NMx_GetName(HS) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the display name for this hasher.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NMxHasher32), INTENT(IN)  :: HS   !! a hasher (HS) object
    tCharAlloc                      :: Name !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Name = 'NMx_Hahser32'
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION NMx_GetName

!******************************************************************************

FUNCTION NM_BlockLength(HS) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the *block length* for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NMHasher32), INTENT(IN)   :: HS       !! a hasher (HS) object
    tIndex                          :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Length = BlockLen
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION NM_BlockLength

!******************************************************************************

SUBROUTINE NM_SetBufPtr(HS, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NMHasher32), TARGET, INTENT(INOUT)    :: HS           !! a hasher (HS) object
    tByte,            POINTER, INTENT(INOUT)    :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => HS%BufArr

    RETURN

END SUBROUTINE NM_SetBufPtr

!******************************************************************************

SUBROUTINE NM_ProcessBlock(HS, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NMHasher32), INTENT(INOUT)    :: HS           !! a hasher (HS) object
    tByte,             INTENT(IN)       :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL NMHASH32_LongRound(HS%AccX, HS%AccY, BytesIn)

    RETURN

END SUBROUTINE NM_ProcessBlock

!******************************************************************************

SUBROUTINE NM_Initialize(HS, Seed, RemoveSign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the hasher.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NMHasher32), TARGET, INTENT(INOUT)    :: HS   !! a hasher (HS) object
    tInteger,                  INTENT(IN)       :: Seed !! seed
    tLogical,        OPTIONAL, INTENT(IN)       :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    HS%AccX = NMH_ACC_INIT
    HS%AccY = Seed
    SET_OPTION(HS%RemoveSign, FalseVal, RemoveSign)
    CALL HS%Reset()

    RETURN

END SUBROUTINE NM_Initialize

!******************************************************************************

FUNCTION NM_Finalize(HS) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  in a 32-bit integer.  The object is reset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NMHasher32), INTENT(INOUT)    :: HS       !! a hasher (HS) object
    tInteger                            :: HashCode !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Length

!** FLOW
    
    IF (HS%GetBlockCount() > 0_kIndex) THEN
        ! compute length
        Length = HS%GetBlockCount()*HS%GetBlockLength() + HS%GetBufLen()
        BLOCK
            ! block variables
            tInteger    :: Sum
            tByte       :: LastBuf(0:BlockLen-1)
            tIndex      :: I, J
            ! initialize
            Sum = 0
            ! copy buffer data for last round
            J = HS%GetBufLen() + 1_kIndex
            DO I = 0_kIndex, BlockLen-1_kIndex
                LastBuf(I) = HS%BufArr(J)
                J = J + 1_kIndex
                IF (J >= BlockLen) J = 0_kIndex
            END DO
            ! perform last round
            CALL NMHASH32_LongRound(HS%AccX, HS%AccY, LastBuf)
            ! merge acc
            DO I = 0, ACC_SIZE-1
                HS%AccX(I) = IEOR(HS%AccX(I), NMH_ACC_INIT(I))
                Sum = Sum + HS%AccX(I)
            END DO
            ! get hash
            Sum = Sum + ToInteger(SHIFTA(Length, 32))
            HashCode = NMHASH32_Avalanche32(IEOR(Sum, ToInteger(Length)))
        END BLOCK
    ELSE
        Length = HS%GetBufLen()
        ASSOCIATE (Input => HS%BufArr, Seed => HS%AccY(0))
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
                            X = Pack_Partial(Input, 0, Length)
                        CASE(4)
                            NewSeed = Seed + NMH_PRIME32_3
                            X = PackFull(Input, 0)
                        END SELECT
                        HashCode = NMHASH32_0to8(X + NewSeed, RotateLeft(NewSeed, 5))
                    END BLOCK
                END IF
            ELSE
                HashCode = NMHASH32_33to255(Input, Length, Seed)
            END IF
        END ASSOCIATE
    END IF
    
    ! remove sign if needed
    IF (HS%RemoveSign) HashCode = IAND(HashCode, ToInteger(Z'7FFFFFFF'))

    ! reset the hasher
    HS%AccX   = 0_kInteger
    HS%AccY   = 0_kInteger
    HS%BufArr = 0_kByte
    HS%RemoveSign = FalseVal
    CALL HS%Reset()

    RETURN
    
CONTAINS

    FUNCTION NMHASH32_0to8(X, Seed) RESULT(HashCode)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, INTENT(IN)    :: X
        tInteger, INTENT(IN)    :: Seed
        tInteger                :: HashCode

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tShort,   PARAMETER :: M1_16(0:1) = [ToShort(Z'776BF593'), ToShort(SHIFTR(ToInteger(Z'776BF593'), 16))]
        tShort,   PARAMETER :: M2_16(0:1) = [ToShort(Z'3FB39C65'), ToShort(SHIFTR(ToInteger(Z'3FB39C65'), 16))]
        tShort,   PARAMETER :: M3_16(0:1) = [ToShort(Z'E9139917'), ToShort(SHIFTR(ToInteger(Z'E9139917'), 16))]

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: VxU32
        tShort      :: VxU16(0:1)

    !** FLOW

        ! initialize
        VxU32 = X

        ! avalanche
        VxU32 = IEOR(VxU32, IEOR(SHIFTR(VxU32, 12), SHIFTR(VxU32, 6)))

        UnPackInteger(VxU32, VxU16)
        VxU16 = VxU16*M1_16
        VxU32 = PackInteger(VxU16)

        VxU32 = IEOR(VxU32, IEOR(SHIFTL(VxU32, 11), SHIFTR(VxU32, 19)))

        UnPackInteger(VxU32, VxU16)
        VxU16 = VxU16*M2_16
        VxU32 = PackInteger(VxU16)

        VxU32 = IEOR(VxU32, Seed)
        VxU32 = IEOR(VxU32, IEOR(SHIFTR(VxU32, 15), SHIFTR(VxU32, 9)))

        UnPackInteger(VxU32, VxU16)
        VxU16 = VxU16*M3_16
        VxU32 = PackInteger(VxU16)

        HashCode = IEOR(VxU32, IEOR(SHIFTL(VxU32, 16), SHIFTR(VxU32, 11)))

        RETURN

    END FUNCTION NMHASH32_0to8

    !**************************************************************************

    FUNCTION NMHASH32_9to255(P, Length, Seed, FullAvalanche) RESULT(HashCode)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,    INTENT(IN)    :: P(0:)
        tIndex,   INTENT(IN)    :: Length
        tInteger, INTENT(IN)    :: Seed
        tLogical, INTENT(IN)    :: FullAvalanche
        tInteger                :: HashCode

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: I, J, K, R, ShiftLen
        tInteger    :: XU32(0:3), YU32(0:3), SL
        tShort      :: XU16(0:1)

    !** FLOW

        ! initialize
        SL = Seed + ToInteger(Length)
        XU32(0) = NMH_PRIME32_1
        XU32(1) = NMH_PRIME32_2
        XU32(2) = NMH_PRIME32_3
        XU32(3) = NMH_PRIME32_4
        YU32 = SL

        IF (FullAvalanche) THEN
            ! 33 to 255 bytes
            R = (Length - 1) / 32
            DO I = 0, R-1
                DO J = 0, 3
                    K = I*32 + J*4
                    XU32(J) = IEOR(XU32(J), PackFull(P, K))
                    YU32(J) = IEOR(YU32(J), PackFull(P, K + 16))
                    XU32(J) = XU32(J) + YU32(J)

                    UnPackInteger(XU32(J), XU16)
                    XU16 = XU16*NMH_M1_16
                    XU32(J) = PackInteger(XU16)

                    XU32(J) = IEOR(XU32(J), IEOR(SHIFTL(XU32(J), 5), SHIFTR(XU32(J), 13)))

                    UnPackInteger(XU32(J), XU16)
                    XU16 = XU16*NMH_M2_16
                    XU32(J) = PackInteger(XU16)

                    XU32(J) = IEOR(XU32(J), YU32(J))
                    XU32(J) = IEOR(XU32(J), IEOR(SHIFTL(XU32(J), 11), SHIFTR(XU32(J), 9)))

                    UnPackInteger(XU32(J), XU16)
                    XU16 = XU16*NMH_M3_16
                    XU32(J) = PackInteger(XU16)

                    XU32(J) = IEOR(XU32(J), IEOR(SHIFTR(XU32(J), 10), SHIFTR(XU32(J), 20)))
                END DO
            END DO
            K = 0
            DO J = 0, 3
                XU32(J) = IEOR(XU32(J), PackFull(P, Length - 32 + K))
                YU32(J) = IEOR(YU32(J), PackFull(P, Length - 16 + K))
                K = K + 4
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
            YU32(2) = IEOR(YU32(2), PackFull(P, Length - 4))
            YU32(3) = IEOR(YU32(3), PackFull(P, Length - 4 - ShiftLen))
        END IF

        DO J = 0, 3
            XU32(J) = XU32(J) + YU32(J)
            YU32(J) = IEOR(YU32(J), IEOR(SHIFTL(YU32(J), 17), SHIFTR(YU32(J), 6)))

            UnPackInteger(XU32(J), XU16)
            XU16 = XU16*NMH_M1_16
            XU32(J) = PackInteger(XU16)

            XU32(J) = IEOR(XU32(J), IEOR(SHIFTL(XU32(J), 5), SHIFTR(XU32(J), 13)))

            UnPackInteger(XU32(J), XU16)
            XU16 = XU16*NMH_M2_16
            XU32(J) = PackInteger(XU16)

            XU32(J) = IEOR(XU32(J), YU32(J))
            XU32(J) = IEOR(XU32(J), IEOR(SHIFTL(XU32(J), 11), SHIFTR(XU32(J), 9)))

            UnPackInteger(XU32(J), XU16)
            XU16 = XU16*NMH_M3_16
            XU32(J) = PackInteger(XU16)

            XU32(J) = IEOR(XU32(J), IEOR(SHIFTR(XU32(J), 10), SHIFTR(XU32(J), 20)))
        END DO

        XU32(0) = IEOR(XU32(0), NMH_PRIME32_1)
        XU32(1) = IEOR(XU32(1), NMH_PRIME32_2)
        XU32(2) = IEOR(XU32(2), NMH_PRIME32_3)
        XU32(3) = IEOR(XU32(3), NMH_PRIME32_4)
        XU32(0) = XU32(0) + XU32(1) + XU32(2) + XU32(3)
        XU32(0) = IEOR(XU32(0), SL + SHIFTR(SL, 5))

        UnPackInteger(XU32(0), XU16)
        XU16 = XU16*NMH_M3_16
        XU32(0) = PackInteger(XU16)

        XU32(0) = IEOR(XU32(0), IEOR(SHIFTR(XU32(0), 10), SHIFTR(XU32(0), 20)))

        HashCode = XU32(0)

        RETURN

    END FUNCTION NMHASH32_9to255

    !**************************************************************************

    FUNCTION NMHASH32_Avalanche32(X) RESULT(HashCode)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, INTENT(IN)    :: X
        tInteger                :: HashCode

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tShort,   PARAMETER :: M1_16(0:1) = [ToShort(Z'CCE5196D'), ToShort(SHIFTR(ToInteger(Z'CCE5196D'), 16))]
        tShort,   PARAMETER :: M2_16(0:1) = [ToShort(Z'464BE229'), ToShort(SHIFTR(ToInteger(Z'464BE229'), 16))]

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: VxU32
        tShort      :: VxU16(0:1)

    !** FLOW

        ! initialize
        VxU32 = X

        ! avalanche
        VxU32 = IEOR(VxU32, IEOR(SHIFTR(VxU32, 8), SHIFTR(VxU32, 21)))

        UnPackInteger(VxU32, VxU16)
        VxU16 = VxU16*M1_16
        VxU32 = PackInteger(VxU16)

        VxU32 = IEOR(VxU32, IEOR(SHIFTL(VxU32, 12), SHIFTR(VxU32, 7)))

        UnPackInteger(VxU32, VxU16)
        VxU16 = VxU16*M2_16
        VxU32 = PackInteger(VxU16)

        HashCode = IEOR(IEOR(VxU32, SHIFTR(VxU32, 8)), SHIFTR(VxU32, 21))

        RETURN

    END FUNCTION NMHASH32_Avalanche32

    !**************************************************************************

END FUNCTION NM_Finalize

!******************************************************************************

FUNCTION NMx_Finalize(HS) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  in a 32-bit integer.  The object is reset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NMxHasher32), INTENT(INOUT)   :: HS       !! a hasher (HS) object
    tInteger                            :: HashCode !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Length

!** FLOW
    
    ! initialize
    IF (HS%GetBlockCount() > 0_kIndex) THEN
        ! compute length
        Length = HS%GetBlockCount()*HS%GetBlockLength() + HS%GetBufLen()
        BLOCK
            ! block variables
            tInteger    :: Sum
            tByte       :: LastBuf(0:BlockLen-1)
            tIndex      :: I, J
            ! initialize
            Sum = 0
            ! copy buffer data for last round
            J = HS%GetBufLen() + 1_kIndex
            DO I = 0_kIndex, BlockLen-1_kIndex
                LastBuf(I) = HS%BufArr(J)
                J = J + 1_kIndex
                IF (J >= BlockLen) J = 0_kIndex
            END DO
            ! perform last round
            CALL NMHASH32_LongRound(HS%AccX, HS%AccY, LastBuf)
            ! merge acc
            DO I = 0, ACC_SIZE-1
                HS%AccX(I) = IEOR(HS%AccX(I), NMH_ACC_INIT(I))
                Sum = Sum + HS%AccX(I)
            END DO
            ! get hash
            Sum = Sum + ToInteger(SHIFTA(Length, 32))
            HashCode = NMxHASH32_Avalanche32(IEOR(Sum, ToInteger(Length)))
        END BLOCK
    ELSE
        Length = HS%GetBufLen()
        ASSOCIATE (Input => HS%BufArr, Seed => HS%AccY(0))
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
                            X = Pack_Partial(Input, 0, Length)
                        CASE(4)
                            NewSeed = Seed + NMH_PRIME32_1
                            X = PackFull(Input, 0)
                        END SELECT
                        HashCode = NMxHASH32_0to4(X, NewSeed)
                    END BLOCK
                END IF
            ELSE
                HashCode = NMxHASH32_9to255(Input, Length, Seed)
            END IF
        END ASSOCIATE
    END IF
    
    ! remove sign if needed
    IF (HS%RemoveSign) HashCode = IAND(HashCode, ToInteger(Z'7FFFFFFF'))

    ! reset the hasher
    HS%AccX   = 0_kInteger
    HS%AccY   = 0_kInteger
    HS%BufArr = 0_kByte
    HS%RemoveSign = FalseVal
    CALL HS%Reset()

    RETURN
    
CONTAINS

    FUNCTION NMxHASH32_0to4(X, Seed) RESULT(HashCode)

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

    !**************************************************************************

    FUNCTION NMxHASH32_5to8(P, Length, Seed) RESULT(HashCode)

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

    !**************************************************************************

    FUNCTION NMxHASH32_9to255(P, Length, Seed) RESULT(HashCode)

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

    !**************************************************************************

    FUNCTION NMxHASH32_Avalanche32(X) RESULT(HashCode)

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

    !**************************************************************************

END FUNCTION NMx_Finalize

!******************************************************************************

FUNCTION NM_HashDirect(HS, Input, InpSize, Seed, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value directly (non-incremental).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(NMHasher32),      INTENT(INOUT)   :: HS           !! a hasher (HS) object
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

END FUNCTION NM_HashDirect

!******************************************************************************

SUBROUTINE NMHASH32_LongRound(AccX, AccY, P)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(INOUT) :: AccX(0:)
    tInteger, INTENT(INOUT) :: AccY(0:)
    tByte,    INTENT(IN)    :: P(0:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, Offset, SizeOfACC
    tShort      :: Acc_16(0:1)

!** FLOW

    ! initialize
    Offset = 0_kIndex
    SizeOfACC = ACC_SIZE*4

    DO I = 0_kIndex, ACC_SIZE-1_kIndex
        AccX(I) = IEOR(AccX(I), PackFull(P, Offset))
        AccY(I) = IEOR(AccY(I), PackFull(P, Offset + SizeOfACC))
        AccX(I) = AccX(I) + AccY(I)
        AccY(I) = IEOR(AccY(I), SHIFTR(AccX(I), 1))

        UnPackInteger(AccX(I), Acc_16)
        Acc_16 = Acc_16*NMH_M1_16
        AccX(I) = PackInteger(Acc_16)

        AccX(I) = IEOR(AccX(I), IEOR(SHIFTL(AccX(I), 5), SHIFTR(AccX(I), 13)))

        UnPackInteger(AccX(I), Acc_16)
        Acc_16 = Acc_16*NMH_M2_16
        AccX(I) = PackInteger(Acc_16)

        AccX(I) = IEOR(AccX(I), AccY(I))
        AccX(I) = IEOR(AccX(I), IEOR(SHIFTL(AccX(I), 11), SHIFTR(AccX(I), 9)))

        UnPackInteger(AccX(I), Acc_16)
        Acc_16 = Acc_16*NMH_M3_16
        AccX(I) = PackInteger(Acc_16)

        AccX(I) = IEOR(AccX(I), IEOR(SHIFTR(AccX(I), 10), SHIFTR(AccX(I), 20)))

        ! update index
        Offset = Offset + 4_kIndex
    END DO

    RETURN

END SUBROUTINE NMHASH32_LongRound

!******************************************************************************

FUNCTION Pack_Partial(Buf, Off, Length) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To pack seven or fewer bytes of the array 'Buf' at offset 'Off'
    ! into the 64-bit word 'Res', in little-endian convention
    ! (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,  INTENT(IN)  :: Buf(0:)  ! buffer
    tIndex, INTENT(IN)  :: Off      ! offset
    tIndex, INTENT(IN)  :: Length   ! the number of bytes to pack (between 1 to 3)
    tInteger            :: Res      ! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte       :: Wrk(0:3)

! FLOW

    Wrk(0:Length-1) = Buf(Off:Off+Length-1)
    Wrk(Length:3)   = 0_kByte
    Res = MaskInteger(Wrk(0)) + SHIFTL(MaskInteger(Wrk(1)),  8) + &
          SHIFTL(MaskInteger(Wrk(2)), 16) + SHIFTL(MaskInteger(Wrk(3)), 24)

    RETURN

END FUNCTION Pack_Partial

!******************************************************************************

END MODULE Class_NMHasher32

!******************************************************************************

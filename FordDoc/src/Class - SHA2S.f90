
MODULE Class_SHA2S

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *SHA2S* type and its related routines.
!   The *SHA2S* type is a *digest* type that directly extends from the
!   <a href="../module/class_mdhelper.html#type-mdhelper">MDHelper</a>
!   type.  As a *concrete* derived type, it provides all remaining
!   deferred procedures required by all its abstract parent types. <br>
!   The *SHA2S* type implements an incremental cryptographic hash
!   function by employing the *SHA-224 or SHA-256 message-digest*
!   algorithm where both algorithms are described in FIPS 180-4 [1].
!   The implementation here is mainly based on the references [2, 3].  <br>
!   By default, the *SHA2S* type employs the *SHA-256 message-digest*
!   algorithm.  However, a user can specify the *IsSHA224* flag to
!   true when initializing the digest object (by calling the *Create*
!   method) in order to use the *SHA-224 message-digest* algorithm
!   instead of the default one. <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://doi.org/10.6028%2FNIST.FIPS.180-4">FIPS PUB 180-4:
!       Secure Hash Standard (SHS). </a> <br>
!   [2] <a href="https://github.com/pornin/sphlib">SPHLIB 3.0: A Set of
!       Implementations of Various Hash Functions, Both in C and in Java. </a> <br>
!   [3] <a href="https://github.com/bcgit/bc-java">The Bouncy Castle Crypto
!       Package For Java. </a> <br>

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_BytePack,           ONLY: BytePackBE, ByteUnpackBE
    USE Class_BaseDigest
    USE Class_MDHelper

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: SHA2S

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tIndex,   PARAMETER :: BlockLen = 64_kIndex
    tIndex,   PARAMETER :: DLen224  = 28_kIndex
    tIndex,   PARAMETER :: DLen256  = 32_kIndex
    tInteger, PARAMETER :: IV224(0:7) = [                   &
            ToInteger(Z'C1059ED8'), ToInteger(Z'367CD507'), &
            ToInteger(Z'3070DD17'), ToInteger(Z'F70E5939'), &
            ToInteger(Z'FFC00B31'), ToInteger(Z'68581511'), &
            ToInteger(Z'64F98FA7'), ToInteger(Z'BEFA4FA4')]
    tInteger, PARAMETER :: IV256(0:7) = [                   &
            ToInteger(Z'6A09E667'), ToInteger(Z'BB67AE85'), &
            ToInteger(Z'3C6EF372'), ToInteger(Z'A54FF53A'), &
            ToInteger(Z'510E527F'), ToInteger(Z'9B05688C'), &
            ToInteger(Z'1F83D9AB'), ToInteger(Z'5BE0CD19')]
    tInteger, PARAMETER :: K(0:63) = [                      &
            ToInteger(Z'428A2F98'), ToInteger(Z'71374491'), ToInteger(Z'B5C0FBCF'), ToInteger(Z'E9B5DBA5'), &
            ToInteger(Z'3956C25B'), ToInteger(Z'59F111F1'), ToInteger(Z'923F82A4'), ToInteger(Z'AB1C5ED5'), &
            ToInteger(Z'D807AA98'), ToInteger(Z'12835B01'), ToInteger(Z'243185BE'), ToInteger(Z'550C7DC3'), &
            ToInteger(Z'72BE5D74'), ToInteger(Z'80DEB1FE'), ToInteger(Z'9BDC06A7'), ToInteger(Z'C19BF174'), &
            ToInteger(Z'E49B69C1'), ToInteger(Z'EFBE4786'), ToInteger(Z'0FC19DC6'), ToInteger(Z'240CA1CC'), &
            ToInteger(Z'2DE92C6F'), ToInteger(Z'4A7484AA'), ToInteger(Z'5CB0A9DC'), ToInteger(Z'76F988DA'), &
            ToInteger(Z'983E5152'), ToInteger(Z'A831C66D'), ToInteger(Z'B00327C8'), ToInteger(Z'BF597FC7'), &
            ToInteger(Z'C6E00BF3'), ToInteger(Z'D5A79147'), ToInteger(Z'06CA6351'), ToInteger(Z'14292967'), &
            ToInteger(Z'27B70A85'), ToInteger(Z'2E1B2138'), ToInteger(Z'4D2C6DFC'), ToInteger(Z'53380D13'), &
            ToInteger(Z'650A7354'), ToInteger(Z'766A0ABB'), ToInteger(Z'81C2C92E'), ToInteger(Z'92722C85'), &
            ToInteger(Z'A2BFE8A1'), ToInteger(Z'A81A664B'), ToInteger(Z'C24B8B70'), ToInteger(Z'C76C51A3'), &
            ToInteger(Z'D192E819'), ToInteger(Z'D6990624'), ToInteger(Z'F40E3585'), ToInteger(Z'106AA070'), &
            ToInteger(Z'19A4C116'), ToInteger(Z'1E376C08'), ToInteger(Z'2748774C'), ToInteger(Z'34B0BCB5'), &
            ToInteger(Z'391C0CB3'), ToInteger(Z'4ED8AA4A'), ToInteger(Z'5B9CCA4F'), ToInteger(Z'682E6FF3'), &
            ToInteger(Z'748F82EE'), ToInteger(Z'78A5636F'), ToInteger(Z'84C87814'), ToInteger(Z'8CC70208'), &
            ToInteger(Z'90BEFFFA'), ToInteger(Z'A4506CEB'), ToInteger(Z'BEF9A3F7'), ToInteger(Z'C67178F2')]

!** DERIVED TYPE DEFINITIONS
    !> *SHA2B* is a concrete *digest* type that implements an
    !  incremental cryptographic hash function by employing either the
    !  *SHA-224* or the *SHA-256 message-digest* algorithm.
    TYPE, EXTENDS(MDHelper) :: SHA2S
        PRIVATE
        !% buffer array used to store input data
        tByte       :: BufArr(0:BlockLen-1) = 0_kByte
        !% state
        tInteger    :: State(0:7) = IV256(0:7)
        !% flag indicating whether the SHA-224 algorithm is employed or not.
        tLogical    :: IsSHA224   = FalseVal
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *InitializeWFlag* method to
        !  initialize the *digest* object with specified flag.
        PROCEDURE, PRIVATE  :: InitializeWFlag  => SHA2S_Initialize_wFlag
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default algorithm (SHA-256).
        PROCEDURE       :: Initialize   => SHA2S_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset        => SHA2S_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone     => SHA2S_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName      => SHA2S_GetName
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE       :: GetDigestLen => SHA2S_GetDigestLen
        !> *GetBlockLen* is a procedure to return the block length of the digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: GetBlockLen  => SHA2S_GetBlockLen
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: SetBufPtr    => SHA2S_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: ProcessBlock => SHA2S_ProcessBlock
        !> *DoPadding* is a procedure to perform padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: DoPadding    => SHA2S_DoPadding
        !> *AddBitsNPad* is a procedure to add the final partial byte and then perform
        !  padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: AddBitsNPad  => SHA2S_AddBitsNPad
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   ! initialize the object to employ the default (SHA-256) algorithm <br>
        !   --->    CALL MD%Create() <br>
        !   ! initialize the object to employ the SHA-224 algorithm <br>
        !   --->    CALL MD%Create(IsSHA224=.TRUE.) <br>
        GENERIC         :: Create       => InitializeWFlag
    END TYPE SHA2S

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE SHA2S_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with default algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA2S), INTENT(INOUT) :: MD    !! 'SHA2S' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! to employ the SHA-256 algorithm
    CALL MD%Create(FalseVal)
   
    RETURN

END SUBROUTINE SHA2S_Initialize

!******************************************************************************

SUBROUTINE SHA2S_Initialize_wFlag(MD, IsSHA224)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified flag.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA2S), INTENT(INOUT) :: MD           !! 'SHA2S' object
    tLogical,     INTENT(IN)    :: IsSHA224
    !^ flag indicating whether the SHA-224 algorithm is employed or not. <br>
    !  - If true, use the SHA-224 algorithm. <br>
    !  - Otherwise, use the SHA-256 algorithm. <br>

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLogical, PARAMETER :: LittleEndian = FalseVal
        
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    MD%IsSHA224 = IsSHA224
    CALL MD%HelperInit(LittleEndian, 8_kIndex)
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE SHA2S_Initialize_wFlag

!******************************************************************************

SUBROUTINE SHA2S_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA2S), INTENT(INOUT) :: MD   !! 'SHA2S' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    MD%BufArr = 0_kByte
    IF (MD%IsSHA224) THEN
        MD%State = IV224
    ELSE
        MD%State = IV256
    END IF
    CALL MD%HelperReset()

    RETURN

END SUBROUTINE SHA2S_Reset

!******************************************************************************

SUBROUTINE SHA2S_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA2S),                   INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(SHA2S :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (SHA2S)
        CALL Dst%Create(Src%IsSHA224)
        Dst%State    = Src%State
        Dst%BufArr   = Src%BufArr
        CALL Src%HelperClone(Dst)
    END SELECT
        
    RETURN

END SUBROUTINE SHA2S_GetClone

!******************************************************************************

FUNCTION SHA2S_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA2S), INTENT(IN)    :: MD       !! 'SHA2S' object
    tCharAlloc                  :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsSHA224) THEN
        Name = 'SHA-224'
    ELSE
        Name = 'SHA-256'
    END IF

    RETURN

END FUNCTION SHA2S_GetName

!******************************************************************************

FUNCTION SHA2S_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the natural hash function output length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA2S), INTENT(IN)    :: MD       !! 'SHA2S' object
    tIndex                      :: Length   !! the digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsSHA224) THEN
        Length = DLen224
    ELSE
        Length = DLen256
    END IF

    RETURN

END FUNCTION SHA2S_GetDigestLen

!******************************************************************************

FUNCTION SHA2S_GetBlockLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the block length for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA2S), INTENT(IN)    :: MD       !! 'SHA2S' object
    tIndex                      :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = BlockLen
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION SHA2S_GetBlockLen

!******************************************************************************

SUBROUTINE SHA2S_SetBufPtr(MD, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA2S), TARGET, INTENT(INOUT) :: MD           !! 'SHA2S' object
    tByte,       POINTER, INTENT(INOUT) :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => MD%BufArr

    RETURN

END SUBROUTINE SHA2S_SetBufPtr

!******************************************************************************

SUBROUTINE SHA2S_ProcessBlock(MD, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA2S), INTENT(INOUT) :: MD           !! 'SHA2S' object
    tByte,        INTENT(IN)    :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: A, B, C, D, E, F, G, H
    tInteger    :: T1, T2
    tInteger    :: W(0:63)
    tIndex      :: I, J

!** SUBROUTINE MACRO DEFINITIONS:
#define BSigma0(X)      IEOR(IEOR(RotateLeft(X, 30), RotateLeft(X, 19)), RotateLeft(X, 10))
#define BSigma1(X)      IEOR(IEOR(RotateLeft(X, 26), RotateLeft(X, 21)), RotateLeft(X, 7))
#define SSigma0(X)      IEOR(IEOR(RotateLeft(X, 25), RotateLeft(X, 14)), SHIFTR(X, 3))
#define SSigma1(X)      IEOR(IEOR(RotateLeft(X, 15), RotateLeft(X, 13)), SHIFTR(X, 10))
#define CH(X, Y, Z)     IEOR(IAND(IEOR(Y, Z), X), Z)        
#define MAJ(X, Y, Z)    IOR(IAND(Y, Z), IAND(IOR(Y, Z), X))

! FLOW

    A = MD%State(0)
    B = MD%State(1)
    C = MD%State(2)
    D = MD%State(3)
    E = MD%State(4)
    F = MD%State(5)
    G = MD%State(6)
    H = MD%State(7)

    CALL BytePackBE(BytesIn, 0_kIndex, W(0:15))
    DO J = 16, 63
        W(J) = SSigma1(W(J-2)) + W(J-7) + SSigma0(W(J-15)) + W(J-16)
    END DO
    DO I = 0, 63
        T1 = H + BSigma1(E) + CH(E, F, G) + K(I) + W(I)
        T2 = BSigma0(A) + MAJ(A, B, C)
        H = G
        G = F
        F = E
        E = D + T1
        D = C
        C = B
        B = A
        A = T1 + T2
    END DO

    MD%State(0) = MD%State(0) + A
    MD%State(1) = MD%State(1) + B
    MD%State(2) = MD%State(2) + C
    MD%State(3) = MD%State(3) + D
    MD%State(4) = MD%State(4) + E
    MD%State(5) = MD%State(5) + F
    MD%State(6) = MD%State(6) + G
    MD%State(7) = MD%State(7) + H

    RETURN

#undef BSigma0
#undef BSigma1
#undef SSigma0
#undef SSigma1
#undef CH
#undef MAJ

END SUBROUTINE SHA2S_ProcessBlock

!******************************************************************************

SUBROUTINE SHA2S_DoPadding(MD, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the final padding and store the result in the
    !  provided buffer.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA2S), INTENT(INOUT) :: MD           !! 'SHA2S' object
    tByte,        INTENT(INOUT) :: BytesOut(0:) !! the output buffer
    tIndex,       INTENT(IN)    :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW
        
    CALL MD%HelperPadding()
    DO I = 0_kIndex, (MD%GetDigestLen()/4_kIndex - 1_kIndex)
        CALL ByteUnpackBE(MD%State(I), BytesOut, Offset + 4_kIndex*I)
    END DO

    RETURN

END SUBROUTINE SHA2S_DoPadding

!******************************************************************************

SUBROUTINE SHA2S_AddBitsNPad(MD, LastByte, NBits, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then perform the final padding and
    !  store the result in the provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA2S), INTENT(INOUT) :: MD           !! 'SHA2S' object
    tByte,        INTENT(IN)    :: LastByte     !! the last byte
    tByte,        INTENT(IN)    :: NBits        !! number of bits in the last byte
    tByte,        INTENT(INOUT) :: BytesOut(0:) !! the output buffer
    tIndex,       INTENT(IN)    :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! SHA-2 algorithms do not handle partial bytes
    CALL MD%DoPadding(BytesOut, Offset)
    ASSOCIATE(Dummy => LastByte, Dummy2 => NBits); END ASSOCIATE

    RETURN

END SUBROUTINE SHA2S_AddBitsNPad

!******************************************************************************

END MODULE Class_SHA2S
    
!******************************************************************************
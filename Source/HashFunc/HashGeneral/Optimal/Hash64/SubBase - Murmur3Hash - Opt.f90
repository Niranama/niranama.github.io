
SUBMODULE (ModBase_OptimalHash64) SubBase_Murmur3Hash_Opt

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an optimal implementation for the Murmur3 hash algorithm
!   for 64-bit-integer (and optionally 128-bit) output by Austin Appleby. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define MaskI8(X)       IAND(ToLong(X), ToLong(Z'00000000000000FF'))
#define K1_Mixing(K) \
    K = K*C1; \
    K = RotateLeft(K, 31); \
    K = K*C2;
#define K2_Mixing(K) \
    K = K*C2; \
    K = RotateLeft(K, 33); \
    K = K*C1;
#define FinalMixing(H) \
    H = IEOR(H, SHIFTR(H, 33)); \
    H = H*ToLong(Z'FF51AFD7ED558CCD'); \
    H = IEOR(H, SHIFTR(H, 33)); \
    H = H*ToLong(Z'C4CEB9FE1A85EC53'); \
    H = IEOR(H, SHIFTR(H, 33));

!** MODULE PARAMETERS:
    tLong, PARAMETER    :: C1 = ToLong(Z'87C37B91114253D5')
    tLong, PARAMETER    :: C2 = ToLong(Z'4CF5AD432745937F')

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE FUNCTION Murmur3_Hash128_Opt(Input, InpSize, StartHash, RemoveSign, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the Murmur3 hash algorithm by Austin Appleby.
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
    tLong,    OPTIONAL,     INTENT(OUT) :: HashPair(2)  !! dual (128-bit) hash codes
    tLong                               :: HashCode     !! single (64-bit) hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER      :: InpPtr(:)
    tLong               :: Seed

!** FLOW

    ! initialize
    SET_OPTION(Seed, 0_kInteger, StartHash)
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)

    ! perform hashing
    HashCode = Murmur3_Hash128(InpPtr, Seed, HashPair)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION Murmur3_Hash128_Opt

!******************************************************************************

FUNCTION Murmur3_Hash128(Input, Seed, HashPair) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the Murmur3 hash algorithm by Austin Appleby.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, TARGET,   INTENT(IN)     :: Input(0:)    !! input bytes
    tLong,           INTENT(IN)     :: Seed         !! seed
    tLong, OPTIONAL, INTENT(OUT)    :: HashPair(2)  !! dual (128-bit) hash codes
    tLong                           :: HashCode     !! single (64-bit) hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong, POINTER  :: LongVal(:) => NULL()     ! Fortran pointer to the input
    TYPE(C_PTR)     :: CPtr                     ! C pointer to the input
    tLong           :: H1, H2, K1, K2
    tIndex          :: Length, Remaining, Offset, Index

!** FLOW
    
    ! initialize
    H1 = Seed
    H2 = Seed
    Length = SIZE(Input)
    Remaining = Length
    Index = 1
    CPtr = C_LOC(Input(0))
    CALL C_F_POINTER(CPtr, LongVal, SHAPE=[Length/8])
        
    ! perform hashing by mixing 16 bytes at a time into the hash
    DO WHILE (Remaining >= 16)
            
        K1 = LongVal(Index)
        K2 = LongVal(Index+1)
        Index = Index + 2
        Remaining = Remaining - 16
            
        K1_Mixing(K1)
        H1 = IEOR(H1, K1)
        H1 = RotateLeft(H1, 27)
        H1 = H1 + H2
        H1 = H1*5_kLong + ToLong(Z'0000000052DCE729')
            
        K2_Mixing(K2)
        H2 = IEOR(H2, K2)
        H2 = RotateLeft(H2, 31)
        H2 = H2 + H1
        H2 = H2*5_kLong + ToLong(Z'0000000038495AB5')
            
    END DO
        
    ! compute offset
    Offset = (Index-1)*8
        
    IF (Remaining > 0) THEN
        K1 = 0_kLong
        K2 = 0_kLong
        SELECT CASE (Remaining)
        CASE (8:15)
            K1 = LongVal(Index)
            K2 = PackPartial(Input, Offset+8, Remaining-8)
        CASE (1:7)
            K1 = PackPartial(Input, Offset, Remaining)
        END SELECT
        K1_Mixing(K1)
        H1 = IEOR(H1, K1)
        K2_Mixing(K2)
        H2 = IEOR(H2, K2)
    END IF
        
    ! Do a few final mixes of the hash to ensure the last few bytes are well-incorporated
    H1 = IEOR(H1, ToLong(Length))
    H2 = IEOR(H2, ToLong(Length))
    H1 = H1 + H2
    H2 = H2 + H1
    FinalMixing(H1)
    FinalMixing(H2)
        
    IF (PRESENT(HashPair)) THEN
        H1 = H1 + H2
        HashPair(1) = H1
        HashPair(2) = H1 + H2
        HashCode = H1
    ELSE
        HashCode = H1 + H2
    END IF
        
    NULLIFY(LongVal)
    cPtr = C_NULL_PTR

    RETURN

END FUNCTION Murmur3_Hash128

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                             +
!                   OPTIMAL VERSIONS OF AUXILIARY ROUTINES                    +
!                                                                             +
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION PackPartial(Buf, Off, Length) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To pack seven or fewer bytes of the array 'Buf' at offset 'Off'
    ! into the 64-bit word 'Res', in little-endian convention
    ! (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,  INTENT(IN)  :: Buf(0:)  ! buffer
    tIndex, INTENT(IN)  :: Off      ! offset
    tIndex, INTENT(IN)  :: Length   ! the number of bytes to pack (between 1 to 7)
    tLong               :: Res      ! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte           :: Wrk(0:7)
! FLOW
        
    ! initialize
    Wrk = 0_kByte
        
    ! gather available bytes
    Wrk(0:Length-1) = Buf(Off:Off+Length-1)

    ! pack bytes into word
#define MaskLong(X)     IAND(ToLong(X), Z'00000000000000FF')
    Res =        MaskLong(Wrk(0))      + SHIFTL(MaskLong(Wrk(1)),  8) + &
          SHIFTL(MaskLong(Wrk(2)), 16) + SHIFTL(MaskLong(Wrk(3)), 24) + &
          SHIFTL(MaskLong(Wrk(4)), 32) + SHIFTL(MaskLong(Wrk(5)), 40) + &
          SHIFTL(MaskLong(Wrk(6)), 48) + SHIFTL(MaskLong(Wrk(7)), 56)
#undef MaskLong

    RETURN

END FUNCTION PackPartial

!**************************************************************************

END SUBMODULE SubBase_Murmur3Hash_Opt

!******************************************************************************

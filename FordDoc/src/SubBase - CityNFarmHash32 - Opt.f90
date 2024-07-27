
SUBMODULE (ModBase_OptimalHash32) SubBase_CityNFarmHash32_Opt

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an optimal implementation for the City and Farm hash algorithm
!   for 32-bit-integer output by Google Inc. <br>

!** USE STATEMENTS:
    USE ModBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr
    USE ModBase_SIntUtil,   ONLY: ReverseBytes
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define     Permute3(A, B, C) \
    SWAP_SCALAR(A, B); \
    SWAP_SCALAR(A, C);
#define     MaskInteger(X)      IAND(ToInteger(X), Z'000000FF')

!** MODULE PARAMETERS:
    tInteger, PARAMETER    :: C1 = ToInteger(Z'CC9E2D51')
    tInteger, PARAMETER    :: C2 = ToInteger(Z'1B873593')
    tInteger, PARAMETER    :: C3 = ToInteger(Z'E6546B64')

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE FUNCTION City_Hash32_Opt(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the City hash algorithm by Google Inc.
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

!** FLOW

    ! initialize
    SET_OPTION(Seed, 0_kInteger, StartHash)
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)

    ! perform hashing
    HashCode = City_Hash32(InpPtr, Seed)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION City_Hash32_Opt

!******************************************************************************

MODULE FUNCTION FarmMk_Hash32_Opt(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the FarmMk hash algorithm by Google Inc.
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

!** FLOW

    ! initialize
    SET_OPTION(Seed, 0_kInteger, StartHash)
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)

    ! perform hashing
    HashCode = FarmMk_Hash32(InpPtr, Seed)

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION FarmMk_Hash32_Opt

!******************************************************************************

FUNCTION City_Hash32(Input, Seed) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the City hash algorithm by Google Inc.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)    :: Input(0:)    !! input bytes
    tInteger, INTENT(IN)    :: Seed         !! seed
    tInteger                :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: H, G, F, Temp
    tInteger    :: A0, A1, A2, A3, A4
    tIndex      :: Iters, Length, Offset

!** SUBROUTINE MACRO DEFINITIONS:
#define MaskInteger(X)  IAND(ToInteger(X), Z'000000FF')
#define FMix(H) \
    H = IEOR(H, SHIFTR(H, 16)); \
    H = H*ToInteger(Z'85EBCA6B'); \
    H = IEOR(H, SHIFTR(H, 13)); \
    H = H*ToInteger(Z'C2B2AE35'); \
    H = IEOR(H, SHIFTR(H, 16));
#define Mur(A,H) \
    A = A*C1; \
    A = RotateRight(A, 17); \
    A = A*C2; \
    H = IEOR(H, A); \
    H = RotateRight(H, 19); \
    H = H*5 + C3;
#define Mix(H,A) \
    H = IEOR(H, A); \
    H = RotateRight(H, 19); \
    H = H*5 + C3;
#define Permute3(A,B,C) \
    SWAP_SCALAR(A, B); \
    SWAP_SCALAR(A, C);
#define MaskInteger(X)      IAND(ToInteger(X), Z'000000FF')
#define Pack_I32(Buf, Off)  IOR(IOR(IOR(MaskInteger(Buf(Off)), SHIFTL(MaskInteger(Buf(Off+1)), 8)), \
                            SHIFTL(MaskInteger(Buf(Off+2)), 16)), SHIFTL(MaskInteger(Buf(Off+3)), 24))

!** FLOW

    ! initialize
    Length = SIZE(Input)

    ! perform hashing
    IF (Length <= 24) THEN
        IF (Length <= 4) THEN
            ! Hash32Len0to4
            BLOCK
                tInteger    :: A, B, C
                tIndex      :: I
                B = Seed
                C = 9
                DO I = 0, Length-1
                    B = B*C1 + MaskInteger(Input(I))
                    C = IEOR(C, B)
                END DO
                A = Length
                HashCode = C
                Mur(A, HashCode)
                Mur(B, HashCode)
                FMix(HashCode)
            END BLOCK
        ELSEIF (Length <= 12) THEN
            ! Hash32Len5to12
            BLOCK
                tInteger    :: A, B, C
                A = ToInteger(Length) + Seed
                B = ToInteger(Length)*5
                C = 9
                HashCode = B
                A = A + Pack_I32(Input, 0)
                B = B + Pack_I32(Input, Length - 4)
                C = C + Pack_I32(Input, IAND(SHIFTA(Length, 1), 4))
                Mur(A, HashCode)
                Mur(B, HashCode)
                Mur(C, HashCode)
                FMix(HashCode)
            END BLOCK
        ELSE
            ! Hash32Len13to24
            BLOCK
                tInteger    :: A, B, C, D, E, F
                A = Pack_I32(Input, SHIFTA(Length, 1) - 4)
                B = Pack_I32(Input, 4)
                C = Pack_I32(Input, Length - 8)
                D = Pack_I32(Input, SHIFTA(Length, 1))
                E = Pack_I32(Input, 0)
                F = Pack_I32(Input, Length - 4)
                HashCode = Seed + ToInteger(Length)
                Mur(A, HashCode)
                Mur(B, HashCode)
                Mur(C, HashCode)
                Mur(D, HashCode)
                Mur(E, HashCode)
                Mur(F, HashCode)
                FMix(HashCode)
            END BLOCK
        END IF
        RETURN
    END IF

    ! Length > 24
    H = ToInteger(Length) + Seed
    G = C1*ToInteger(Length)
    F = G
    A0 = RotateRight(Pack_I32(Input, Length - 4)*C1, 17)*C2
    A1 = RotateRight(Pack_I32(Input, Length - 8)*C1, 17)*C2
    A2 = RotateRight(Pack_I32(Input, Length - 16)*C1, 17)*C2
    A3 = RotateRight(Pack_I32(Input, Length - 12)*C1, 17)*C2
    A4 = RotateRight(Pack_I32(Input, Length - 20)*C1, 17)*C2
    Mix(H, A0)
    Mix(H, A2)
    Mix(G, A1)
    Mix(G, A3)
    F = F + A4
    F = RotateRight(F, 19)
    F = F*5 + C3
    Iters = (Length - 1) / 20
    Offset = 0
    DO
        A0 = RotateRight(Pack_I32(Input, Offset)*C1, 17)*C2
        A1 = Pack_I32(Input, Offset + 4)
        A2 = RotateRight(Pack_I32(Input, Offset + 8)*C1, 17)*C2
        A3 = RotateRight(Pack_I32(Input, Offset + 12)*C1, 17)*C2
        A4 = Pack_I32(Input, Offset + 16)
        H = IEOR(H, A0)
        H = RotateRight(H, 18)
        H = H*5 + C3
        F = F + A1
        F = RotateRight(F, 19)
        F = F*C1
        G = G + A2
        G = RotateRight(G, 18)
        G = G*5 + C3
        H = IEOR(H, A3 + A1)
        H = RotateRight(H, 19)
        H = H*5 + C3
        G = IEOR(G, A4)
        G = ReverseBytes(G)*5
        H = H + A4*5
        H = ReverseBytes(H)
        F = F + A0
        Permute3(F, H, G)
        Offset = Offset + 20
        Iters = Iters - 1
        IF (Iters == 0) EXIT
    END DO
    G = RotateRight(G, 11)*C1
    G = RotateRight(G, 17)*C1
    F = RotateRight(F, 11)*C1
    F = RotateRight(F, 17)*C1
    H = RotateRight(H + G, 19)
    H = H*5 + C3
    H = RotateRight(H, 17)*C1
    H = RotateRight(H + F, 19)
    H = H*5 + C3
    HashCode = RotateRight(H, 17)*C1

! undefine macros
#undef FMix
#undef Mur
#undef Mix
#undef Permute3
#undef MaskInteger
#undef Pack_I32

    RETURN

END FUNCTION City_Hash32

!******************************************************************************

FUNCTION FarmMk_Hash32(Input, Seed) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the FarmMk hash algorithm by Google Inc.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)    :: Input(0:)    !! input bytes
    tInteger, INTENT(IN)    :: Seed         !! seed
    tInteger                :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: M, N, X
    tIndex      :: Length

!** SUBROUTINE MACRO DEFINITIONS:
#define FMix(H) \
    H = IEOR(H, SHIFTR(H, 16)); \
    H = H*ToInteger(Z'85EBCA6B'); \
    H = IEOR(H, SHIFTR(H, 13)); \
    H = H*ToInteger(Z'C2B2AE35'); \
    H = IEOR(H, SHIFTR(H, 16));
#define Mur(A,H) \
    X = A; \
    X = X*C1; \
    X = RotateRight(X, 17); \
    X = X*C2; \
    H = IEOR(H, X); \
    H = RotateRight(H, 19); \
    H = H*5 + C3;
#define MurPlus(A,H,B) \
    Mur(A, H); \
    H = H + B;
#define Mix(H,A) \
    H = IEOR(H, A); \
    H = RotateRight(H, 19); \
    H = H*5 + C3;
#define MaskInteger(X)          IAND(ToInteger(X), Z'000000FF')
#define UnsignedShort(Val, Off) IOR(MaskInteger(Val(Off)), SHIFTL(MaskInteger(Val(Off+1)), 8))
#define Pack_I32(Buf, Off)      IOR(UnsignedShort(Buf, Off), SHIFTL(UnsignedShort(Buf, Off+2), 16))

!** FLOW

    ! initialize
    Length = SIZE(Input)
        
    ! perform hashing
    IF (Length <= 4) THEN
        HashCode = HashLen0To4(Input, Seed)
        RETURN
    ELSEIF (Length <= 12) THEN
        HashCode = HashLen5To12(Input, Seed)
        RETURN
    ELSEIF (Length <= 24)  THEN 
        HashCode = HashLen13To24(Input, Seed*C1)
        RETURN
    END IF
        
    HashCode = HashLen13To24(Input(0:23), IEOR(Seed, ToInteger(Length)))
    M = HashWithoutSeed(Input(24:))
    ! Hash32WithSeed
    N = M + Seed
    Mur(N, HashCode)

    RETURN
    
    CONTAINS

    FUNCTION HashLen0To4(Input, Seed) RESULT(HashCode)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,    INTENT(IN)    :: Input(0:)    ! input bytes
        tInteger, INTENT(IN)    :: Seed         ! seed
        tInteger                :: HashCode     ! single hash code

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: A, B, C, X
        tIndex      :: I
        tIndex      :: Length
        
    !** FLOW
        
        Length = SIZE(Input)

        ! Hash32Len0to4
        B = Seed
        C = 9
        DO I = 0, Length-1
            B = B*C1 +  ToInteger(Input(I))
            C = IEOR(C, B)
        END DO
        A = ToInteger(Length)
        HashCode = C
        Mur(A, HashCode)
        Mur(B, HashCode)
        FMix(HashCode)
        
        RETURN

    END FUNCTION HashLen0To4

    !**************************************************************************

    FUNCTION HashLen5To12(Input, Seed) RESULT(HashCode)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,    INTENT(IN)    :: Input(0:)    ! input bytes
        tInteger, INTENT(IN)    :: Seed         ! seed
        tInteger                :: HashCode     ! single hash code

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: A, B, C, X
        tIndex      :: Length
        
    !** FLOW
        
        Length = SIZE(Input)

        ! Hash32Len5to12
        A = ToInteger(Length)
        B = ToInteger(Length)*5
        C = 9
        HashCode = B + Seed
        A = A + Pack_I32(Input, 0)
        B = B + Pack_I32(Input, Length - 4)
        C = C + Pack_I32(Input, IAND(SHIFTA(Length, 1), 4))
        Mur(A, HashCode)
        Mur(B, HashCode)
        Mur(C, HashCode)
        HashCode = IEOR(Seed, HashCode)
        FMix(HashCode)
        
        RETURN

    END FUNCTION HashLen5To12

    !**************************************************************************

    FUNCTION HashLen13To24(Input, Seed) RESULT(HashCode)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,    INTENT(IN)    :: Input(0:)    ! input bytes
        tInteger, INTENT(IN)    :: Seed         ! seed
        tInteger                :: HashCode     ! single hash code

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: A, B, C, D, E, F, X
        tIndex      :: Length
        
    !** FLOW
        
        Length = SIZE(Input)

        ! Hash32Len13to24
        A = Pack_I32(Input, SHIFTA(Length, 1) - 4)
        B = Pack_I32(Input, 4)
        C = Pack_I32(Input, Length - 8)
        D = Pack_I32(Input, SHIFTA(Length, 1))
        E = Pack_I32(Input, 0)
        F = Pack_I32(Input, Length - 4)
        HashCode = D*C1 + ToInteger(Length) + Seed
        A = RotateRight(A, 12) + F
        MurPlus(C, HashCode, A)
        A = RotateRight(A, 3) + C
        MurPlus(E, HashCode, A)
        A = RotateRight(A + F, 12) + D
        B = IEOR(B, Seed)
        MurPlus(B, HashCode, A)
        FMix(HashCode)

        RETURN

    END FUNCTION HashLen13To24

    !**************************************************************************

    FUNCTION HashWithoutSeed(Input) RESULT(HashCode)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte, INTENT(IN)   :: Input(0:)    ! input bytes
        tInteger            :: HashCode     ! single hash code

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: G, F, T, X
        tInteger    :: A0, A1, A2, A3, A4
        tIndex      :: Iters, Length, Offset

    !** FLOW

        ! initialize
        Length = SIZE(Input)
        
        ! perform hashing
        IF (Length <= 4) THEN
            HashCode = HashLen0To4(Input, 0)
            RETURN
        ELSEIF (Length <= 12) THEN
            HashCode = HashLen5To12(Input, 0)
            RETURN
        ELSEIF (Length <= 24)  THEN 
            HashCode = HashLen13To24(Input, 0)
            RETURN
        END IF
        
        ! Length > 24 of Hash32
        HashCode = ToInteger(Length)
        G = C1*ToInteger(Length)
        F = G
        A0 = RotateRight(Pack_I32(Input, Length - 4)*C1, 17)*C2
        A1 = RotateRight(Pack_I32(Input, Length - 8)*C1, 17)*C2
        A2 = RotateRight(Pack_I32(Input, Length - 16)*C1, 17)*C2
        A3 = RotateRight(Pack_I32(Input, Length - 12)*C1, 17)*C2
        A4 = RotateRight(Pack_I32(Input, Length - 20)*C1, 17)*C2
        Mix(HashCode, A0)
        Mix(HashCode, A2)
        Mix(G, A1)
        Mix(G, A3)
        F = F + A4
        F = RotateRight(F, 19) + 113
        Iters = (Length - 1) / 20
        Offset = 0
        DO
            A0 = Pack_I32(Input, Offset)
            A1 = Pack_I32(Input, Offset + 4)
            A2 = Pack_I32(Input, Offset + 8)
            A3 = Pack_I32(Input, Offset + 12)
            A4 = Pack_I32(Input, Offset + 16)
            HashCode = HashCode + A0
            G = G + A1
            F = F + A2
            MurPlus(A3, HashCode, A4)
            MurPlus(A2, G, A0)
            T = A1 + A4*C1
            MurPlus(T, F, A3)
            F = F + G
            G = G + F
            Offset = Offset + 20
            Iters = Iters - 1
            IF (Iters == 0) EXIT
        END DO
        G = RotateRight(G, 11)*C1
        G = RotateRight(G, 17)*C1
        F = RotateRight(F, 11)*C1
        F = RotateRight(F, 17)*C1
        HashCode = RotateRight(HashCode + G, 19)
        HashCode = HashCode*5 + C3
        HashCode = RotateRight(HashCode, 17)*C1
        HashCode = RotateRight(HashCode + F, 19)
        HashCode = HashCode*5 + C3
        HashCode = RotateRight(HashCode, 17)*C1

        RETURN
        
    END FUNCTION HashWithoutSeed

    !**************************************************************************

#undef FMix
#undef Mur
#undef MurPlus
#undef Mix
#undef MaskInteger
#undef UnsignedShort
#undef Pack_I32

END FUNCTION FarmMk_Hash32

!******************************************************************************

END SUBMODULE SubBase_CityNFarmHash32_Opt

!******************************************************************************

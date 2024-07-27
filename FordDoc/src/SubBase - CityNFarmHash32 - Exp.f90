
SUBMODULE (ModBase_ExperimentalHash32) SubBase_CityNFarmHash32_Exp

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an experimental implementation for the City and Farm hash algorithm
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

MODULE FUNCTION City_Hash32_Exp(Input, InpSize, Algo, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the City hash algorithm by Google Inc.
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
        HashCode = City_Hash32(InpPtr, Seed, Pack_I32_A1)
    CASE (2)
        HashCode = City_Hash32(InpPtr, Seed, Pack_I32_A2)
    CASE (3)
        HashCode = City_Hash32(InpPtr, Seed, Pack_I32_A3)
    CASE (4)
        HashCode = City_Hash32(InpPtr, Seed, Pack_I32_A4)
    CASE (5)
        HashCode = City_Hash32(InpPtr, Seed, Pack_I32_A5)
    CASE (6)
        HashCode = City_Hash32(InpPtr, Seed, Pack_I32_A6)
    CASE (7)
        HashCode = City_Hash32(InpPtr, Seed, Pack_I32_A7)
    CASE (8)
        HashCode = City_Hash32_08(InpPtr, Seed)
    CASE (9)
        HashCode = City_Hash32_09(InpPtr, Seed)
    END SELECT

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION City_Hash32_Exp

!******************************************************************************

MODULE FUNCTION FarmMk_Hash32_Exp(Input, InpSize, Algo, StartHash, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value using the FarmMk hash algorithm by Google Inc.
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
        HashCode = FarmMk_Hash32(InpPtr, Seed, Pack_I32_A1)
    CASE (2)
        HashCode = FarmMk_Hash32(InpPtr, Seed, Pack_I32_A2)
    CASE (3)
        HashCode = FarmMk_Hash32(InpPtr, Seed, Pack_I32_A3)
    CASE (4)
        HashCode = FarmMk_Hash32(InpPtr, Seed, Pack_I32_A4)
    CASE (5)
        HashCode = FarmMk_Hash32(InpPtr, Seed, Pack_I32_A5)
    CASE (6)
        HashCode = FarmMk_Hash32(InpPtr, Seed, Pack_I32_A6)
    CASE (7)
        HashCode = FarmMk_Hash32(InpPtr, Seed, Pack_I32_A7)
    END SELECT

    ! remove sign bit
    IF (PRESENT(RemoveSign)) THEN
        IF (RemoveSign) HashCode = IAND(HashCode, MaxHash)
    END IF

    ! free pointers
    NULLIFY(InpPtr)

    RETURN

END FUNCTION FarmMk_Hash32_Exp

!******************************************************************************

FUNCTION City_Hash32(Input, Seed, PackFull) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the City hash algorithm by Google Inc.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)    :: Input(0:)    !! input bytes
    tInteger, INTENT(IN)    :: Seed         !! seed
    PROCEDURE(Pack_I32)     :: PackFull     !! procedure to convert a byte array to 32-bit integer
    tInteger                :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: H, G, F, Temp
    tInteger    :: A0, A1, A2, A3, A4
    tIndex      :: Iters, Length, Offset

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
                    B = B*C1 +  MaskInteger(Input(I))
                    C = IEOR(C, B)
                END DO
                A = Length
                HashCode = C
                CALL Mur(A, HashCode)
                CALL Mur(B, HashCode)
                CALL FMix(HashCode)
            END BLOCK
        ELSEIF (Length <= 12) THEN
            ! Hash32Len5to12
            BLOCK
                tInteger    :: A, B, C
                A = ToInteger(Length) + Seed
                B = ToInteger(Length)*5
                C = 9
                HashCode = B
                A = A + PackFull(Input, 0)
                B = B + PackFull(Input, Length - 4)
                C = C + PackFull(Input, IAND(SHIFTA(Length, 1), 4))
                CALL Mur(A, HashCode)
                CALL Mur(B, HashCode)
                CALL Mur(C, HashCode)
                CALL FMix(HashCode)
            END BLOCK
        ELSE
            ! Hash32Len13to24
            BLOCK
                tInteger    :: A, B, C, D, E, F
                A = PackFull(Input, SHIFTA(Length, 1) - 4)
                B = PackFull(Input, 4)
                C = PackFull(Input, Length - 8)
                D = PackFull(Input, SHIFTA(Length, 1))
                E = PackFull(Input, 0)
                F = PackFull(Input, Length - 4)
                HashCode = Seed + ToInteger(Length)
                CALL Mur(A, HashCode)
                CALL Mur(B, HashCode)
                CALL Mur(C, HashCode)
                CALL Mur(D, HashCode)
                CALL Mur(E, HashCode)
                CALL Mur(F, HashCode)
                CALL FMix(HashCode)
            END BLOCK
        END IF
        RETURN
    END IF

    ! Length > 24
    H = ToInteger(Length) + Seed
    G = C1*ToInteger(Length)
    F = G
    A0 = RotateRight(PackFull(Input, Length - 4)*C1, 17)*C2
    A1 = RotateRight(PackFull(Input, Length - 8)*C1, 17)*C2
    A2 = RotateRight(PackFull(Input, Length - 16)*C1, 17)*C2
    A3 = RotateRight(PackFull(Input, Length - 12)*C1, 17)*C2
    A4 = RotateRight(PackFull(Input, Length - 20)*C1, 17)*C2
    CALL Mix(H, A0)
    CALL Mix(H, A2)
    CALL Mix(G, A1)
    CALL Mix(G, A3)
    F = F + A4
    F = RotateRight(F, 19)
    F = F*5 + C3
    Iters = (Length - 1) / 20
    Offset = 0
    DO
        A0 = RotateRight(PackFull(Input, Offset)*C1, 17)*C2
        A1 = PackFull(Input, Offset + 4)
        A2 = RotateRight(PackFull(Input, Offset + 8)*C1, 17)*C2
        A3 = RotateRight(PackFull(Input, Offset + 12)*C1, 17)*C2
        A4 = PackFull(Input, Offset + 16)
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

    RETURN

END FUNCTION City_Hash32

!******************************************************************************

FUNCTION FarmMk_Hash32(Input, Seed, PackFull) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute hash code using the FarmMk hash algorithm by Google Inc.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)    :: Input(0:)    !! input bytes
    tInteger, INTENT(IN)    :: Seed         !! seed
    PROCEDURE(Pack_I32)     :: PackFull     !! procedure to convert a byte array to 32-bit integer
    tInteger                :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: M, N
    tIndex      :: Length

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
    CALL Mur(N, HashCode)

    RETURN

CONTAINS

    FUNCTION HashLen0To4(Input, Seed) RESULT(HashCode)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute hash code for length less than or equal to 4.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,                INTENT(IN)    :: Input(0:)    ! input bytes
        tInteger,             INTENT(IN)    :: Seed         ! seed
        tInteger                            :: HashCode     ! single hash code

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
        CALL Mur(A, HashCode)
        CALL Mur(B, HashCode)
        CALL FMix(HashCode)

        RETURN

    END FUNCTION HashLen0To4

    !**************************************************************************

    FUNCTION HashLen5To12(Input, Seed) RESULT(HashCode)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute hash code for length between 5 and 12.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,                INTENT(IN)    :: Input(0:)    ! input bytes
        tInteger,             INTENT(IN)    :: Seed         ! seed
        tInteger                            :: HashCode     ! single hash code

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
        A = A + PackFull(Input, 0)
        B = B + PackFull(Input, Length - 4)
        C = C + PackFull(Input, IAND(SHIFTA(Length, 1), 4))
        CALL Mur(A, HashCode)
        CALL Mur(B, HashCode)
        CALL Mur(C, HashCode)
        HashCode = IEOR(Seed, HashCode)
        CALL FMix(HashCode)

        RETURN

    END FUNCTION HashLen5To12

    !**************************************************************************

    FUNCTION HashLen13To24(Input, Seed) RESULT(HashCode)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute hash code for length between 13 and 24.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,                INTENT(IN)    :: Input(0:)    ! input bytes
        tInteger,             INTENT(IN)    :: Seed         ! seed
        tInteger                            :: HashCode     ! single hash code

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: A, B, C, D, E, F, X
        tIndex      :: Length

    !** FLOW

        Length = SIZE(Input)

        ! Hash32Len13to24
        A = PackFull(Input, SHIFTA(Length, 1) - 4)
        B = PackFull(Input, 4)
        C = PackFull(Input, Length - 8)
        D = PackFull(Input, SHIFTA(Length, 1))
        E = PackFull(Input, 0)
        F = PackFull(Input, Length - 4)
        HashCode = D*C1 + ToInteger(Length) + Seed
        A = RotateRight(A, 12) + F
        CALL MurPlus(C, HashCode, A)
        A = RotateRight(A, 3) + C
        CALL MurPlus(E, HashCode, A)
        A = RotateRight(A + F, 12) + D
        B = IEOR(B, Seed)
        CALL MurPlus(B, HashCode, A)
        CALL FMix(HashCode)

        RETURN

    END FUNCTION HashLen13To24

    !**************************************************************************

    FUNCTION HashWithoutSeed(Input) RESULT(HashCode)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute hash code without seed.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,                INTENT(IN)    :: Input(0:)    ! input bytes
        tInteger                            :: HashCode     ! single hash code

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
        A0 = RotateRight(PackFull(Input, Length - 4)*C1, 17)*C2
        A1 = RotateRight(PackFull(Input, Length - 8)*C1, 17)*C2
        A2 = RotateRight(PackFull(Input, Length - 16)*C1, 17)*C2
        A3 = RotateRight(PackFull(Input, Length - 12)*C1, 17)*C2
        A4 = RotateRight(PackFull(Input, Length - 20)*C1, 17)*C2
        CALL Mix(HashCode, A0)
        CALL Mix(HashCode, A2)
        CALL Mix(G, A1)
        CALL Mix(G, A3)
        F = F + A4
        F = RotateRight(F, 19) + 113
        Iters = (Length - 1) / 20
        Offset = 0
        DO
            A0 = PackFull(Input, Offset)
            A1 = PackFull(Input, Offset + 4)
            A2 = PackFull(Input, Offset + 8)
            A3 = PackFull(Input, Offset + 12)
            A4 = PackFull(Input, Offset + 16)
            HashCode = HashCode + A0
            G = G + A1
            F = F + A2
            CALL MurPlus(A3, HashCode, A4)
            CALL MurPlus(A2, G, A0)
            T = A1 + A4*C1
            CALL MurPlus(T, F, A3)
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

END FUNCTION FarmMk_Hash32

!******************************************************************************

SUBROUTINE FMix(H)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform mixing.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(INOUT) :: H

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    H = IEOR(H, SHIFTR(H, 16))
    H = H*ToInteger(Z'85EBCA6B')
    H = IEOR(H, SHIFTR(H, 13))
    H = H*ToInteger(Z'C2B2AE35')
    H = IEOR(H, SHIFTR(H, 16))

    RETURN

END SUBROUTINE FMix

!******************************************************************************

SUBROUTINE Mix(H, A)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform mixing.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(INOUT) :: H
    tInteger, INTENT(IN)    :: A

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    H = IEOR(H, A)
    H = RotateRight(H, 19)
    H = H*5 + C3

    RETURN

END SUBROUTINE Mix

!******************************************************************************

SUBROUTINE Mur(A, H)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication and mixing.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: A
    tInteger, INTENT(INOUT) :: H

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: X

!** FLOW
    
    X = A
    X = X*C1
    X = RotateRight(X, 17)
    X = X*C2
    H = IEOR(H, X)
    H = RotateRight(H, 19)
    H = H*5 + C3

    RETURN

END SUBROUTINE Mur

!******************************************************************************

SUBROUTINE MurPlus(A, H, B)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication and mixing.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: A
    tInteger, INTENT(INOUT) :: H
    tInteger, INTENT(IN)    :: B

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    CALL Mur(A, H)
    H = H + B

    RETURN

END SUBROUTINE MurPlus

!******************************************************************************

FUNCTION City_Hash32_08(Input, Seed) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the City hash algorithm by Google Inc.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, TARGET, INTENT(IN)   :: Input(0:)    ! input bytes
    tInteger,      INTENT(IN)   :: Seed         ! seed
    tInteger                    :: HashCode     ! single hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger, POINTER   :: IntVal(:)
    tInteger, POINTER   :: I32Val
    TYPE(C_PTR)         :: CPtr         ! C pointer to the input
    tInteger            :: H, G, F, Temp
    tInteger            :: A0, A1, A2, A3, A4
    tIndex              :: Iters, Length, Offset

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
                    B = B*C1 +  MaskInteger(Input(I))
                    C = IEOR(C, B)
                END DO
                A = Length
                HashCode = C
                CALL Mur(A, HashCode)
                CALL Mur(B, HashCode)
                CALL FMix(HashCode)
            END BLOCK
        ELSEIF (Length <= 12) THEN
            ! Hash32Len5to12
            BLOCK
                tInteger    :: A, B, C
                A = ToInteger(Length) + Seed
                B = ToInteger(Length)*5
                C = 9
                HashCode = B
                CPtr = C_LOC(Input(0))
                CALL C_F_POINTER(CPtr, I32Val)
                A = A + I32Val
                CPtr = C_LOC(Input(Length - 4))
                CALL C_F_POINTER(CPtr, I32Val)
                B = B + I32Val
                CPtr = C_LOC(Input(IAND(SHIFTA(Length, 1), 4)))
                CALL C_F_POINTER(CPtr, I32Val)
                C = C + I32Val
                CALL Mur(A, HashCode)
                CALL Mur(B, HashCode)
                CALL Mur(C, HashCode)
                CALL FMix(HashCode)
            END BLOCK
        ELSE
            ! Hash32Len13to24
            BLOCK
                tInteger    :: A, B, C, D, E, F
                CPtr = C_LOC(Input(SHIFTA(Length, 1) - 4))
                CALL C_F_POINTER(CPtr, IntVal, SHAPE=[2])
                A = IntVal(1)
                D = IntVal(2)
                CPtr = C_LOC(Input(0))
                CALL C_F_POINTER(CPtr, IntVal, SHAPE=[2])
                B = IntVal(2)
                E = IntVal(1)
                CPtr = C_LOC(Input(Length - 8))
                CALL C_F_POINTER(CPtr, IntVal, SHAPE=[2])
                C = IntVal(1)
                F = IntVal(2)
                HashCode = Seed + ToInteger(Length)
                CALL Mur(A, HashCode)
                CALL Mur(B, HashCode)
                CALL Mur(C, HashCode)
                CALL Mur(D, HashCode)
                CALL Mur(E, HashCode)
                CALL Mur(F, HashCode)
                CALL FMix(HashCode)
            END BLOCK
        END IF
        RETURN
    END IF

    ! Length > 24
    H = ToInteger(Length) + Seed
    G = C1*ToInteger(Length)
    F = G
    CPtr = C_LOC(Input(Length - 20))
    CALL C_F_POINTER(CPtr, IntVal, SHAPE=[5])
    A0 = RotateRight(IntVal(5)*C1, 17)*C2
    A1 = RotateRight(IntVal(4)*C1, 17)*C2
    A2 = RotateRight(IntVal(2)*C1, 17)*C2
    A3 = RotateRight(IntVal(3)*C1, 17)*C2
    A4 = RotateRight(IntVal(1)*C1, 17)*C2
    CALL Mix(H, A0)
    CALL Mix(H, A2)
    CALL Mix(G, A1)
    CALL Mix(G, A3)
    F = F + A4
    F = RotateRight(F, 19)
    F = F*5 + C3
    Iters = (Length - 1) / 20
    Offset = 0
    DO
        CPtr = C_LOC(Input(Offset))
        CALL C_F_POINTER(CPtr, IntVal, SHAPE=[5])
        A0 = RotateRight(IntVal(1)*C1, 17)*C2
        A1 = IntVal(2)
        A2 = RotateRight(IntVal(3)*C1, 17)*C2
        A3 = RotateRight(IntVal(4)*C1, 17)*C2
        A4 = IntVal(5)
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

    RETURN

END FUNCTION City_Hash32_08

!******************************************************************************

FUNCTION City_Hash32_09(Input, Seed) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code using the City hash algorithm by Google Inc.

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,                INTENT(IN)    :: Input(0:)    ! input bytes
    tInteger,             INTENT(IN)    :: Seed         ! seed
    tInteger                            :: HashCode     ! single hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger        :: H, G, F, Temp
    tInteger        :: A0, A1, A2, A3, A4
    tIndex          :: Iters, Length, Offset
    tByte           :: ByteVal(0:19)
    tInteger        :: IntVal(1:5)
    tInteger        :: I32Val
    EQUIVALENCE(ByteVal, IntVal)
    EQUIVALENCE(I32Val,  IntVal)

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
                    B = B*C1 +  MaskInteger(Input(I))
                    C = IEOR(C, B)
                END DO
                A = Length
                HashCode = C
                CALL Mur(A, HashCode)
                CALL Mur(B, HashCode)
                CALL FMix(HashCode)
            END BLOCK
        ELSEIF (Length <= 12) THEN
            ! Hash32Len5to12
            BLOCK
                tInteger    :: A, B, C, ShiftLen
                A = ToInteger(Length) + Seed
                B = ToInteger(Length)*5
                C = 9
                HashCode = B
                ByteVal(0:3) = Input(0:3)
                A = A + I32Val
                ByteVal(0:3) = Input(Length-4:Length-1)
                B = B + I32Val
                ShiftLen = IAND(SHIFTA(Length, 1), 4)
                ByteVal(0:3) = Input(ShiftLen:ShiftLen+3)
                C = C + I32Val
                CALL Mur(A, HashCode)
                CALL Mur(B, HashCode)
                CALL Mur(C, HashCode)
                CALL FMix(HashCode)
            END BLOCK
        ELSE
            ! Hash32Len13to24
            BLOCK
                tInteger    :: A, B, C, D, E, F, ShiftLen
                ShiftLen = SHIFTA(Length, 1) - 4
                ByteVal(0:3) = Input(ShiftLen:ShiftLen+3)
                A = IntVal(1)
                D = IntVal(2)
                ByteVal(0:7) = Input(0:7)
                B = IntVal(2)
                E = IntVal(1)
                ByteVal(0:7) = Input(Length-8:Length-1)
                C = IntVal(1)
                F = IntVal(2)
                HashCode = Seed + ToInteger(Length)
                CALL Mur(A, HashCode)
                CALL Mur(B, HashCode)
                CALL Mur(C, HashCode)
                CALL Mur(D, HashCode)
                CALL Mur(E, HashCode)
                CALL Mur(F, HashCode)
                CALL FMix(HashCode)
            END BLOCK
        END IF
        RETURN
    END IF

    ! Length > 24
    H = ToInteger(Length) + Seed
    G = C1*ToInteger(Length)
    F = G
    ByteVal(0:19) = Input(Length-20:Length-1)
    A0 = RotateRight(IntVal(5)*C1, 17)*C2
    A1 = RotateRight(IntVal(4)*C1, 17)*C2
    A2 = RotateRight(IntVal(2)*C1, 17)*C2
    A3 = RotateRight(IntVal(3)*C1, 17)*C2
    A4 = RotateRight(IntVal(1)*C1, 17)*C2
    CALL Mix(H, A0)
    CALL Mix(H, A2)
    CALL Mix(G, A1)
    CALL Mix(G, A3)
    F = F + A4
    F = RotateRight(F, 19)
    F = F*5 + C3
    Iters = (Length - 1) / 20
    Offset = 0
    DO
        ByteVal(0:19) = Input(Offset:Offset+19)
        A0 = RotateRight(IntVal(1)*C1, 17)*C2
        A1 = IntVal(2)
        A2 = RotateRight(IntVal(3)*C1, 17)*C2
        A3 = RotateRight(IntVal(4)*C1, 17)*C2
        A4 = IntVal(5)
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

    RETURN

END FUNCTION City_Hash32_09

!******************************************************************************

END SUBMODULE SubBase_CityNFarmHash32_Exp

!******************************************************************************
